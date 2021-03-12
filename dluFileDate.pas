unit dluFileDate;

interface

uses Windows;


type TuFileDate = class
   strict private
     fRangeMinF : TFileTime;
     fRangeMaxF : TfileTime;
     function GetRangeMaxD: TDateTime;
     function GetRangeMinD: TDateTime;
     procedure SetRangeMaxD(const Value: TDateTime);
     procedure SetRangeMinD(const Value: TDateTime);
   public
     class function Less( const ALeft, ARight: TFileTime ): boolean;
     class function FileTimeToDateTime( const AFileTime: TFileTime ): TDateTime;
     class function DateTimeToFileTime( const ADateTime: TDateTime ): TFileTime;
     class function IsBetween( const AFileDate, AMin, AMax: TFileTime ): boolean; overload; inline;
     class function IsBetween( const AFileDate: TFileTime; const AMin, AMax: TDateTime ): boolean; overload; inline;
     //
     constructor Create();
     //
     function InRange( const AFileDate: TFileTime ): boolean;
     //
     procedure SetRange( const AMin, AMax : TFileTime ); overload;
     procedure SetRange( const AMin, AMax : TDateTime ); overload;
     //
     property RangeMinF : TFileTime read fRangeMinF    write fRangeMinF;
     property RangeMaxF : TFileTime read fRangeMaxF    write fRangeMaxF;
     property RangeMinD : TDateTime read GetRangeMinD write SetRangeMinD;
     property RangeMaxD : TDateTime read GetRangeMaxD write SetRangeMaxD;
end;

implementation

uses SysUtils, Dialogs;

{ TFileDateMask }

// TFileTime == FILETIME
// Contains a 64-bit value representing the number of 100-nanosecond intervals since January 1, 1601 (UTC).

const OA_ZERO_TICKS = UInt64( 94353120000000000 );      //12/30/1899 12:00am in ticks
const TICKS_PER_DAY = UInt64( 864000000000 );

class function TuFileDate.DateTimeToFileTime( const ADateTime: TDateTime ): TFileTime;
  var li : ULARGE_INTEGER;
begin
    li.QuadPart := Round( ADateTime * TICKS_PER_DAY + OA_ZERO_TICKS );
    Result.dwLowDateTime  := li.LowPart;
    Result.dwHighDateTime := li.HighPart;
    if not LocalFileTimeToFileTime( Result, Result ) then RaiseLastOSError;
end;

class function TuFileDate.FileTimeToDateTime(  const AFileTime: TFileTime ): TDateTime;
  var _time : TFileTime;
      li    : ULARGE_INTEGER;
begin
   if not FileTimeToLocalFileTime( AFileTime, _time ) then RaiseLastOSError;
   li.LowPart  := _time.dwLowDateTime;
   li.HighPart := _time.dwHighDateTime;
   Result      := ( li.QuadPart - OA_ZERO_TICKS ) / TICKS_PER_DAY;
end;

constructor TuFileDate.Create;
begin
   inherited Create;
   fRangeMinF := Default( TFileTime );
   fRangeMaxF.dwLowDateTime  := not fRangeMinF.dwLowDateTime;
   fRangeMaxF.dwHighDateTime := not fRangeMinF.dwHighDateTime;
end;

class function TuFileDate.Less(const ALeft, ARight: TFileTime): boolean;
begin
   Result := UInt64( ALeft ) < UInt64( ARight );
end;

class function TuFileDate.IsBetween(const AFileDate, AMin, AMax: TFileTime): boolean;
begin
//   Result := (UInt64( AFileDate ) >= UInt64( AMin )) and (UInt64( AFileDate ) <= UInt64( AMax ));

   Result := not (Less( AFileDate, AMin ) or Less( AMax, AFileDate ));

//   ShowMessageFmt( '?? is "%s" between ["%s", "%s"] %s',
//                   [ DateTimeToStr( FileTimeToDateTime(AFileDate) ), 
//                     DateTimeToStr( FileTimeToDateTime(AMin) ),
//                     DateTimeToStr( FileTimeToDateTime(AMax) ),
//                     BoolToStr( Result, true ) ] );

end;

class function TuFileDate.IsBetween(const AFileDate: TFileTime; const AMin, AMax: TDateTime): boolean;
begin
   Result := IsBetween( AFileDate, DateTimeToFileTime(AMin), DateTimeToFileTime(AMax) );
end;

function TuFileDate.InRange( const AFileDate: TFileTime): boolean;
begin
   try
     Result := IsBetween( AFileDate, fRangeMinF, fRangeMaxF );
   except
//       ShowMessage( 'Error screen 1/3:'+sLineBreak+
//                    'fRangeMinF: ' + DateTimeToStr( FileTimeToDateTime( fRangeMinF ) )
//                  );
//       ShowMessage( 'Error screen 2/3:'+sLineBreak+
//                    'fRangeMaxF: ' + DateTimeToStr( FileTimeToDateTime( fRangeMaxF ) )
//                  );
             
       ShowMessage( 'Error screen 3/3:'+sLineBreak+
                    'AFileDate:'   + DateTimeToStr( FileTimeToDateTime( AFileDate  ) )
                  );
       Result := false;
   end;
end;

function TuFileDate.GetRangeMaxD: TDateTime;
begin
   Result := FileTimeToDateTime( fRangeMaxF );
end;

function TuFileDate.GetRangeMinD: TDateTime;
begin
   Result := FileTimeToDateTime( fRangeMinF );
end;

procedure TuFileDate.SetRange(const AMin, AMax: TFileTime);
begin
   fRangeMinF := AMin;
   fRangeMaxF := AMax;
end;

procedure TuFileDate.SetRange(const AMin, AMax: TDateTime);
begin
   SetRangeMinD( AMin );
   SetRangeMaxD( AMax );
end;

procedure TuFileDate.SetRangeMaxD(const Value: TDateTime);
begin
   fRangeMaxF := DateTimeToFileTime( Value );
end;

procedure TuFileDate.SetRangeMinD(const Value: TDateTime);
begin
   fRangeMinF := DateTimeToFileTime( Value );
end;

end.
