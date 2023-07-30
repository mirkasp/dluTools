unit dluFileDate;

interface

uses Windows;


type TuFileDate = class
   strict private
     fRangeMinD : TDateTime;
     fRangeMaxD : TDateTime;
     function GetRangeMaxF: TFileTime;
     function GetRangeMinF: TFileTime;
     procedure SetRangeMaxD( const Value: TDateTime );
     procedure SetRangeMaxF( AValue: TFileTime );
     procedure SetRangeMinD( const Value: TDateTime );
     procedure SetRangeMinF( AValue: TFileTime );
   public
     class function Less( const ALeft, ARight: TFileTime ): boolean;
     //
     class function FileTimeToDateTime( const AFileTime: TFileTime; const ConvertToLocalTimeZone: boolean = true ): TDateTime;
     class function DateTimeToFileTime( const ADateTime: TDateTime ): TFileTime;
     //

     //class function IsBetween( const AFileDate, AMin, AMax: TFileTime ): boolean; overload; inline;
     //class function IsBetween( const AFileDate: TFileTime; const AMin, AMax: TDateTime ): boolean; overload; inline;
     //
     constructor Create();
     //
     function InRange( const AFileDate: TFileTime ): boolean;
     //
     procedure SetRange( const AMin, AMax : TFileTime ); overload;
     procedure SetRange( const AMin, AMax : TDateTime ); overload;
     //
     //property RangeMinF : TFileTime read GetRangeMinF    write SetRangeMinF;
     //property RangeMaxF : TFileTime read GetRangeMaxF    write SetRangeMaxF;
     //property RangeMinD : TDateTime read GetRangeMinD write SetRangeMinD;
     //property RangeMaxD : TDateTime read GetRangeMaxD write SetRangeMaxD;
     property RangeMinD : TDateTime read fRangeMinD write fRangeMinD;
     property RangeMaxD : TDateTime read fRangeMaxD write fRangeMaxD;
end;

implementation

uses SysUtils
   , DateUtils
   , Dialogs;

{ TFileDateMask }

class function TuFileDate.DateTimeToFileTime( const ADateTime: TDateTime ): TFileTime;
  var sysTime: TSYSTEMTIME;
      temp   : TFILETIME;
begin
   DateTimeToSystemTime( ADateTime, sysTime );
   SystemTimeToFileTime( @sysTime, @temp );
   LocalFileTimeToFileTime( @temp, @result );
end;



class function TuFileDate.FileTimeToDateTime( const AFileTime: TFileTime; const ConvertToLocalTimeZone: boolean ): TDateTime;
  var localFileTime: TFileTime;
      sysTime      : TSystemTime;
begin
  if ConvertToLocalTimeZone
     then FileTimeToLocalFileTime( AFileTime, localFileTime)
     else localFileTime := AFileTime;
  FileTimeToSystemTime( localFileTime, sysTime );
  Result := SystemTimeToDateTime( sysTime );
end;






constructor TuFileDate.Create;
begin
   inherited Create;
   fRangeMinD := MinDateTime;
   fRangeMaxD := MaxDateTime;
end;

class function TuFileDate.Less(const ALeft, ARight: TFileTime): boolean;
begin
   Result := UInt64( ALeft ) < UInt64( ARight );
end;

function TuFileDate.InRange( const AFileDate: TFileTime): boolean;
begin
   try
     Result := DateUtils.DateTimeInRange( FileTimeToDateTime( AFileDate ),
                                          fRangeMinD,
                                          fRangeMaxD );
   except
       ShowMessage( 'Error screen 3/3:'+sLineBreak+
                    'AFileDate:'   + DateTimeToStr( FileTimeToDateTime( AFileDate  ) )
                  );
       Result := false;
   end;
end;

function TuFileDate.GetRangeMaxF: TFileTime;
begin
   Result := DateTimeToFileTime( fRangeMaxD );
end;

function TuFileDate.GetRangeMinF: TFileTime;
begin
   Result := DateTimeToFileTime( fRangeMinD );
end;

procedure TuFileDate.SetRange(const AMin, AMax: TFileTime);
begin
   SetRangeMinF( AMin );
   SetRangeMaxF( AMax );
end;

procedure TuFileDate.SetRange(const AMin, AMax: TDateTime);
begin
   fRangeMinD := AMin;
   fRangeMaxD := AMax;
end;

procedure TuFileDate.SetRangeMaxD(const Value: TDateTime);
begin
   fRangeMaxD := Value;
end;

procedure TuFileDate.SetRangeMaxF(AValue: TFileTime);
begin
   fRangeMaxD := FileTimeToDateTime( AValue );
end;

procedure TuFileDate.SetRangeMinD(const Value: TDateTime);
begin
   fRangeMinD := Value;
end;

procedure TuFileDate.SetRangeMinF(AValue: TFileTime);
begin
   fRangeMinD := FileTimeToDateTime( AValue );
end;

end.
