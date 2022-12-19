unit dluDictionary;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   //{$modeswitch UNICODESTRINGS+}
   {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}
interface

uses Classes;

 { TuxDict }
type

  { IuDictionary }

  IuDictionary = interface
    procedure Add( const AKey: Cardinal; const AValue: AnsiString ); overload;
    procedure Add( const AKey: Cardinal; const AValue: UnicodeString ); overload;
    procedure Add( const AKey: Cardinal; const AValue: string; Params: array of const ); overload;
    //
    procedure Add( const AKeyStr: string; const AValue: AnsiString ); overload;
    //
    procedure AddRange( const AKeyFrom, AKeyTo: Cardinal; const AValue: string );
    function TryLocate( const AKey: Cardinal; out AValue: string ): boolean;
    function Value( const AKey: Cardinal ) : string; overload;
    function QuotedValue( const AKey: Cardinal ) : string;
    function GetValuesList( const ABitField: UInt64; const AMaxBit: Word = 0 ): TStrings;
    function GetMasksList( const AMask: Cardinal ): TStrings;
 end;

{ TuxDictionary }

 TuxDictionary = class( TInterfacedObject, IuDictionary )
   private
     fKeys         : TList;
     fValues       : TStrings;
     fErrorCaption : string;
     fRanges       : TStrings;
   public
     constructor Create( const xErrorCaption: string = '' );
     destructor Destroy; override;
     //
     function Value( const AKey: Cardinal ) : string;
     function QuotedValue( const AKey: Cardinal ) : string;
     //
     function GetValuesList( const ABitField: UInt64; const AMaxBit: Word = 0 ): TStrings;
     function GetMasksList( const AMask: Cardinal ): TStrings;
     //
     procedure Add( const AKey: Cardinal; const AValue: AnsiString ); overload;
     procedure Add( const AKey: Cardinal; const AValue: UnicodeString ); overload;
     procedure Add( const AKey: Cardinal; const AValue: string; Params: array of const ); overload;
     //
     procedure Add( const AKeyStr: string; const AValue: AnsiString ); overload;
     //
     procedure AddRange( const AKeyFrom, AKeyTo: Cardinal; const AValue: string );
     function TryLocate( const AKey: Cardinal; out AValue: string ): boolean;
end;

function StrToCardinal( const AStr: AnsiString ): Cardinal;


implementation

uses SysUtils;

type TDictRange = record
   KeyFrom : Cardinal;
   KeyTo   : Cardinal;
end;

PDictRange = ^TDictRange;


function StrToCardinal( const AStr: AnsiString ): Cardinal;
  var s : packed array[0..3] of AnsiChar;
begin
  s := Copy( AStr + #0#0#0#0, 1, 4 );
  Result := BEToN( Cardinal( s ) );
end;

function IsBit( const AValue: UInt64; const ABit: integer ): boolean;
begin
   Result := ((AValue shr ABit) and $1) = $1;
end;

{ TuxDictionary }

constructor TuxDictionary.Create( const xErrorCaption: string) ;
begin
   inherited Create;
   fErrorCaption := xErrorCaption;
   if fErrorCaption = '' then fErrorCaption := 'Unknown value for key "%x"';
   fKeys   := TList.Create;
   fValues := TStringList.Create;
   fRanges := TStringList.Create;
end;

destructor TuxDictionary.Destroy;
  var i: integer;
begin
   with fRanges do begin
      for i:=0 to fRanges.Count-1 do Dispose( PDictRange( Objects[i] ) );
      Free;
   end;
   fValues.Free;
   fKeys.Free;
   inherited Destroy;
end;

function TuxDictionary.GetValuesList(const ABitField: UInt64; const AMaxBit: Word ): TStrings;
  var i, n : Word;
begin
   if AMaxBit <= 0
      then n := 8 * SizeOf( ABitField )
      else n := AMaxBit;

   Result := TStringList.Create;
   for i := 0 to Pred( n ) do
      if IsBit( ABitField, i ) then begin
         {$IFDEF FPC}
         Result.Add( AnsiString(Value( i )) );
         {$ELSE}
         Result.Add( Value( i ) );
         {$ENDIF}
      end;

end;

function TuxDictionary.GetMasksList(const AMask: Cardinal): TStrings;
  var i : integer;
      k : Cardinal;
begin
   Result := TStringList.Create;
   for i:=0 to fKeys.Count-1 do begin
      k := Cardinal( fKeys[i] );
      if ((k and AMask) = k) then begin
         {$IFDEF FPC}
         Result.Add( AnsiString( Value( k )) );
         {$ELSE}
         Result.Add( Value( k ) );
         {$ENDIF}
      end;
   end;
end;

procedure TuxDictionary.Add( const AKey: Cardinal; const AValue: AnsiString );
begin
   fKeys.Add( Pointer( AKey ) );
   fValues.Add( AValue );
end;

procedure TuxDictionary.Add( const AKey: Cardinal; const AValue: UnicodeString );
begin
   {$IFDEF FPC}
   self.Add( AKey, AnsiString( UTF8Encode( AValue ) ) );
   {$ELSE}
   fKeys.Add( Pointer( AKey ) );
   fValues.Add( AValue );
   {$ENDIF}
end;
procedure TuxDictionary.Add( const AKey: Cardinal; const AValue: string; Params: array of const) ;
begin
   Add( AKey, Format( AValue, Params ) );
end;

procedure TuxDictionary.Add(const AKeyStr: string; const AValue: AnsiString);
begin
   fKeys.Add( Pointer( StrToCardinal( AKeyStr ) ) );
   fValues.Add( AValue );
end;

procedure TuxDictionary.AddRange( const AKeyFrom, AKeyTo: Cardinal; const AValue: string);
  var pd : PDictRange;
begin
   New( pd );
   pd^.KeyFrom := AKeyFrom;
   pd^.KeyTo   := AKeyTo;

   {$IFDEF FPC}
   fRanges.AddObject( UTF8Encode( AValue ), TObject( pd ) );
   {$ELSE}
   fRanges.AddObject( AValue, TObject( pd ) );
   {$ENDIF}

end;

function TuxDictionary.TryLocate( const AKey: Cardinal; out AValue: string ) : boolean;
  var n : integer;
begin
  n := fKeys.IndexOf( Pointer( AKey ) );
  Result := (n >= 0);
  if Result then AValue := fValues[ n ]
  else begin
     n := fRanges.Count-1;
     while n >= 0 do
        with PDictRange( fRanges.Objects[n] )^ do
           if (AKey >= KeyFrom) and (AKey <= KeyTo) then break
           else Dec(n);
     Result := (n >= 0);
     if Result then AValue := fRanges[ n ];
  end;
end;

function TuxDictionary.Value( const AKey: Cardinal) : string;
begin
   if not TryLocate( AKey, Result ) then
     if (Pos( '%x', fErrorCaption ) > 0) or (Pos( '%d', fErrorCaption ) > 0)
        then Result := Format( fErrorCaption, [AKey] )
        else Result := fErrorCaption;

end;

function TuxDictionary.QuotedValue( const AKey: Cardinal): string;
begin
   Result := QuotedStr( Value( AKey ) );
end;



end.
