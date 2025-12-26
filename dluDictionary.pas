unit dluDictionary;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}
interface

uses Classes
   , Generics.Collections
   ;


{ IuxDictionary }
type IuxDictionary = interface
    procedure Add( const AKey: Cardinal; const AValue: String ); overload;
    procedure Add( const AKey: Cardinal; const AValue: String; Params: array of const ); overload;
    //
    procedure Add( const AKeyStr: string; const AValue: String ); overload;
    //
    procedure AddRange( const AKeyFrom, AKeyTo: Cardinal; const AValue: string; const ADispVal: boolean = false );
    function TryLocate( const AKey: Cardinal; out AValue: string ): boolean;
    function Value( const AKey: Cardinal ) : string; overload;
    function QuotedValue( const AKey: Cardinal ) : string;
    function GetValuesList( const ABitField: UInt64; const AMaxBit: Word = 0 ): TStrings;
    function GetMasksList( const AMask: Cardinal ): TStrings;
 end;

{ TuxDictionary }

type TuxDictionary = class( TInterfacedObject, IuxDictionary )
   strict private
     type TDictRange = record
            KeyFrom : Cardinal;
            KeyTo   : Cardinal;
            Value   : string;
            DispVal : boolean;
     end;
     var
        FErrorCaption : string;
        FDict         : specialize TDictionary<Cardinal,String>;
        FRanges       : specialize TList<TDictRange>;
     function TryLocateInRanges(const AKey: Cardinal; out AValue: string): boolean;
     //
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
     procedure Add( const AKey: Cardinal; const AValue: String ); overload;
     procedure Add( const AKey: Cardinal; const AValue: String; Params: array of const ); overload;
     procedure Add( const AKeyStr: string; const AValue: String ); overload;
     //
     procedure AddRange( const AKeyFrom, AKeyTo: Cardinal; const AValue: string; const ADispVal: boolean );
     function TryLocate( const AKey: Cardinal; out AValue: string ): boolean;
end;

function StrToCardinal( const AStr: AnsiString ): Cardinal;


implementation

uses SysUtils;

function StrToCardinal(const AStr: AnsiString): Cardinal;
  var buf: array[0..3] of Byte = (0,0,0,0);
      m: Integer;
begin
   m := Length(AStr);
   if m > 4 then m := 4;
   if m > 0 then
      Move(AStr[1], buf[0], m);
   Result := BEToN(PCardinal(@buf)^);
end;

function IsBit( const AValue: UInt64; const ABit: integer ): boolean; inline;
begin
   Result := ((AValue shr ABit) and $1) = $1;
end;

{ TuxDictionary }

constructor TuxDictionary.Create( const xErrorCaption: string) ;
begin
   inherited Create;
   FErrorCaption := xErrorCaption;
   if FErrorCaption = '' then
      FErrorCaption := 'Unknown value for key "%x"';

   FDict   := specialize TDictionary<Cardinal,String>.Create;
   FRanges := specialize TList<TDictRange>.Create;
end;

destructor TuxDictionary.Destroy;
begin
   FRanges.Free;
   FDict.Free;
   inherited Destroy;
end;

function TuxDictionary.GetValuesList(const ABitField: UInt64; const AMaxBit: Word ): TStrings;
  var i, n : Word;
begin
   if AMaxBit <= 0
      then n := 8 * SizeOf( ABitField )
      else n := AMaxBit;

   Result := TStringList.Create;
   for i := 0 to Pred( n ) do begin
      if ABitField shr i = 0 then Break;  // Wszystkie kolejne bity są 0
      if IsBit( ABitField, i ) then
         Result.Add( Value( i ) );
   end ;

end;

function TuxDictionary.GetMasksList(const AMask: Cardinal): TStrings;
  var pair: specialize TPair<Cardinal, string>;
begin
  Result := TStringList.Create;
  for pair in FDict do
    if (pair.Key and AMask) = pair.Key then
       Result.Add(pair.Value);
end;


procedure TuxDictionary.Add( const AKey: Cardinal; const AValue: String );
begin
   FDict.AddOrSetValue( AKey, AValue );
end;


procedure TuxDictionary.Add( const AKey: Cardinal; const AValue: String; Params: array of const );
begin
   FDict.AddOrSetValue( AKey, Format(AValue, Params) );
end;

procedure TuxDictionary.Add( const AKeyStr: string; const AValue: String );
begin
   FDict.AddOrSetValue( StrToCardinal(AKeyStr), AValue );
end;

procedure TuxDictionary.AddRange( const AKeyFrom, AKeyTo: Cardinal; const AValue: string; const ADispVal: boolean );
  var r : TDictRange;
begin
   if AKeyFrom > AKeyTo then
      raise Exception.Create('Invalid range: KeyFrom > KeyTo');

   r.KeyFrom := AKeyFrom;
   r.KeyTo   := AKeyTo;
   r.Value   := AValue;
   r.DispVal := ADispVal;

   FRanges.Add( r );
end;

function TuxDictionary.TryLocate(const AKey: Cardinal; out AValue: string): boolean;
begin
  if FDict.TryGetValue(AKey, AValue) then
    Exit(True);

  Result := TryLocateInRanges(AKey, AValue);
end;

function TuxDictionary.TryLocateInRanges( const AKey: Cardinal; out AValue: string) : boolean;
  var n : integer;
begin
   n := FRanges.Count-1;
   while n >= 0 do
      if (AKey >= FRanges[n].KeyFrom) and (AKey <= FRanges[n].KeyTo) then
         break
      else
         Dec(n);
   Result := (n >= 0);
   if Result then begin
      AValue := FRanges[ n ].Value;
      if FRanges[n].DispVal then
         AValue := 'Bit ' + IntToStr(AKey) + ': ' + AValue;
   end ;
end ;

function TuxDictionary.Value( const AKey: Cardinal) : string;
begin
   if not TryLocate( AKey, Result ) then
     if (Pos( '%x', FErrorCaption ) > 0) or (Pos( '%d', FErrorCaption ) > 0)
        then Result := Format( FErrorCaption, [AKey] )
        else Result := FErrorCaption;

end;

function TuxDictionary.QuotedValue( const AKey: Cardinal): string;
begin
   Result := QuotedStr( Value( AKey ) );
end;

end.
