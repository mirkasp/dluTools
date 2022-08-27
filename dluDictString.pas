unit dluDictString;
(*******************************************************************************
  dluDictString - based on "dluHashDictionary" specialized type for string dictionary

  2022-08-21 initial version
********************************************************************************)
{$mode ObjFPC}{$H+}

interface

uses dluHashDictionary;

{ TSpecDict }
type TDictString   = class( specialize THashDictionary<string, string> )
   public
     constructor Create; reintroduce;
end;


implementation

uses SysUtils;

function EqualString(const ALeft, ARight: string): boolean;
begin
   Result := SameStr( ALeft, ARight );
end;

function HashString(const AKey: string): THashIndex;
  var i: integer;
begin
   Result := 0;
   for i:=1 to Length(AKey) do Result := (Result + Ord(AKey[i])) mod $FF;
end;



{ TSpecDict }

constructor TDictString.Create;
begin
   inherited Create( @EqualString, @EqualString, @HashString );
end;


end.

