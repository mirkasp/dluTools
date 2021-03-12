unit dluCmdLine;

interface

uses Classes;

type TuCmdLine = class
   strict private
     fSwitches : TStringList;
   public
     constructor Create( const AValidSwitches: array of string );
     destructor Destroy; override;
     function SwitchExist( const s: string ): boolean;
     function SwitchValue( const s: string ): string;
end;

implementation

uses SysUtils, Dialogs;

const cSWITCH_PREFIX_SLASH = '/';
const cSWITCH_PREFIX_PAUSE = '-';
const cSWITCH_SUFFIX_EQUAL = '=';
const cSWITCH_SUFFIX_COLON = ':';

function SearchInArray( const AItem: string; const AItemArray: array of string ): boolean;
  var i: integer;
begin
   i      := High( AItemArray );
   Result := false;
   while not Result and (i>=0) do
       if AItem = AItemArray[i]
          then Result := true
          else Dec(i);
end;

{ TuCmdLine }

constructor TuCmdLine.Create( const AValidSwitches: array of string );
  var i, n: integer;
      tsw, s : string;
begin
   inherited Create();
   fSwitches := TStringList.Create;

   for i := 1 to ParamCount() do begin
      s := ParamStr(i);
      if CharInSet( s[1], [ cSWITCH_PREFIX_SLASH, cSWITCH_PREFIX_PAUSE ] ) then begin
         Delete( s, 1, 1 );
         n := Pos( cSWITCH_SUFFIX_COLON, s );
         if n > 0  then s[n] := cSWITCH_SUFFIX_EQUAL;
         n := Pos( cSWITCH_SUFFIX_EQUAL, s );
         if n > 0
            then tsw := Copy( s, 1, n-1)
            else tsw := s;
         tsw := UpperCase( tsw );
         if SearchInArray( tsw, AValidSwitches ) then
            if n > 0
               then s := tsw + cSWITCH_SUFFIX_EQUAL + Copy( s, n+1, MaxInt )
               else s := tsw + cSWITCH_SUFFIX_EQUAL;

         fSwitches.Add( s )
      end;
   end;

end;

destructor TuCmdLine.Destroy;
begin
   fSwitches.Free;
   inherited;
end;

function TuCmdLine.SwitchExist(const s: string): boolean;
begin
   Result := fSwitches.IndexOfName( UpperCase( s ) ) >= 0;
end;

function TuCmdLine.SwitchValue(const s: string): string;
begin
   Result := fSwitches.Values[ UpperCase( s ) ]
end;

end.
