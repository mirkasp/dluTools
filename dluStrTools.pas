unit dluStrTools;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
//  {$modeswitch UNICODESTRINGS+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

function FirstChar( const AText: string; const ADefaultChar : char = #0 ): char; inline;
function LastChar( const AText: string; const ADefaultChar : char = #0 ): char; inline;
function Iff( const ABoolValue: boolean; const AYesStr, ANoStr: string ): string; inline;

implementation


function FirstChar( const AText: string; const ADefaultChar: char ): char;
begin
   if Length( AText ) = 0
      then Result := ADefaultChar
      else Result := AText[1];
end;

function LastChar( const AText: string; const AdefaultChar : char ): char;
  var n : integer;
begin
   n := Length( AText );
   if n = 0
      then Result := ADefaultChar
      else Result := AText[ n ];
end;

function Iff( const ABoolValue: boolean; const AYesStr, ANoStr: string ): string; inline;
begin
   if ABoolValue then Result := AYesStr else Result := ANoStr;
end;

end.