unit dluSplitString;

{$mode ObjFPC}{$H+}

interface

type TAnsiStringDynArray = array of AnsiString;
type TStrDynArray = TAnsiStringDynArray;

type TUnicodeStringDynArray = array of UnicodeString;


function SplitString( const ASource: AnsiString; const ASeparator: AnsiChar ): TAnsiStringDynArray; overload;
function SplitString( const ASource: UnicodeString; const ASeparator: UnicodeChar ): TUnicodeStringDynArray; overload;

implementation

uses Classes;

function SplitString( const ASource: AnsiString; const ASeparator: AnsiChar ): TAnsiStringDynArray; overload;
  var i  : integer;
begin
   Result := nil;
   with TStringList.Create do begin
      try
         Delimiter     := ASeparator;
         QuoteChar     := '"';
         DelimitedText := ASource;
         SetLength( Result, Count );
         for i := 0 to Count-1 do Result[i] := Strings[i];
      finally
         Free;
      end;
   end;
end;


function SplitString( const ASource: UnicodeString; const ASeparator: UnicodeChar ): TUnicodeStringDynArray; overload;
  var i  : integer;
begin
   Result := nil;
   with TStringList.Create do
      try
         Delimiter     := ASeparator;
         QuoteChar     := '"';
         DelimitedText := AnsiString(ASource);
         SetLength( Result, Count );
         for i := 0 to Count-1 do Result[i] := UnicodeString( Strings[i] );
      finally
         Free;
      end;
end;


end.
