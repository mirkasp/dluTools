unit dluSplitString;

{$I dluOptions.inc}

interface

type TStrDynArray = array of string;

function SplitString( const ASource: string; const ASeparator: char ): TStrDynArray; //overload;

implementation

uses Classes;

function SplitString( const ASource: string; const ASeparator: char ):TStrDynArray;
  var i  : integer;
begin
   with TStringList.Create do
      try
         Delimiter     := ASeparator;
         QuoteChar     := '"';
         DelimitedText := ASource;
         SetLength( Result{%H-}, Count );
         for i := 0 to Count-1 do Result[i] := {%H-}Strings[i];
      finally
         Free;
      end;
end;


end.
