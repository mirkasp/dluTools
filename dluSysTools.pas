
unit dluSysTools;

{$mode ObjFPC}{$H+}

interface

type
   TByteUnitSystem = (busBinary, busDecimal);

function OpenDocument( const ADocumentName : UnicodeString ): Boolean; overload;
function OpenDocument( const ADocumentName : AnsiString    ): Boolean; overload;

function xFormat( const AFormat: string; const AParams: array of const ): UnicodeString;

function ConvertBytes( const ABytes: Int64;
                       const ASystem: TByteUnitSystem = busBinary ): String;

implementation

uses SysUtils
   , LCLIntf
   ;

const
   UNIT_COUNT = 6;

   UNIT_NAMES: array[TByteUnitSystem, 1..UNIT_COUNT] of String = (
      ('KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB'), // Binary units
      ('kB',  'MB',  'GB',  'TB',  'PB',  'EB')   // Decimal units
   );


// Groups decimal digits using the current locale's thousand separator.
// The input must contain only decimal digits.
function GroupDigits( const ADigits: String ): String;
var
   i, n: Integer;
   LSeparator: Char;
begin
   Result := '';
   n := 0;
   LSeparator := DefaultFormatSettings.ThousandSeparator;

   for i := Length(ADigits) downto 1 do begin
      if (n > 0) and (n mod 3 = 0) and (LSeparator <> #0) then
         Result := LSeparator + Result;

      Result := ADigits[i] + Result;
      Inc(n);
   end;
end;


// Converts a signed byte count to a human-readable representation.
// The exact byte count is always included when a scaled unit is used.
function ConvertBytes( const ABytes: Int64;
                       const ASystem: TByteUnitSystem ): String;
const
   asSign: array[Boolean] of String = ('', '-');
var
   LAbs  : QWord;
   LSign : String;
   LExact: String;
   LBase : QWord;
   LDiv  : QWord;
   LIdx  : Integer;
   LValue: Double;
begin
   // Calculate the absolute value without overflowing for Low(Int64).
   if ABytes < 0
      then LAbs := QWord(-(ABytes + 1)) + 1
      else LAbs := QWord(ABytes);

   LSign  := asSign[ABytes < 0];
   LExact := LSign + GroupDigits(IntToStr(LAbs)) + ' B';

   // Select the base used by the requested unit system.
   case ASystem of
      busBinary : LBase := 1024;
      busDecimal: LBase := 1000;
   end;

   if LAbs < LBase then begin
      Result := LExact;
      Exit;
   end;

   // Select the largest supported unit using integer comparisons.
   LIdx := Low(UNIT_NAMES[ASystem]);
   LDiv := LBase;

   while (LIdx < High(UNIT_NAMES[ASystem])) and
         (LAbs >= LDiv * LBase) do begin
      LDiv := LDiv * LBase;
      Inc(LIdx);
   end;

   LValue := LAbs / LDiv;

   // Promote the value if rounding to two decimal places reaches
   // the next unit boundary.
   if (LIdx < High(UNIT_NAMES[ASystem])) and
      (Round(LValue * 100) >= LBase * 100) then begin
      LDiv := LDiv * LBase;
      Inc(LIdx);
      LValue := LAbs / LDiv;
   end;

   Result := LSign +
             FormatFloat('#,##0.##', LValue) + ' ' +
             UNIT_NAMES[ASystem, LIdx] +
             ' (' + LExact + ')';
end;


// Opens a document or URL using the operating system's default handler.
function OpenDocument( const ADocumentName: UnicodeString ): Boolean;
begin
   Result := OpenURL(UTF8Encode(ADocumentName));
end;


// Opens a document or URL supplied as an ANSI string.
function OpenDocument( const ADocumentName: AnsiString ): Boolean;
begin
   Result := OpenURL(ADocumentName);
end;


// Formats a string using the specified arguments and returns Unicode text.
function xFormat( const AFormat: string;
                  const AParams: array of const ): UnicodeString;
begin
   Result := UnicodeFormat(AFormat, AParams);
end;

end.
