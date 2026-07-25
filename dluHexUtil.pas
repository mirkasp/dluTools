unit dluHexUtil;

{$mode objfpc}{$H+}

interface

uses SysUtils;

// Zamienia bufor bajtów na tekst HEX. ASeparator wstawiany między bajtami
// (pusty = bez separatora). AMaxBytes > 0 ogranicza liczbę wypisanych bajtów
// i dokleja separator + '...' przy obcięciu; AMaxBytes = 0 = bez ograniczenia.
function BytesToHex( const AData: PByte;  const ACount: integer; const ASeparator: string = ''; AMaxBytes: integer = 0 ): string; overload;
function BytesToHex( const AData: array of Byte; const ASeparator: string = ''; const AMaxBytes: Integer = 0): string; overload;

// Odwrotność BytesToHex: zamienia tekst HEX na bufor bajtów. ASeparator musi
// być identyczny jak użyty przy kodowaniu (pusty = brak separatora).
// UWAGA: nie obsługuje znacznika obcięcia '...' dopisywanego przez BytesToHex
// przy AMaxBytes > 0 - obcięty HEX z definicji nie da się bezstratnie odtworzyć.
//
// TryHexToBytes nie zgłasza wyjątków: zwraca False i ABytes = nil przy
// niepoprawnym formacie AHex (nieparzysta liczba cyfr, znak spoza 0-9A-Fa-f,
// niedopasowany separator).
function TryHexToBytes( const AHex: string; out ABytes: TBytes; const ASeparator: string = '' ): boolean;

// Jak TryHexToBytes, ale przy niepoprawnym formacie AHex zgłasza EConvertError
// zamiast zwracać False.
function HexToBytes( const AHex: string; const ASeparator: string = '' ): TBytes;

implementation

type
   THexPair = array[0..1] of Char;
   PHexPair = ^THexPair;

const
   // Gotowe pary znaków HEX dla każdej wartości bajtu (0..255),
   // indeks tablicy = wartość bajtu.
   HexPairTable : array[Byte] of THexPair = (
   '00', '01', '02', '03', '04', '05', '06', '07',    '08', '09', '0A', '0B', '0C', '0D', '0E', '0F',
   '10', '11', '12', '13', '14', '15', '16', '17',    '18', '19', '1A', '1B', '1C', '1D', '1E', '1F',
   '20', '21', '22', '23', '24', '25', '26', '27',    '28', '29', '2A', '2B', '2C', '2D', '2E', '2F',
   '30', '31', '32', '33', '34', '35', '36', '37',    '38', '39', '3A', '3B', '3C', '3D', '3E', '3F',
   '40', '41', '42', '43', '44', '45', '46', '47',    '48', '49', '4A', '4B', '4C', '4D', '4E', '4F',
   '50', '51', '52', '53', '54', '55', '56', '57',    '58', '59', '5A', '5B', '5C', '5D', '5E', '5F',
   '60', '61', '62', '63', '64', '65', '66', '67',    '68', '69', '6A', '6B', '6C', '6D', '6E', '6F',
   '70', '71', '72', '73', '74', '75', '76', '77',    '78', '79', '7A', '7B', '7C', '7D', '7E', '7F',
   '80', '81', '82', '83', '84', '85', '86', '87',    '88', '89', '8A', '8B', '8C', '8D', '8E', '8F',
   '90', '91', '92', '93', '94', '95', '96', '97',    '98', '99', '9A', '9B', '9C', '9D', '9E', '9F',
   'A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7',    'A8', 'A9', 'AA', 'AB', 'AC', 'AD', 'AE', 'AF',
   'B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7',    'B8', 'B9', 'BA', 'BB', 'BC', 'BD', 'BE', 'BF',
   'C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7',    'C8', 'C9', 'CA', 'CB', 'CC', 'CD', 'CE', 'CF',
   'D0', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7',    'D8', 'D9', 'DA', 'DB', 'DC', 'DD', 'DE', 'DF',
   'E0', 'E1', 'E2', 'E3', 'E4', 'E5', 'E6', 'E7',    'E8', 'E9', 'EA', 'EB', 'EC', 'ED', 'EE', 'EF',
   'F0', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7',    'F8', 'F9', 'FA', 'FB', 'FC', 'FD', 'FE', 'FF'
   );

var
   // Odwrotność HexPairTable: wartość cyfry HEX (0..15) dla kodu znaku ASCII,
   // -1 dla znaków spoza zakresu 0-9A-Fa-f. Budowana raz w initialization -
   // literał 256-elementowy byłby tu mało czytelny (240 identycznych -1
   // przeplatanych 22 użytecznymi wpisami).
   HexNibbleTable : array[Byte] of ShortInt;

function BytesToHex( const AData: PByte; const ACount: integer; const ASeparator: string; AMaxBytes: integer ): string;
   var i, LCount, LSepLen, LTotalLen : integer;
       LHasMore                      : boolean;
       LDst                          : PChar;
begin
   if ( ACount <= 0 ) or ( AData = nil ) then Exit( '' );
   if AMaxBytes < 0 then AMaxBytes := 0;
   LCount   := ACount;
   LHasMore := ( AMaxBytes > 0 ) and ( ACount > AMaxBytes );
   if LHasMore then
      LCount := AMaxBytes;
   LSepLen := Length( ASeparator );
   // 2 znaki HEX na bajt + separator przed każdym oprócz pierwszego,
   // ewentualnie separator + '...' na końcu przy obcięciu.
   LTotalLen := ( LCount shl 1 ) + ( Pred( LCount ) * LSepLen );
   if LHasMore then
      Inc( LTotalLen, LSepLen + 3 );
   SetLength( Result, LTotalLen );
   LDst := PChar( Result );   // wskaźnik na unikalny bufor, ustawiony raz
   for i := 0 to Pred( LCount ) do begin
      if ( i > 0 ) and ( LSepLen > 0 ) then begin
         Move( ASeparator[1], LDst^, LSepLen );
         Inc( LDst, LSepLen );
      end;
      // Jeden odczyt z tablicy + jeden zapis obu znaków naraz
      // zamiast dwóch osobnych shr/and i dwóch zapisów.
      PHexPair( LDst )^ := HexPairTable[ AData[i] ];
      Inc( LDst, 2 );
   end;
   if LHasMore then begin
      if LSepLen > 0 then begin
         Move( ASeparator[1], LDst^, LSepLen );
         Inc( LDst, LSepLen );
      end;
      LDst^ := '.'; Inc( LDst );
      LDst^ := '.'; Inc( LDst );
      LDst^ := '.';
   end;
end;

function BytesToHex(const AData: array of Byte; const ASeparator: string; const AMaxBytes: Integer): string; overload;
begin
  if Length(AData) = 0 then Exit('');
  Result := BytesToHex(@AData[0], Length(AData), ASeparator, AMaxBytes);
end;

function TryHexToBytes( const AHex: string; out ABytes: TBytes; const ASeparator: string ): boolean;
   var i, LHexLen, LSepLen, LByteCount : integer;
       LSrc                            : PChar;
       LHi, LLo                        : ShortInt;
begin
   ABytes  := nil;
   LHexLen := Length( AHex );
   if LHexLen = 0 then Exit( True );   // pusty ciąg = pusty bufor, nie błąd
   LSepLen := Length( ASeparator );
   // n bajtów zapisanych jako: n*2 znaków HEX + (n-1)*LSepLen znaków separatora.
   if LSepLen = 0 then begin
      if Odd( LHexLen ) then Exit( False );
      LByteCount := LHexLen div 2;
   end else begin
      LByteCount := ( LHexLen + LSepLen ) div ( 2 + LSepLen );
      if LByteCount <= 0 then Exit( False );
      if ( LByteCount shl 1 ) + ( Pred( LByteCount ) * LSepLen ) <> LHexLen then Exit( False );
   end;
   SetLength( ABytes, LByteCount );
   LSrc := PChar( AHex );
   for i := 0 to Pred( LByteCount ) do begin
      if ( i > 0 ) and ( LSepLen > 0 ) then begin
         if not CompareMem( LSrc, PChar( ASeparator ), LSepLen ) then begin
            ABytes := nil;
            Exit( False );
         end;
         Inc( LSrc, LSepLen );
      end;
      LHi := HexNibbleTable[ Ord( LSrc[0] ) ];
      LLo := HexNibbleTable[ Ord( LSrc[1] ) ];
      if ( LHi < 0 ) or ( LLo < 0 ) then begin
         ABytes := nil;
         Exit( False );
      end;
      ABytes[i] := Byte( ( LHi shl 4 ) or LLo );
      Inc( LSrc, 2 );
   end;
   Result := True;
end;

function HexToBytes( const AHex: string; const ASeparator: string ): TBytes;
begin
   if not TryHexToBytes( AHex, Result, ASeparator ) then
      raise EConvertError.CreateFmt( 'Niepoprawny format HEX: "%s"', [ AHex ] );
end;

procedure InitHexNibbleTable;
  var c : Char;
begin
   FillChar( HexNibbleTable, SizeOf( HexNibbleTable ), $FF );   // -1 = znak nieprawidłowy
   for c := '0' to '9' do HexNibbleTable[ Ord( c ) ] := Ord( c ) - Ord( '0' );
   for c := 'A' to 'F' do HexNibbleTable[ Ord( c ) ] := Ord( c ) - Ord( 'A' ) + 10;
   for c := 'a' to 'f' do HexNibbleTable[ Ord( c ) ] := Ord( c ) - Ord( 'a' ) + 10;
end;

initialization
   InitHexNibbleTable;

end.



