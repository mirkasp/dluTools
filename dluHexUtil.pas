unit dluHexUtil;

{$mode objfpc}{$H+}

{==============================================================================

  Unit Name : dluHexUtil
  Purpose   : Fast and memory-efficient byte array / pointer <-> HEX string
              conversions supporting both Big-Endian and Little-Endian byte orders.

  Features  :
    - High-performance lookup-table-based HEX encoding and decoding.
    - Support for custom separators between byte representations (e.g., ' ', ':').
    - Output truncation support for string formatting (AMaxBytes > 0 appends '...').
    - Dual endianness support:
        * Big-Endian (BE): Normal byte order (first byte to last byte).
        * Little-Endian (LE): Reversed byte order (last byte to first byte).
    - Exception-safe parsing options (TryHexToBytes / TryHexToBytesLE).

==============================================================================}

interface

uses SysUtils;

// --- Big-Endian (Natural Order) Functions ---

// Converts a byte buffer to a HEX string (from first byte to last byte).
// ASeparator is inserted between hex pairs (empty = no separator).
// AMaxBytes > 0 limits the printed byte count and appends ASeparator + '...' when truncated.
function BytesToHex( const AData: PByte; const ACount: SizeInt; const ASeparator: string = ''; AMaxBytes: SizeInt = 0 ): string; overload; inline;
function BytesToHex( const AData: array of Byte; const ASeparator: string = ''; const AMaxBytes: SizeInt = 0 ): string; overload;

// Reverses BytesToHex: converts a HEX string to a byte array.
// ASeparator must match the one used during encoding.
// Returns False and sets ABytes to nil on invalid HEX format without throwing exceptions.
function TryHexToBytes( const AHex: string; out ABytes: TBytes; const ASeparator: string = '' ): Boolean; inline;

// Same as TryHexToBytes, but raises EConvertError on invalid format instead of returning False.
function HexToBytes( const AHex: string; const ASeparator: string = '' ): TBytes; inline;

// --- Little-Endian (Reversed Order) Functions ---

// Converts a byte buffer to a HEX string in Little-Endian byte order (from last byte to first byte).
function BytesToHexLE( const AData: PByte; const ACount: SizeInt; const ASeparator: string = ''; AMaxBytes: SizeInt = 0 ): string; overload; inline;
function BytesToHexLE( const AData: array of Byte; const ASeparator: string = ''; const AMaxBytes: SizeInt = 0 ): string; overload;

// Reverses BytesToHexLE: converts a HEX string to a byte array filled from last index to first index.
function TryHexToBytesLE( const AHex: string; out ABytes: TBytes; const ASeparator: string = '' ): Boolean; inline;

// Same as TryHexToBytesLE, but raises EConvertError on invalid format.
function HexToBytesLE( const AHex: string; const ASeparator: string = '' ): TBytes; inline;

implementation

type
  // Byte order selector used internally for refactored routines
  TEndianness = (eBigEndian, eLittleEndian);

  THexPair = array[0..1] of Char;
  PHexPair = ^THexPair;

const
  // Precomputed HEX character pairs for every byte value (0..255).
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
  // Reverse lookup table for ASCII hex characters (0..15 nibble value, -1 for invalid chars).
  HexNibbleTable : array[Byte] of ShortInt;

{ Core internal function for encoding byte buffers to HEX }
function BytesToHexInternal( const AData: PByte; const ACount: SizeInt; const ASeparator: string; AMaxBytes: SizeInt; AEndianness: TEndianness ): string;
  var i, LCount, LSepLen, LTotalLen, LSrcInc : SizeInt;
      LHasMore                               : Boolean;
      LDst                                   : PChar;
      LSrc                                   : PByte;

   procedure WriteHexByte; inline;
   begin
      PHexPair( LDst )^ := HexPairTable[ LSrc^ ];
      Inc( LDst, 2 );
      Inc( LSrc, LSrcInc );
   end;
begin
   if ( ACount <= 0 ) or ( AData = nil ) then Exit( '' );
   if AMaxBytes < 0 then AMaxBytes := 0;

   LCount   := ACount;
   LHasMore := ( AMaxBytes > 0 ) and ( ACount > AMaxBytes );
   if LHasMore then
      LCount := AMaxBytes;

   LSepLen   := Length( ASeparator );
   LTotalLen := ( LCount shl 1 ) + ( Pred( LCount ) * LSepLen );
   if LTotalLen <= 0 then
      Exit('');

   if LHasMore then
      Inc( LTotalLen, LSepLen + 3 );

   SetLength(Result, LTotalLen);
   LDst := @Result[1];

   // Determine starting pointer and iteration direction once before the loop
   LSrc := AData;
   if AEndianness = eBigEndian then begin
      LSrcInc := 1;
   end else begin
      Inc( LSrc, ACount-1 );
      LSrcInc := -1;
   end;

   WriteHexByte;
   if LSepLen = 0 then begin
      for i := 1 to Pred( LCount ) do begin
         WriteHexByte;
      end;
   end else if LSepLen = 1 then begin
      for i := 1 to Pred( LCount ) do begin
         LDst^ := ASeparator[1];
         Inc( LDst );
         WriteHexByte;
      end;
   end else begin
      for i := 1 to Pred( LCount ) do begin
         Move( ASeparator[1], LDst^, LSepLen );
         Inc( LDst, LSepLen );
         WriteHexByte;
      end;
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

{ Core internal function for decoding HEX strings to byte arrays }
function TryHexToBytesInternal( const AHex: string; out ABytes: TBytes; const ASeparator: string; AEndianness: TEndianness ): Boolean;
  var i, LHexLen, LSepLen, LByteCount, LDstInc : SizeInt;
      LSrc                                     : PChar;
      LDstByte                                 : PByte;
      LHi, LLo                                 : ShortInt;
begin
   ABytes  := nil;
   LHexLen := Length( AHex );
   if LHexLen = 0 then Exit( True );

   LSepLen := Length( ASeparator );
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

   // Determine destination pointer and iteration direction once before the loop
   if AEndianness = eBigEndian then begin
      LDstByte := @ABytes[0];
      LDstInc  := 1;
   end else begin
      LDstByte := @ABytes[ LByteCount - 1 ];
      LDstInc  := -1;
   end;

   for i := 0 to Pred( LByteCount ) do begin
      if ( i > 0 ) and ( LSepLen > 0 ) then begin
         if not {%H-}CompareMem( LSrc, PChar( ASeparator ), LSepLen ) then begin
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

      LDstByte^ := Byte( ( LHi shl 4 ) or LLo );
      Inc( LDstByte, LDstInc );
      Inc( LSrc, 2 );
   end;

   Result := True;
end;

// --- Public Interface Wrappers ---

function BytesToHex( const AData: PByte; const ACount: SizeInt; const ASeparator: string; AMaxBytes: SizeInt ): string;
begin
   Result := BytesToHexInternal( AData, ACount, ASeparator, AMaxBytes, eBigEndian );
end;

function BytesToHex( const AData: array of Byte; const ASeparator: string; const AMaxBytes: SizeInt ): string;
begin
   if Length( AData ) = 0 then Exit( '' );
   Result := BytesToHexInternal( @AData[0], Length( AData ), ASeparator, AMaxBytes, eBigEndian );
end;

function BytesToHexLE( const AData: PByte; const ACount: SizeInt; const ASeparator: string; AMaxBytes: SizeInt ): string;
begin
   Result := BytesToHexInternal( AData, ACount, ASeparator, AMaxBytes, eLittleEndian );
end;

function BytesToHexLE( const AData: array of Byte; const ASeparator: string; const AMaxBytes: SizeInt ): string;
begin
   if Length( AData ) = 0 then Exit( '' );
   Result := BytesToHexInternal( @AData[0], Length( AData ), ASeparator, AMaxBytes, eLittleEndian );
end;

function TryHexToBytes( const AHex: string; out ABytes: TBytes; const ASeparator: string ): Boolean;
begin
   Result := TryHexToBytesInternal( AHex, ABytes, ASeparator, eBigEndian );
end;

function HexToBytes( const AHex: string; const ASeparator: string ): TBytes;
begin
   if not TryHexToBytesInternal( AHex, Result, ASeparator, eBigEndian ) then
      raise EConvertError.CreateFmt( 'Invalid HEX format: "%s"', [ AHex ] );
end;

function TryHexToBytesLE( const AHex: string; out ABytes: TBytes; const ASeparator: string ): Boolean;
begin
   Result := TryHexToBytesInternal( AHex, ABytes, ASeparator, eLittleEndian );
end;

function HexToBytesLE( const AHex: string; const ASeparator: string ): TBytes;
begin
   if not TryHexToBytesInternal( AHex, Result, ASeparator, eLittleEndian ) then
      raise EConvertError.CreateFmt( 'Invalid HEX format (LE): "%s"', [ AHex ] );
end;

procedure InitHexNibbleTable;
  var c : Char;
begin
   FillChar( HexNibbleTable, SizeOf( HexNibbleTable ), $FF ); // -1 = invalid character
   for c := '0' to '9' do HexNibbleTable[ Ord( c ) ] := Ord( c ) - Ord( '0' );
   for c := 'A' to 'F' do HexNibbleTable[ Ord( c ) ] := Ord( c ) - Ord( 'A' ) + 10;
   for c := 'a' to 'f' do HexNibbleTable[ Ord( c ) ] := Ord( c ) - Ord( 'a' ) + 10;
end;

initialization
   InitHexNibbleTable;

end.
