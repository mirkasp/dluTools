unit dluSqlite3DbHeader;

{$IFDEF FPC}
  //{$mode objfpc}{$H+}
  {$mode DELPHI}
  {$modeswitch UNICODESTRINGS+}
  {$modeswitch ADVANCEDRECORDS+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

// https://www.sqlite.org/fileformat2.html#database_header

type TuSDHF = (
    hfMagicStr,
    hfPageSize,
    hfFileFormatW,
    hfFileFormatR,
    hfUnusedSpace,
    hfEmbdPayloadMax,
    hfEmbdPayloadMin,
    hfLeafPayload,
    hfChangeCounter,
    hfSizeInPages,
    hfTP_PageNumber,
    hfFL_TotalPages,
    hfSchemaCookie,
    hfSchemaFormat,
    hfPageCacheSize,
    hfLargesRoot,
    hfTextEncoding,
    hfUserVersion,
    hfIncVacuumMode,
    hfApplicationID,
    hfReserved,
    hfVersion,
    hfVersionNumber );

type TuSDHF_Descriptor = record
   Name : string;
   Desc : string;
end;



{ TuSqlite3DbHeader }

type TuSqlite3DbHeader = class
  strict private
     var Buffer  : Pointer;
         BufSize : integer;
         Fields  : array[ TuSDHF ] of TuSDHF_Descriptor;
  public
    constructor Create( const AFileName: String );
    destructor Destroy; override;
    function ToString(): String; reintroduce;
    function GetNameStr( const AVal: TuSDHF ): String;
    function GetValueStr( const AVal: TuSDHF ): String;
    function GetValueStrHex( const AVal: TuSDHF ): String;
    function GetDescStr( const AVal: TuSDHF ): String;
    function GetValueDesc( const AVal: TuSDHF ): String;
    function MagicStr(): String;
    function PageSize(): Cardinal;
    function GetValue( const AVal: TuSDHF ): Cardinal;
end;

implementation

uses SysUtils
   , Classes
   //, Dialogs
   //, AppTools
   ;

type UInt1 = Byte;
type UInt2 = Word;
type UInt4 = LongWord;

// Database Header Format
type TuSqliteFileHeader = packed record           // Offset/Size/Description
    MagicStr       : array[0..15] of UInt1;       //  0      16 The header string: "SQLite format 3\000"
    PageSize       : UInt2;                       // 16       2 The database page size in bytes. Must be a power of two between 512 and 32768 inclusive, or the value 1 representing a page size of 65536.
    FileFormatW    : UInt1;                       // 18       1 File format write version. 1 for legacy; 2 for WAL.
    FileFormatR    : UInt1;                       // 19       1 File format read version. 1 for legacy; 2 for WAL.
    UnusedSpace    : UInt1;                       // 20       1 Bytes of unused "reserved" space at the end of each page. Usually 0.
    EmbdPayloadMax : UInt1;                       // 21       1 Maximum embedded payload fraction. Must be 64.
    EmbdPayloadMin : UInt1;                       // 22       1 Minimum embedded payload fraction. Must be 32.
    LeafPayload    : UInt1;                       // 23       1 Leaf payload fraction. Must be 32.
    ChangeCounter  : UInt4;                       // 24       4 File change counter.
    SizeInPages    : UInt4;                       // 28       4 Size of the database file in pages. The "in-header database size".
    TP_PageNumber  : UInt4;                       // 32       4 Page number of the first freelist trunk page.
    FL_TotalPages  : UInt4;                       // 36       4 Total number of freelist pages.
    SchemaCookie   : UInt4;                       // 40       4 The schema cookie.
    SchemaFormat   : UInt4;                       // 44       4 The schema format number. Supported schema formats are 1, 2, 3, and 4.
    PageCacheSize  : UInt4;                       // 48       4 Default page cache size.
    LargesRoot     : UInt4;                       // 52       4 The page number of the largest root b-tree page when in auto-vacuum or incremental-vacuum modes, or zero otherwise.
    TextEncoding   : UInt4;                       // 56       4 The database text encoding. A value of 1 means UTF-8. A value of 2 means UTF-16le. A value of 3 means UTF-16be.
    UserVersion    : UInt4;                       // 60       4 The "user version" as read and set by the user_version pragma.
    IncVacuumMode  : UInt4;                       // 64       4 True (non-zero) for incremental-vacuum mode. False (zero) otherwise.
    ApplicationID  : UInt4;                       // 68       4 The "Application ID" set by PRAGMA application_id.
    Reserved       : array[0..19] of UInt1;       // 72      20 Reserved for expansion. Must be zero.
    Version        : UInt4;                       // 92       4 The version-valid-for number.
    VersionNumber  : UInt4;                       // 96       4 SQLITE_VERSION_NUMBER
end;

type PHeader = ^TuSqliteFileHeader;


const HexDigit: array[0..15] of AnsiChar = '0123456789ABCDEF';

function ByteAsHex( const AData: PAnsiChar ): shortstring;
begin
   Result := HexDigit[Ord(AData^) shr 4] + HexDigit[Ord(AData^) and $0F];
end;

function DataAsHex( const AData: PAnsiChar ): UnicodeString;
  var p : PAnsiChar;
begin
   p := AData;
   Result := UnicodeString(ByteAsHex( p ));
   while p^ <> #0 do begin
      Inc( p );
      Result := Result + ' ' + UnicodeString(ByteAsHex( p ));
   end;
end;

procedure SwapBytes(var Bytes; Len: Integer);
  var Swapped: PByte;
      i: Integer;
begin
  if Len > 1 then begin
     GetMem(Swapped, Len);
     try
        for i := 0 to Len - 1 do Swapped[Len - i - 1] := PByte(@Bytes)[i];
        Move(Swapped^, Bytes, Len);
     finally
        FreeMem(Swapped);
     end;
  end;
end;

function SwapX( const n: Word ): Word; overload;
begin
   Result := n;
   SwapBytes( Result, SizeOf(Result) );
end;

function SwapX( const n: LongWord ): LongWord; overload;
begin
   Result := n;
   SwapBytes( Result, SizeOf(Result) );
end;

function AsUnicodeString( const AValue: Cardinal ): UnicodeString;
begin
   Result := UnicodeString( IntToStr( AValue ) );
end;

const UxHexDigit: array[0..15] of UnicodeChar = '0123456789ABCDEF';

function AsHex( const AValue: byte ): UnicodeString; overload;
begin
   Result := UxHexDigit[ AValue shr 4] + UxHexDigit[ AValue and $0F];
end;

{$WARN 4022 off : lo/hi(dword/qword) returns the upper/lower word/dword}
function AsHex( const AValue: Word ): UnicodeString; overload;
begin
   Result := AsHex( Hi( AValue ) ) + AsHex( Lo( AValue ) );
end;

function AsHex( const AValue: LongWord ): UnicodeString; overload;
begin
   Result := AsHex( Hi( AValue ) ) + ' ' +AsHex( Lo( AValue ) );
end;
{$WARN 4022 on}

{ TuSqlite3DbHeader }

constructor TuSqlite3DbHeader.Create( const AFileName: String) ;
  const cErrorStr = 'Illegal %s size ($d)';
  var Res  : integer;
      fs   : TFileStream;
begin
   Res := SizeOf(UInt1);
   if Res <> 1 then raise Exception.CreateFmt( cErrorStr, [ 'UInt1', Res ] );

   Res := SizeOf(UInt2);
   if Res <> 2 then raise Exception.CreateFmt( cErrorStr, [ 'UInt2', Res ] );

   Res := SizeOf(UInt4);
   if Res <> 4 then raise Exception.CreateFmt( cErrorStr, [ 'UInt4', Res ] );

   with Fields[ hfMagicStr ] do begin
      Name   := 'MagicStr';
      Desc   := 'The header string: "SQLite format 3\000"';
   end;
   with Fields[ hfPageSize ] do begin
      Name := 'PageSize';
      Desc := 'The database page size in bytes. Must be a power of two between 512 and 32768 inclusive, or the value 1 representing a page size of 65536.';
   end;
   with Fields[ hfFileFormatW ] do begin
      Name := 'FileFormatW';
      Desc := 'File format write version. 1 for legacy; 2 for WAL.';
   end;
   with Fields[ hfFileFormatR ] do begin
      Name := 'FileFormatR';
      Desc := 'File format read version. 1 for legacy; 2 for WAL.';
   end;
   with Fields[ hfUnusedSpace ] do begin
      Name := 'UnusedSpace';
      Desc := 'Bytes of unused "reserved" space at the end of each page. Usually 0.';
   end;
   with Fields[ hfEmbdPayloadMax ] do begin
      Name := 'EmbdPayloadMax';
      Desc := 'Maximum embedded payload fraction. Must be 64.';
   end;
   with Fields[ hfEmbdPayloadMin ] do begin
      Name := 'EmbdPayloafMin';
      Desc := 'Minimum embedded payload fraction. Must be 32.';
   end;
   with Fields[ hfLeafPayload ] do begin
      Name := 'LeafPayload';
      Desc := 'Leaf payload fraction. Must be 32.';
   end;
   with Fields[ hfChangeCounter ] do begin
      Name := 'ChangeCounter';
      Desc := 'File change counter.';
   end;
   with Fields[ hfSizeInPages ] do begin
      Name := 'SizeInPages';
      Desc := 'Size of the database file in pages. The "in-header database size".';
   end;
   with Fields[ hfTP_PageNumber ] do begin
      Name := 'TP_PageNumber';
      Desc := 'Page number of the first freelist trunk page.';
   end;
   with Fields[ hfFL_TotalPages ] do begin
      Name := 'FL_TotalPages';
      Desc := 'Total number of freelist pages.';
   end;
   with Fields[ hfSchemaCookie ] do begin
      Name := 'SchemaCookie';
      Desc := 'The schema cookie.';
   end;
   with Fields[ hfSchemaFormat ] do begin
      Name := 'SchemaFormat';
      Desc := 'The schema format number. Supported schema formats are 1, 2, 3, and 4.';
   end;
   with Fields[ hfPageCacheSize ] do begin
      Name := 'PageCacheSize';
      Desc := 'Default page cache size.';
   end;
   with Fields[ hfLargesRoot ] do begin
      Name := 'LargesRoot';
      Desc := 'The page number of the largest root b-tree page when in auto-vacuum or incremental-vacuum modes, or zero otherwise.';
   end;
   with Fields[ hfTextEncoding ] do begin
      Name := 'TextEncoding';
      Desc := 'The database text encoding. A value of 1 means UTF-8. A value of 2 means UTF-16le. A value of 3 means UTF-16be.';
   end;
   with Fields[ hfUserVersion ] do begin
      Name := 'UserVersion';
      Desc := 'The "user version" as read and set by the user_version pragma.';
   end;
   with Fields[ hfIncVacuumMode ] do begin
      Name := 'IncVacuumMode';
      Desc := 'True (non-zero) for incremental-vacuum mode. False (zero) otherwise.';
   end;
   with Fields[ hfApplicationID ] do begin
      Name := 'ApplicationID';
      Desc := 'The "Application ID" set by PRAGMA application_id.';
   end;
   with Fields[ hfReserved ] do begin
      Name := 'Reserved';
      Desc := 'Reserved for expansion. Must be zero.';
   end;
   with Fields[ hfVersion ] do begin
      Name := 'Version';
      Desc := 'The version-valid-for number.';
   end;
   with Fields[ hfVersionNumber ] do begin
      Name := 'VersionNumber';
      Desc := 'SQLITE_VERSION_NUMBER';
   end;

///////////////
   BufSize := SizeOf( TuSqliteFileHeader );
   GetMem( Buffer, BufSize );

   fs := TFileStream.Create( UTF8Encode( AFileName ), fmOpenRead+fmShareDenyNone );
   fs.Seek( 0, fsFromBeginning );
   if fs.Read( Buffer^, BufSize ) <> BufSize then raise Exception.Create( 'SQLite database reading error!' );
   fs.Free;

end;

destructor TuSqlite3DbHeader.Destroy;
begin
  FreeMem( Buffer );
  inherited Destroy;
end;

function TuSqlite3DbHeader.GetDescStr( const AVal: TuSDHF) : String;
begin
   Result := Fields[ AVal ].Desc;
end;

function TuSqlite3DbHeader.GetValueDesc( const AVal: TuSDHF) : String;
 const cUnkVal = 'Unknown value';
       cLegVal = 'Legacy';
       cWALVal = 'WAL';
       cFmt    = '(%d) %s';
  var n: UInt4;
begin
  case AVal of
     hfFileFormatW :begin
          n := PHeader(Buffer)^.FileFormatW;
          case n of
             1 : Result := cLegVal;
             2 : Result := cWALVal;
            else Result := cUnkVal;
          end;
          Result := UTF8Decode( Format( cFmt, [ n, Result ] ) );
     end;
     hfFileFormatR : begin
          n := PHeader(Buffer)^.FileFormatR;
          case n of
             1 : Result := cLegVal;
             2 : Result := cWALVal;
            else Result := cUnkVal;
          end;
          Result := UTF8Decode( Format( cFmt, [ n, Result ] ) );
     end;
     hfTextEncoding: begin
          n := swapx( PHeader(Buffer)^.TextEncoding );
          case n of
             1 : Result := 'UTF-8';
             2 : Result := 'UTF-16le';
             3 : Result := 'UTF-16be';
            else Result := cUnkVal;
          end;
          Result := UTF8Decode( Format( cFmt, [ n, Result ] ) );
     end
  else Result := GetValueStr( AVal );
  end;
end;

function TuSqlite3DbHeader.GetNameStr( const AVal: TuSDHF) : String;
begin
   Result := Fields[ AVal ].Name;
end;

function TuSqlite3DbHeader.GetValue( const AVal: TuSDHF) : Cardinal;
  var tmp : UInt2;
begin
   case AVal of
      //hfMagicStr            : Result := MagicStr();
      hfPageSize            : begin
                                 tmp := swapx( PHeader(Buffer)^.PageSize );
                                 if tmp = $FFFF
                                     then Result := $10000
                                     else Result := tmp;
                              end;
      hfFileFormatW         : Result := PHeader( Buffer )^.FileFormatW;
      hfFileFormatR         : Result := PHeader( Buffer )^.FileFormatR;
      hfUnusedSpace         : Result := PHeader( Buffer )^.UnusedSpace;
      hfEmbdPayloadMax      : Result := PHeader( Buffer )^.EmbdPayloadMax;
      hfEmbdPayloadMin      : Result := PHeader( Buffer )^.EmbdPayloadMin;
      hfLeafPayload         : Result := PHeader( Buffer )^.LeafPayload;
      hfChangeCounter       : Result := swapx( PHeader( Buffer )^.ChangeCounter );
      hfSizeInPages         : Result := swapx( PHeader( Buffer )^.SizeInPages   );
      hfTP_PageNumber       : Result := swapx( PHeader( Buffer )^.TP_PageNumber );
      hfFL_TotalPages       : Result := swapx( PHeader( Buffer )^.FL_TotalPages );
      hfSchemaCookie        : Result := swapx( PHeader( Buffer )^.SchemaCookie  );
      hfSchemaFormat        : Result := swapx( PHeader( Buffer )^.SchemaFormat  );
      hfPageCacheSize       : Result := swapx( PHeader( Buffer )^.PageCacheSize );
      hfLargesRoot          : Result := swapx( PHeader( Buffer )^.LargesRoot    );
      hfTextEncoding        : Result := swapx( PHeader( Buffer )^.TextEncoding  );
      hfUserVersion         : Result := swapx( PHeader( Buffer )^.UserVersion   );
      hfIncVacuumMode       : Result := swapx( PHeader( Buffer )^.IncVacuumMode );
      hfApplicationID       : Result := swapx( PHeader( Buffer )^.ApplicationID );
//      hfReserved            :
      hfVersion             : Result := swapx( PHeader( Buffer )^.Version       );
      hfVersionNumber       : Result := swapx( PHeader( Buffer )^.VersionNumber );
   else
      raise Exception.Create( 'Illegal numeric argument in GetValue function' );
   end;

end;

function TuSqlite3DbHeader.GetValueStr( const AVal: TuSDHF ) : String;
begin
   case AVal of
      hfMagicStr            : Result := MagicStr();
      hfPageSize            : Result := AsUnicodeString( PageSize() );
      hfFileFormatW         : Result := AsUnicodeString( PHeader(Buffer)^.FileFormatW );
      hfFileFormatR         : Result := AsUnicodeString( PHeader(Buffer)^.FileFormatR );
      hfUnusedSpace         : Result := AsUnicodeString( PHeader(Buffer)^.UnusedSpace );
      hfEmbdPayloadMax      : Result := AsUnicodeString( PHeader(Buffer)^.EmbdPayloadMax );
      hfEmbdPayloadMin      : Result := AsUnicodeString( PHeader(Buffer)^.EmbdPayloadMin );
      hfLeafPayload         : Result := AsUnicodeString( PHeader(Buffer)^.LeafPayload );
      hfChangeCounter       : Result := AsUnicodeString( swapx(PHeader(Buffer)^.ChangeCounter) );
      hfSizeInPages         : Result := AsUnicodeString( swapx(PHeader(Buffer)^.SizeInPages) );
      hfTP_PageNumber       : Result := AsUnicodeString( swapx(PHeader(Buffer)^.TP_PageNumber) );
      hfFL_TotalPages       : Result := AsUnicodeString( swapx(PHeader(Buffer)^.FL_TotalPages) );
      hfSchemaCookie        : Result := AsUnicodeString( swapx(PHeader(Buffer)^.SchemaCookie) );
      hfSchemaFormat        : Result := AsUnicodeString( swapx(PHeader(Buffer)^.SchemaFormat) );
      hfPageCacheSize       : Result := AsUnicodeString( swapx(PHeader(Buffer)^.PageCacheSize) );
      hfLargesRoot          : Result := AsUnicodeString( swapx(PHeader(Buffer)^.LargesRoot) );
      hfTextEncoding        : Result := AsUnicodeString( swapx(PHeader(Buffer)^.TextEncoding) );
      hfUserVersion         : Result := AsUnicodeString( swapx(PHeader(Buffer)^.UserVersion) );
      hfIncVacuumMode       : Result := AsUnicodeString( swapx(PHeader(Buffer)^.IncVacuumMode) );
      hfApplicationID       : Result := AsUnicodeString( swapx(PHeader(Buffer)^.ApplicationID) );
//      hfReserved            :
      hfVersion             : Result := AsUnicodeString( swapx( PHeader(Buffer)^.Version ) );
      hfVersionNumber       : Result := AsUnicodeString( swapx( PHeader(Buffer)^.VersionNumber ) );

   else
      Result := '...';
   end;
end;

function TuSqlite3DbHeader.GetValueStrHex( const AVal: TuSDHF ) : String;
  var p: PAnsiChar;
begin
   case AVal of
      hfMagicStr            : begin
         p := @PHeader(Buffer)^.MagicStr;
         Result := DataAsHex( p );
      end;
      hfPageSize            : Result := '$' + AsHex( PageSize() );
      hfFileFormatW         : Result := '$' + AsHex( PHeader(Buffer)^.FileFormatW    );
      hfFileFormatR         : Result := '$' + AsHex( PHeader(Buffer)^.FileFormatR    );
      hfUnusedSpace         : Result := '$' + AsHex( PHeader(Buffer)^.UnusedSpace    );
      hfEmbdPayloadMax      : Result := '$' + AsHex( PHeader(Buffer)^.EmbdPayloadMax );
      hfEmbdPayloadMin      : Result := '$' + AsHex( PHeader(Buffer)^.EmbdPayloadMin );
      hfLeafPayload         : Result := '$' + AsHex( PHeader(Buffer)^.LeafPayload    );
      hfChangeCounter       : Result := '$' + AsHex( swapx( PHeader(Buffer)^.ChangeCounter ) );
      hfSizeInPages         : Result := '$' + AsHex( swapx( PHeader(Buffer)^.SizeInPages   ) );
      hfTP_PageNumber       : Result := '$' + AsHex( swapx( PHeader(Buffer)^.TP_PageNumber ) );
      hfFL_TotalPages       : Result := '$' + AsHex( swapx( PHeader(Buffer)^.FL_TotalPages ) );
      hfSchemaCookie        : Result := '$' + AsHex( swapx( PHeader(Buffer)^.SchemaCookie  ) );
      hfSchemaFormat        : Result := '$' + AsHex( swapx( PHeader(Buffer)^.SchemaFormat  ) );
      hfPageCacheSize       : Result := '$' + AsHex( swapx( PHeader(Buffer)^.PageCacheSize ) );
      hfLargesRoot          : Result := '$' + AsHex( swapx( PHeader(Buffer)^.LargesRoot    ) );
      hfTextEncoding        : Result := '$' + AsHex( swapx( PHeader(Buffer)^.TextEncoding  ) );
      hfUserVersion         : Result := '$' + AsHex( swapx( PHeader(Buffer)^.UserVersion   ) );
      hfIncVacuumMode       : Result := '$' + AsHex( swapx( PHeader(Buffer)^.IncVacuumMode ) );
      hfApplicationID       : Result := '$' + AsHex( swapx( PHeader(Buffer)^.ApplicationID ) );
//      hfReserved            :
      hfVersion             : Result := '$' + AsHex( swapx( PHeader(Buffer)^.Version       ) );
      hfVersionNumber       : Result := '$' + AsHex( swapx( PHeader(Buffer)^.VersionNumber ) );

   else
      Result := '...';
   end;
end;

function TuSqlite3DbHeader.ToString( ) : String;
begin
   Result := String(inherited ToString()) + sLinebreak +
             UTF8Decode('Test Line = ĄĘĆŁŃÓŚŻŹ ąęćłńóśżź') + sLineBreak +
             'BufSize = ' + AsUnicodeString( BufSize ) + sLineBreak +
             'MagicStr = '+ MagicStr() + sLineBreak +
             'PageSize = ' + AsUnicodeString(PageSize()) + sLineBreak +
             'ApplicationID = ' + AsUnicodeString(PHeader(Buffer)^.ApplicationId) + ' # ' + AsUnicodeString(swapx(PHeader(Buffer)^.ApplicationID)) + sLineBreak +
             'Version = ' + AsUnicodeString(PHeader(Buffer)^.Version) + ' # ' + AsUnicodeString(swapx(PHeader(Buffer)^.Version)) + sLineBreak +
             'Version = $' + AsHex(PHeader(Buffer)^.Version) + ' # $' + AsHex(swapx(PHeader(Buffer)^.Version)) + sLineBreak +
             'VersionNumber = ' + AsUnicodeString(PHeader(Buffer)^.VersionNumber) + ' # ' + AsUnicodeString(swapx(PHeader(Buffer)^.VersionNumber)) + sLineBreak +
             'VersionNumber = $' + AsHex(PHeader(Buffer)^.VersionNumber) + ' # $' + AsHex(swapx(PHeader(Buffer)^.VersionNumber)) + sLineBreak
            ;
end;

{$IFDEF FPC}
function TuSqlite3DbHeader.MagicStr( ) : String;
begin
  SetString( Result, Addr( PHeader(Buffer)^.MagicStr ), Length(PHeader(Buffer)^.MagicStr)-1 );
end;
{$ELSE}
function TuSqliteDatabaseHeader.MagicStr( ) : string;
  var p : PAnsiChar;
begin
  p := @PHeader(Buffer)^.MagicStr;
  SetString( Result, p, StrLen( p ) );
end;
{$ENDIF}

function TuSqlite3DbHeader.PageSize( ) : Cardinal;
  var ps : UInt2;
begin
   ps := swapx( PHeader(Buffer)^.PageSize );
   if ps = $FFFF
     then Result := $10000
     else Result := ps;
end;

end.
