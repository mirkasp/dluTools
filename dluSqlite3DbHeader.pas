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

uses Classes;

// https://www.sqlite.org/fileformat2.html#database_header

type TuSDHF = ( hfMagicStr,
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
                hfVersionNumber
              );

{ TuSqlite3DbHeader }

type TuSqlite3DbHeader = class
  strict private
     var Buffer  : Pointer;
         BufSize : integer;
     function PageSizeInt( const AX: Word ): Cardinal;
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
    procedure AsStrings( const AStrings: TStrings );
end;

implementation

uses SysUtils
   ;

type TuSDHF_Descriptor = record
   Name : string;
   Desc : string;
end;

const cFieldDescriptors: array[TuSDHF] of TuSDHF_Descriptor = (
      (* hfMagicStr       *) (Name: 'MagicStr';       Desc: 'The header string: "SQLite format 3\000"' ),
      (* hfPageSize       *) (Name: 'PageSize';       Desc: 'The database page size in bytes. Must be a power of two between 512 and 32768 inclusive, or the value 1 representing a page size of 65536.' ),
      (* hfFileFormatW    *) (Name: 'FileFormatW';    Desc: 'File format write version. 1 for legacy; 2 for WAL.' ),
      (* hfFileFormatR    *) (Name: 'FileFormatR';    Desc: 'File format read version. 1 for legacy; 2 for WAL.' ),
      (* hfUnusedSpace    *) (Name: 'UnusedSpace';    Desc: 'Bytes of unused "reserved" space at the end of each page. Usually 0.' ),
      (* hfEmbdPayloadMax *) (Name: 'EmbdPayloadMax'; Desc: 'Maximum embedded payload fraction. Must be 64.' ),
      (* hfEmbdPayloadMin *) (Name: 'EmbdPayloadMin'; Desc: 'Minimum embedded payload fraction. Must be 32.' ),
      (* hfLeafPayload    *) (Name: 'LeafPayload';    Desc: 'Leaf payload fraction. Must be 32.' ),
      (* hfChangeCounter  *) (Name: 'ChangeCounter';  Desc: 'File change counter.' ),
      (* hfSizeInPages    *) (Name: 'SizeInPages';    Desc: 'Size of the database file in pages. The "in-header database size".' ),
      (* hfTP_PageNumber  *) (Name: 'TP_PageNumber';  Desc: 'Page number of the first freelist trunk page.' ),
      (* hfFL_TotalPages  *) (Name: 'FL_TotalPages';  Desc: 'Total number of freelist pages.' ),
      (* hfSchemaCookie   *) (Name: 'SchemaCookie';   Desc: 'The schema cookie.' ),
      (* hfSchemaFormat   *) (Name: 'SchemaFormat';   Desc: 'The schema format number. Supported schema formats are 1, 2, 3, and 4.' ),
      (* hfPageCacheSize  *) (Name: 'PageCacheSize';  Desc: 'Default page cache size.' ),
      (* hfLargesRoot     *) (Name: 'LargesRoot';     Desc: 'The page number of the largest root b-tree page when in auto-vacuum or incremental-vacuum modes, or zero otherwise.' ),
      (* hfTextEncoding   *) (Name: 'TextEncoding';   Desc: 'The database text encoding. A value of 1 means UTF-8. A value of 2 means UTF-16le. A value of 3 means UTF-16be.' ),
      (* hfUserVersion    *) (Name: 'UserVersion';    Desc: 'The "user version" as read and set by the user_version pragma.' ),
      (* hfIncVacuumMode  *) (Name: 'IncVacuumMode';  Desc: 'True (non-zero) for incremental-vacuum mode. False (zero) otherwise.' ),
      (* hfApplicationID  *) (Name: 'ApplicationID';  Desc: 'The "Application ID" set by PRAGMA application_id.' ),
      (* hfReserved       *) (Name: 'Reserved';       Desc: 'Reserved for expansion. Must be zero.' ),
      (* hfVersion        *) (Name: 'Version';        Desc: 'The version-valid-for number.' ),
      (* hfVersionNumber  *) (Name: 'VersionNumber';  Desc: 'SQLITE_VERSION_NUMBER' )
);

// Database Header Format
type TuSqliteFileHeader = packed record           // Offset/Size/Description
    MagicStr       : array[0..15] of Byte;        //  0      16 The header string: "SQLite format 3\000"
    PageSize       : Word;                        // 16       2 The database page size in bytes. Must be a power of two between 512 and 32768 inclusive, or the value 1 representing a page size of 65536.
    FileFormatW    : Byte;                        // 18       1 File format write version. 1 for legacy; 2 for WAL.
    FileFormatR    : Byte;                        // 19       1 File format read version. 1 for legacy; 2 for WAL.
    UnusedSpace    : Byte;                        // 20       1 Bytes of unused "reserved" space at the end of each page. Usually 0.
    EmbdPayloadMax : Byte;                        // 21       1 Maximum embedded payload fraction. Must be 64.
    EmbdPayloadMin : Byte;                        // 22       1 Minimum embedded payload fraction. Must be 32.
    LeafPayload    : Byte;                        // 23       1 Leaf payload fraction. Must be 32.
    SizeInPages    : LongWord;                    // 28       4 Size of the database file in pages. The "in-header database size".
    TP_PageNumber  : LongWord;                    // 32       4 Page number of the first freelist trunk page.
    FL_TotalPages  : LongWord;                    // 36       4 Total number of freelist pages.
    ChangeCounter  : LongWord;                    // 24       4 File change counter.
    SchemaCookie   : LongWord;                    // 40       4 The schema cookie.
    SchemaFormat   : LongWord;                    // 44       4 The schema format number. Supported schema formats are 1, 2, 3, and 4.
    PageCacheSize  : LongWord;                    // 48       4 Default page cache size.
    LargesRoot     : LongWord;                    // 52       4 The page number of the largest root b-tree page when in auto-vacuum or incremental-vacuum modes, or zero otherwise.
    TextEncoding   : LongWord;                    // 56       4 The database text encoding. A value of 1 means UTF-8. A value of 2 means UTF-16le. A value of 3 means UTF-16be.
    UserVersion    : LongWord;                    // 60       4 The "user version" as read and set by the user_version pragma.
    IncVacuumMode  : LongWord;                    // 64       4 True (non-zero) for incremental-vacuum mode. False (zero) otherwise.
    ApplicationID  : LongWord;                    // 68       4 The "Application ID" set by PRAGMA application_id.
    Reserved       : array[0..19] of Byte;        // 72      20 Reserved for expansion. Must be zero.
    Version        : LongWord;                    // 92       4 The version-valid-for number.
    VersionNumber  : LongWord;                    // 96       4 SQLITE_VERSION_NUMBER
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

{ ---------------------------------------------------------------------------- }
{ TuSqlite3DbHeader }

constructor TuSqlite3DbHeader.Create( const AFileName: String) ;
  const cErrorStr = 'Illegal %s size ($d)';
  var Res  : integer;
      fs   : TFileStream;
begin
   Res := SizeOf(Byte);
   if Res <> 1 then raise Exception.CreateFmt( cErrorStr, [ 'UInt1', Res ] );

   Res := SizeOf(Word);
   if Res <> 2 then raise Exception.CreateFmt( cErrorStr, [ 'UInt2', Res ] );

   Res := SizeOf(LongWord);
   if Res <> 4 then raise Exception.CreateFmt( cErrorStr, [ 'UInt4', Res ] );

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
   Result := cFieldDescriptors[ AVal ].Desc;
end;

function TuSqlite3DbHeader.GetNameStr( const AVal: TuSDHF) : String;
begin
   Result := cFieldDescriptors[ AVal ].Name;
end;

function TuSqlite3DbHeader.GetValueDesc( const AVal: TuSDHF) : String;
 const cUnkVal = 'Unknown value';
       cLegVal = 'Legacy';
       cWALVal = 'WAL';
       cFmt    = '(%d) %s';
  var n: LongWord;
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
          n := SwapEndian( PHeader(Buffer)^.TextEncoding );
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

function TuSqlite3DbHeader.GetValue( const AVal: TuSDHF) : Cardinal;
begin
   case AVal of
      //hfMagicStr            : Result := MagicStr();
      hfPageSize            : Result := PageSizeInt( PHeader(Buffer)^.PageSize );
      hfFileFormatW         : Result := PHeader( Buffer )^.FileFormatW;
      hfFileFormatR         : Result := PHeader( Buffer )^.FileFormatR;
      hfUnusedSpace         : Result := PHeader( Buffer )^.UnusedSpace;
      hfEmbdPayloadMax      : Result := PHeader( Buffer )^.EmbdPayloadMax;
      hfEmbdPayloadMin      : Result := PHeader( Buffer )^.EmbdPayloadMin;
      hfLeafPayload         : Result := PHeader( Buffer )^.LeafPayload;
      hfChangeCounter       : Result := SwapEndian( PHeader( Buffer )^.ChangeCounter );
      hfSizeInPages         : Result := SwapEndian( PHeader( Buffer )^.SizeInPages   );
      hfTP_PageNumber       : Result := SwapEndian( PHeader( Buffer )^.TP_PageNumber );
      hfFL_TotalPages       : Result := SwapEndian( PHeader( Buffer )^.FL_TotalPages );
      hfSchemaCookie        : Result := SwapEndian( PHeader( Buffer )^.SchemaCookie  );
      hfSchemaFormat        : Result := SwapEndian( PHeader( Buffer )^.SchemaFormat  );
      hfPageCacheSize       : Result := SwapEndian( PHeader( Buffer )^.PageCacheSize );
      hfLargesRoot          : Result := SwapEndian( PHeader( Buffer )^.LargesRoot    );
      hfTextEncoding        : Result := SwapEndian( PHeader( Buffer )^.TextEncoding  );
      hfUserVersion         : Result := SwapEndian( PHeader( Buffer )^.UserVersion   );
      hfIncVacuumMode       : Result := SwapEndian( PHeader( Buffer )^.IncVacuumMode );
      hfApplicationID       : Result := SwapEndian( PHeader( Buffer )^.ApplicationID );
      // hfReserved            :
      hfVersion             : Result := SwapEndian( PHeader( Buffer )^.Version       );
      hfVersionNumber       : Result := SwapEndian( PHeader( Buffer )^.VersionNumber );
   else
      raise Exception.Create( 'Illegal numeric argument in GetValue function' );
   end;

end;

procedure TuSqlite3DbHeader.AsStrings( const AStrings : TStrings) ;
begin
   if Assigned( AStrings ) then begin
      AStrings.AddPair( AnsiString( GetDescStr( hfMagicStr       ) ), AnsiString( GetValueDesc( hfMagicStr       ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfChangeCounter  ) ), AnsiString( GetValueDesc( hfChangeCounter  ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfPageSize       ) ), AnsiString( GetValueDesc( hfPageSize       ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfSizeInPages    ) ), AnsiString( GetValueDesc( hfSizeInPages    ) ) );
      AStrings.AddPair( 'Text encoding',                              AnsiString( GetValueDesc( hfTextEncoding   ) ) );
      AStrings.AddPair( 'Format WRITE version',                       AnsiString( GetValueDesc( hfFileFormatW    ) ) );
      AStrings.AddPair( 'Format READ version',                        AnsiString( GetValueDesc( hfFileFormatR    ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfUnusedSpace    ) ), AnsiString( GetValueDesc( hfUnusedSpace    ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfEmbdPayloadMax ) ), AnsiString( GetValueDesc( hfEmbdPayloadMax ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfEmbdPayloadMin ) ), AnsiString( GetValueDesc( hfEmbdPayloadMin ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfLeafPayload    ) ), AnsiString( GetValueDesc( hfLeafPayload    ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfTP_PageNumber  ) ), AnsiString( GetValueDesc( hfTP_PageNumber  ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfFL_TotalPages  ) ), AnsiString( GetValueDesc( hfFL_TotalPages  ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfSchemaCookie   ) ), AnsiString( GetValueDesc( hfSchemaCookie   ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfSchemaFormat   ) ), AnsiString( GetValueDesc( hfSchemaFormat   ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfPageCacheSize  ) ), AnsiString( GetValueDesc( hfPageCacheSize  ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfLargesRoot     ) ), AnsiString( GetValueDesc( hfLargesRoot     ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfUserVersion    ) ), AnsiString( GetValueDesc( hfUserVersion    ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfIncVacuumMode  ) ), AnsiString( GetValueDesc( hfIncVacuumMode  ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfApplicationID  ) ), AnsiString( GetValueDesc( hfApplicationID  ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfVersion        ) ), AnsiString( GetValueDesc( hfVersion        ) ) );
      AStrings.AddPair( AnsiString( GetDescStr( hfVersionNumber  ) ), AnsiString( GetValueDesc( hfVersionNumber  ) ) );
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
      hfChangeCounter       : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.ChangeCounter) );
      hfSizeInPages         : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.SizeInPages) );
      hfTP_PageNumber       : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.TP_PageNumber) );
      hfFL_TotalPages       : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.FL_TotalPages) );
      hfSchemaCookie        : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.SchemaCookie) );
      hfSchemaFormat        : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.SchemaFormat) );
      hfPageCacheSize       : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.PageCacheSize) );
      hfLargesRoot          : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.LargesRoot) );
      hfTextEncoding        : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.TextEncoding) );
      hfUserVersion         : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.UserVersion) );
      hfIncVacuumMode       : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.IncVacuumMode) );
      hfApplicationID       : Result := AsUnicodeString( SwapEndian(PHeader(Buffer)^.ApplicationID) );
//      hfReserved            :
      hfVersion             : Result := AsUnicodeString( SwapEndian( PHeader(Buffer)^.Version ) );
      hfVersionNumber       : Result := AsUnicodeString( SwapEndian( PHeader(Buffer)^.VersionNumber ) );

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
      hfChangeCounter       : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.ChangeCounter ) );
      hfSizeInPages         : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.SizeInPages   ) );
      hfTP_PageNumber       : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.TP_PageNumber ) );
      hfFL_TotalPages       : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.FL_TotalPages ) );
      hfSchemaCookie        : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.SchemaCookie  ) );
      hfSchemaFormat        : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.SchemaFormat  ) );
      hfPageCacheSize       : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.PageCacheSize ) );
      hfLargesRoot          : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.LargesRoot    ) );
      hfTextEncoding        : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.TextEncoding  ) );
      hfUserVersion         : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.UserVersion   ) );
      hfIncVacuumMode       : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.IncVacuumMode ) );
      hfApplicationID       : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.ApplicationID ) );
//      hfReserved            :
      hfVersion             : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.Version       ) );
      hfVersionNumber       : Result := '$' + AsHex( SwapEndian( PHeader(Buffer)^.VersionNumber ) );

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
             'ApplicationID = ' + AsUnicodeString(PHeader(Buffer)^.ApplicationId) + ' # ' + AsUnicodeString(SwapEndian(PHeader(Buffer)^.ApplicationID)) + sLineBreak +
             'Version = ' + AsUnicodeString(PHeader(Buffer)^.Version) + ' # ' + AsUnicodeString(SwapEndian(PHeader(Buffer)^.Version)) + sLineBreak +
             'Version = $' + AsHex(PHeader(Buffer)^.Version) + ' # $' + AsHex(SwapEndian(PHeader(Buffer)^.Version)) + sLineBreak +
             'VersionNumber = ' + AsUnicodeString(PHeader(Buffer)^.VersionNumber) + ' # ' + AsUnicodeString(SwapEndian(PHeader(Buffer)^.VersionNumber)) + sLineBreak +
             'VersionNumber = $' + AsHex(PHeader(Buffer)^.VersionNumber) + ' # $' + AsHex(SwapEndian(PHeader(Buffer)^.VersionNumber)) + sLineBreak
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

function TuSqlite3DbHeader.PageSizeInt( const AX: Word) : Cardinal;
  var ps : Word;
begin
   ps := SwapEndian( AX );
   if ps <> $0001
      then Result := ps
      else Result := $10000;
end ;

function TuSqlite3DbHeader.PageSize( ) : Cardinal;
begin
   Result := PageSizeInt( PHeader(Buffer)^.PageSize );
end;

end.
