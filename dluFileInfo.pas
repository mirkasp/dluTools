unit dluFileInfo;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch UNICODESTRINGS+}
  {$modeswitch ADVANCEDRECORDS+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Classes;

type TPEType = (pe_unknown, pe_32bit, pe_64bit );
const cPEType: array[ TPEType ] of string = ( 'unknown', '32-bit', '64-bit' );

{ TFileVersionInfo }
type TFileVersionInfo = record
    public
       FileName         : string;
       //
       IsStdFileInfo    : boolean;
       CreationTime     : TDateTime;    // FILETIME;
       LastAccessTime   : TDateTime;    // FILETIME;
       LastWriteTime    : TDateTime;    // FILETIME;

       FileAttributes   : Cardinal; // DWORD;
       FileSize         : UInt64;

       AlternateFileName: string; // array[0..13] of AnsiCHAR;
       //
       IsVersionInfo    : boolean;
       FileType         : string;
       FileExeType      : string;
       CompanyName      : string;
       FileDescription  : string;
       FileVersion      : string;
       InternalName     : string;
       LegalCopyRight   : string;
       LegalTradeMarks  : string;
       OriginalFileName : string;
       ProductName      : string;
       ProductVersion   : string;
       Comments         : string;
       SpecialBuildStr  : string;
       PrivateBuildStr  : string;
       FileFunction     : string;
       DebugBuild       : Boolean;
       PreRelease       : Boolean;
       SpecialBuild     : Boolean;
       PrivateBuild     : Boolean;
       Patched          : Boolean;
       InfoInferred     : Boolean;
       constructor Create( const AFileName: String );
       procedure ReadFileVersionInfo( const AFileName: string );
       procedure Clear();
       procedure AsString( const AStrings: TStrings );
    strict private
       function ReadStdFileInfo(): boolean;
       //
end;

function GetFileInfo( const AFileName: string = '' ): TFileVersionInfo;

// alternative version - only for version info
function GetFileVersion( const AFileName: string = '' ): string;

function GetPEType( const APath: UnicodeString ): TPEType; overload;
function GetPEType( const APath: AnsiString ): TPEType; overload;

function GetPETypeStr( const APath: UnicodeString ): UnicodeString; overload;
function GetPETypeStr( const APath: AnsiString ): AnsiString; overload;

function AttrWin32( const x: Cardinal ): string;

implementation

uses
  ShellApi, Windows
  , JwaWindows
{$IFDEF FPC}
  , LCLIntf
  , LCLType
{$ENDIF}
  , SysUtils
  //, dluDictionary
  ;

{$IFNDEF FPC}
// from lazarus file 'defines.inc'
const VFT2_DRV_INPUTMETHOD       = $0b;
const VFT2_DRV_VERSIONED_PRINTER = $0c;
{$ENDIF}

function ErrStr( const AType: String; const AValue: Cardinal ): String;
begin
   Result := 'Unknown ' + AType + ' type value (' + {%H-}IntToStr(AValue) + ')';
end ;

function GetFileTypeStr( const AValue: Cardinal ): String;
begin
   case AValue of
      VFT_UNKNOWN   : Result := 'Unknown';
      VFT_APP       : Result := 'Application';
      VFT_DLL       : Result := 'DLL';
      VFT_DRV       : Result := 'DRV';
      VFT_FONT      : Result := 'Font';
      VFT_VXD       : Result := 'VXD';
      VFT_STATIC_LIB: Result := 'Static-link Library';
      else            Result := ErrStr( 'file', AValue );
   end;
end;

function GetFileSubTypeStr( const AValue: Cardinal ): String;
begin
   case AValue of
      VFT2_UNKNOWN               : Result := 'Unknown Driver';
      VFT2_DRV_PRINTER           : Result := 'Printer Driver';
      VFT2_DRV_KEYBOARD          : Result := 'Keyboard Driver';
      VFT2_DRV_LANGUAGE          : Result := 'Language Driver';
      VFT2_DRV_DISPLAY           : Result := 'Display Driver';
      VFT2_DRV_MOUSE             : Result := 'Mouse Driver';
      VFT2_DRV_NETWORK           : Result := 'Network Driver';
      VFT2_DRV_SYSTEM            : Result := 'System Driver';
      VFT2_DRV_INSTALLABLE       : Result := 'InstallableDriver';
      VFT2_DRV_SOUND             : Result := 'Sound Driver';
      VFT2_DRV_COMM              : Result := 'Communications Driver';
      VFT2_DRV_INPUTMETHOD       : Result := 'Input method (?)';
      VFT2_DRV_VERSIONED_PRINTER : Result := 'Versioned Printer Driver';
      else                         Result := ErrStr( 'file sub-', AValue );
   end;
end;

function GetFontTypeStr( const AValue: Cardinal ): String;
begin
   case AValue of
      VFT2_UNKNOWN       : Result := 'Unknown Font';
      VFT2_FONT_RASTER   : Result := 'Raster Font';
      VFT2_FONT_VECTOR   : Result := 'Vector Font';
      VFT2_FONT_TRUETYPE : Result := 'Truetype Font';
      else                 Result := ErrStr( 'font', AValue );
   end ;
end;

function xFileTimeToDateTime( const AFileTime : TFileTime ) : TDateTime;
   var _time    : TFileTime;
       _systime : TSystemTime;
begin
   if not FileTimeToLocalFileTime( AFileTime, _time{%H-} )
      then RaiseLastOSError;
   if not FileTimeToSystemTime( _time, _systime{%H-} )
      then RaiseLastOSError;
   Result := SystemTimeToDateTime( _systime );
end;

function GetFileInfo( const AFileName: string) : TFileVersionInfo;
  var s : string;
begin
  if AFileName = ''
    then s := ParamStr(0)
    else s := AFileName;
  Result := TFileVersionInfo.Create( s );
end;

function GetFileVersion( const AFileName: string) : string;
begin
  Result := GetFileInfo( AFileName ).FileVersion;
end;

{ TFileVersionInfo }

constructor TFileVersionInfo.Create(const AFileName: String);
begin
   ReadFileVersionInfo( AFileName );
end;

procedure TFileVersionInfo.Clear();
begin
  // Initialize the Result
  Self := Default( TFileVersionInfo );
end;

procedure TFileVersionInfo.AsString( const AStrings : TStrings) ;
begin
   if IsVersionInfo then begin
      AStrings.AddPair( 'File Type',          UTF8Encode( self.FileType + ' ' + GetPETypeStr( self.FileName ) ) );
      AStrings.AddPair( 'File Function',      UTF8Encode( self.FileFunction     ) );
      AStrings.AddPair( 'File Exe Type',      UTF8Encode( self.FileExeType      ) );
      AStrings.AddPair( 'Company Name',       UTF8Encode( self.CompanyName      ) );
      AStrings.AddPair( 'File Description',   UTF8Encode( self.FileDescription  ) );
      AStrings.AddPair( 'File Version',       UTF8Encode( self.FileVersion      ) );
      AStrings.AddPair( 'Internal Name',      UTF8Encode( self.InternalName     ) );
      AStrings.AddPair( 'Legal CopyRight',    UTF8Encode( self.LegalCopyRight   ) );
      AStrings.AddPair( 'Legal TradeMarks',   UTF8Encode( self.LegalTradeMarks  ) );
      AStrings.AddPair( 'Original File Name', UTF8Encode( self.OriginalFileName ) );
      AStrings.AddPair( 'Product Name',       UTF8Encode( self.ProductName      ) );
      AStrings.AddPair( 'Product Version',    UTF8Encode( self.ProductVersion   ) );
      AStrings.AddPair( 'Comments',           UTF8Encode( self.Comments         ) );
      AStrings.AddPair( 'Special BuildStr',   UTF8Encode( self.SpecialBuildStr  ) );
      AStrings.AddPair( 'Private BuildStr',   UTF8Encode( self.PrivateBuildStr  ) );
   end else
      AStrings.AddPair( 'Version info', '--- not available'    );



end;

function TFileVersionInfo.ReadStdFileInfo(): boolean;
  var MyFd   : TWin32FindDataW;
begin
  Result := FindFirstFileW( PChar( Self.FileName ), MyFd{%H-} ) <> INVALID_HANDLE_VALUE;
  if Result then begin
     self.CreationTime   := xFileTimeToDateTime( MyFd.ftCreationTime   );
     self.LastAccessTime := xFileTimeToDateTime( MyFd.ftLastAccessTime );
     self.LastWriteTime  := xFileTimeToDateTime( MyFd.ftLastWriteTime  );

     self.FileAttributes := MyFd.dwFileAttributes;
     FileSize            := MyFd.nFileSizeLow or MyFd.nFileSizeHigh shl Cardinal(32);

     AlternateFileName   :=  MyFd.cAlternateFileName;
  end;
end;

procedure TFileVersionInfo.ReadFileVersionInfo(const AFileName: string);
  var rSHFI         : TSHFileInfoW;
      iRet          : Integer;
      VerSize       : Integer;
      VerBuf        : PChar;
      VerBufValue   : Pointer;
      VerHandle     : Cardinal;
      VerBufLen     : Cardinal;
      VerKey        : string;
      FixedFileInfo : PVSFixedFileInfo;

  // dwFileType, dwFileSubtype
  function GetFileSubType(FixedFileInfo: PVSFixedFileInfo): string;
  begin
     Result := '?';
     with FixedFileInfo^ do begin
        case dwFileType of
          VFT_UNKNOWN,
          VFT_APP,
          VFT_DLL,
          VFT_STATIC_LIB : Result := GetFileTypeStr( dwFileType ); //UnicodeString( GetFileTypeDict().Value( dwFileType ) );
          VFT_DRV        : Result := GetFileSubTypeStr( dwFileSubtype ); // UnicodeString( GetFileSubTypeDict().Value( dwFileSubtype ) );
          VFT_FONT       : Result := GetFontTypeStr( dwFileSubtype );  // UnicodeString( GetFontTypeDict().Value( dwFileSubtype ) );
          VFT_VXD        : Result := 'Virtual Device Identifier = ' +  UnicodeString( IntToHex( dwFileSubtype, 8 ) );
          else             Result := 'Unknown value';
        end;
     end;
  end;

  function HasdwFileFlags( FixedFileInfo: PVSFixedFileInfo; Flag: Word): Boolean;
  begin
     Result := (FixedFileInfo^.dwFileFlagsMask and FixedFileInfo^.dwFileFlags and Flag) = Flag;
  end;

  function GetFixedFileInfo: PVSFixedFileInfo;
  begin
     Result := Default(PVSFixedFileInfo);
     if not VerQueryValueW(VerBuf, '', Pointer(Result), VerBufLen)
        then Result := nil
  end;

  function GetInfo(const aKey: string): string;
  begin
     Result := '';
     VerKey := String( Format( '\StringFileInfo\%.4x%.4x\%s',
                               [ LoWord( Integer( VerBufValue^ ) ),
                                 HiWord( Integer( VerBufValue^ ) ), aKey
                               ]
                             )
                     );
     if VerQueryValueW( VerBuf, PChar( VerKey ), VerBufValue, VerBufLen )
        then Result := PChar(VerBufValue);
  end;

  function QueryValue(const aValue: string): string;
  begin
     Result := '';
     // obtain version information about the specified file
     if GetFileVersionInfoW( PChar( AFileName ), VerHandle, VerSize, VerBuf) and
        // return selected version information
        VerQueryValueW(VerBuf, '\VarFileInfo\Translation', VerBufValue, VerBufLen) then
        Result := GetInfo(aValue);
  end;

  function ExeType( const n: Cardinal ): string;
    type TExeType = record
            c0 : AnsiChar;
            c1 : AnsiChar;
            b2 : byte;
            b3 : byte;
         end;
    var x : TExeType;
  begin
     if n = 0 then Result := 'Nonexecutable file or an error condition.'
     else begin
        x := TExeType( n );
        if ((x.c0 = 'N') or (x.c0 = 'P')) and (x.c1 = 'E') and ((x.b2 <> 0) or (x.b3 <> 0))
           then Result := 'Windows application.'
           else if (x.c0 = 'M') and (x.c1 = 'Z') and (x.b2 = 0) and (x.b3 = 0)
                then Result := 'MS-DOS .exe or .com file '
                else if (x.c0 = 'P') and (x.c1 = 'E') and (x.b2 = 0) and (x.b3 = 0)
                     then Result := 'Console application or .bat file'
                     else Result := 'Unknown Exe type: 0x'+ String(IntToHex( n, 8 ));
     end;
  end;
begin

  // Initialize record
  self.Clear;
  self.FileName := AFileName;

  // Get the file type
  rSHFI := Default( TSHFileInfoW );
  if SHGetFileInfoW( PChar( AFileName ), 0, rSHFI, SizeOf(rSHFI), SHGFI_TYPENAME) <> 0 then begin
    self.FileType := rSHFI.szTypeName;
  end;

  iRet := SHGetFileInfoW( PChar( AFileName ), 0, rSHFI, SizeOf(rSHFI), SHGFI_EXETYPE );
  if iRet <> 0 then begin
     self.FileExeType := ExeType( iRet );
  end;
  // determine whether the OS can obtain version information
  VerHandle := Default( Cardinal );
  VerSize   := GetFileVersionInfoSizeW( PChar( AFileName ), VerHandle);
  if VerSize > 0 then begin
     VerBuf := AllocMem( VerSize );
     try
        with self do begin
          IsVersionInfo    := true;
          CompanyName      := QueryValue( 'CompanyName'      );
          FileDescription  := QueryValue( 'FileDescription'  );
          FileVersion      := QueryValue( 'FileVersion'      );
          InternalName     := QueryValue( 'InternalName'     );
          LegalCopyRight   := QueryValue( 'LegalCopyRight'   );
          LegalTradeMarks  := QueryValue( 'LegalTradeMarks'  );
          OriginalFileName := QueryValue( 'OriginalFileName' );
          ProductName      := QueryValue( 'ProductName'      );
          ProductVersion   := QueryValue( 'ProductVersion'   );
          Comments         := QueryValue( 'Comments'         );
          SpecialBuildStr  := QueryValue( 'SpecialBuild'     );
          PrivateBuildStr  := QueryValue( 'PrivateBuild'     );
          // Fill the VS_FIXEDFILEINFO structure
          FixedFileInfo    := GetFixedFileInfo;
          DebugBuild       := HasdwFileFlags( FixedFileInfo, VS_FF_DEBUG);
          PreRelease       := HasdwFileFlags( FixedFileInfo, VS_FF_PRERELEASE);
          PrivateBuild     := HasdwFileFlags( FixedFileInfo, VS_FF_PRIVATEBUILD);
          SpecialBuild     := HasdwFileFlags( FixedFileInfo, VS_FF_SPECIALBUILD);
          Patched          := HasdwFileFlags( FixedFileInfo, VS_FF_PATCHED);
          InfoInferred     := HasdwFileFlags( FixedFileInfo, VS_FF_INFOINFERRED);
          FileFunction     := GetFileSubType( FixedFileInfo );
        end;
      finally
        FreeMem( VerBuf, VerSize );
      end
  end;

  self.IsStdFileInfo := ReadStdFileInfo();

end;

function AttrWin32( const x: Cardinal ): string;
  //
  // https://docs.microsoft.com/en-us/windows/desktop/fileio/file-attribute-constants
  //
  const FILE_ATTRIBUTE_DEVICE                = $00000040;
  const FILE_ATTRIBUTE_INTEGRITY_STREAM      = $00008000;
  const FILE_ATTRIBUTE_NO_SCRUB_DATA         = $00020000;
  const FILE_ATTRIBUTE_RECALL_ON_OPEN        = $00040000;
  const FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS = $00400000;
  type trax = record
                atr    : Cardinal;
                letter : Char;
              end;
  const fAtr : array[ 1..19 ] of trax = (
             ( atr: FILE_ATTRIBUTE_READONLY;              letter : 'r' ),  // 15 r FILE_ATTRIBUTE_READONLY             = $00000001;
             ( atr: FILE_ATTRIBUTE_HIDDEN;                letter : 'h' ),  // 14 h FILE_ATTRIBUTE_HIDDEN               = $00000002;
             ( atr: FILE_ATTRIBUTE_SYSTEM;                letter : 's' ),  // 13 s FILE_ATTRIBUTE_SYSTEM               = $00000004;

             ( atr: FILE_ATTRIBUTE_DIRECTORY;             letter : 'D' ),  // 12 D FILE_ATTRIBUTE_DIRECTORY            = $00000010;
             ( atr: FILE_ATTRIBUTE_ARCHIVE;               letter : 'a' ),  // 11 a FILE_ATTRIBUTE_ARCHIVE              = $00000020;
             ( atr: FILE_ATTRIBUTE_DEVICE;                letter : 'd' ),  // 10 d FILE_ATTRIBUTE_DEVICE               = $00000040;
             ( atr: FILE_ATTRIBUTE_NORMAL;                letter : 'n' ),  // 09 n FILE_ATTRIBUTE_NORMAL               = $00000080;

             ( atr: FILE_ATTRIBUTE_TEMPORARY;             letter : 't' ),  // 08 t FILE_ATTRIBUTE_TEMPORARY            = $00000100;
             ( atr: FILE_ATTRIBUTE_SPARSE_FILE;           letter : 's' ),  // 09 s FILE_ATTRIBUTE_SPARSE_FILE          = $00000200;
             ( atr: FILE_ATTRIBUTE_REPARSE_POINT;         letter : 'J' ),  // 06 J FILE_ATTRIBUTE_REPARSE_POINT        = $00000400;
             ( atr: FILE_ATTRIBUTE_COMPRESSED;            letter : 'c' ),  // 05 c FILE_ATTRIBUTE_COMPRESSED           = $00000800;

             ( atr: FILE_ATTRIBUTE_OFFLINE;               letter : 'o' ),  // 04 o FILE_ATTRIBUTE_OFFLINE              = $00001000;
             ( atr: FILE_ATTRIBUTE_NOT_CONTENT_INDEXED;   letter : 'i' ),  // 03 i FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  = $00002000;
             ( atr: FILE_ATTRIBUTE_ENCRYPTED;             letter : 'e' ),  // 02 e FILE_ATTRIBUTE_ENCRYPTED            = $00004000;
             ( atr: FILE_ATTRIBUTE_INTEGRITY_STREAM;      letter : 'g' ),  //                                          = $00008000;

             ( atr: FILE_ATTRIBUTE_VIRTUAL;               letter : 'v' ),  // 01 v FILE_ATTRIBUTE_VIRTUAL              = $00010000;
             ( atr: FILE_ATTRIBUTE_NO_SCRUB_DATA;         letter : 'N' ),  //                                          = $00020000;
             ( atr: FILE_ATTRIBUTE_RECALL_ON_OPEN;        letter : 'C' ),  //                                          = $00040000;

             ( atr: FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS; letter : 'A' )  //                                           = $00400000;

         );
  var i : integer;
begin
   Result := '';
   for i := Low(fAtr) to High(fAtr) do
      if x and fAtr[i].atr <> 0
         then Result := Result + fAtr[i].letter
         else Result := Result + '-';
end;


function GetPEType(const APath: UnicodeString): TPEType;
  var hFile,
      hFileMap : THandle;
      PMapView : Pointer;
      PIDH     : PImageDosHeader;
      PINTH    : PImageNtHeaders;
      Base     : Pointer;
begin
   Result := pe_unknown;
   hFile := CreateFileW( PWideChar(APath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
   if hFile = INVALID_HANDLE_VALUE then begin
      CloseHandle( hFile );
      Exit;
   end;

   hFileMap  := CreateFileMapping( hFile, nil, PAGE_READONLY, 0, 0, nil );
   if hFileMap = 0 then begin
      CloseHandle( hFile );
      CloseHandle( hFileMap );
      Exit;
   end;

   PMapView := MapViewOfFile( hFileMap, FILE_MAP_READ, 0, 0, 0 );
   if PMapView = nil then begin
      CloseHandle( hFile );
      CloseHandle( hFileMap );
      Exit;
   end;

   PIDH := PImageDosHeader(PMapView);
   if PIDH^.e_magic <> IMAGE_DOS_SIGNATURE then begin
      CloseHandle( hFile );
      CloseHandle( hFileMap );
      UnmapViewOfFile( PMapView );
      Exit;
   end;

   Base  := PIDH;
   PINTH := PIMAGENTHEADERS( Base + LongWord(PIDH^.e_lfanew) );
   if PINTH^.Signature = IMAGE_NT_SIGNATURE then begin
      case PINTH^.OptionalHeader.Magic of
         $10b: Result := pe_32bit;
         $20b: Result := pe_64bit;
      end;
   end;

   CloseHandle( hFile );
   CloseHandle( hFileMap );
   UnmapViewOfFile( PMapView );
end;

function GetPEType( const APath : AnsiString) : TPEType;
begin
   Result := GetPEType( UnicodeString( APath ) );
end;

function GetPETypeStr( const APath: UnicodeString ): UnicodeString;
begin
   Result := cPEType[ GetPEType( APath ) ];
end;

function GetPETypeStr( const APath : AnsiString) : AnsiString;
begin
   Result := AnsiString( cPEType[ GetPEType( APath ) ] );
end;

end.
