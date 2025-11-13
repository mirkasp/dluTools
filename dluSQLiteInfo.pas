unit dluSQLiteInfo;

{$mode ObjFPC}{$H+}
{$modeswitch UNICODESTRINGS+}
{$IFNDEF FPC}{$WARN ONLY FOR LAZARUS!}{$ENDIF}

interface

{ TuSQLiteInfo }
type TuSQLiteInfo = class
   strict private
      fSQLiteEng        : UnicodeString;
      fSQLiteEngInt     : UnicodeString;
      fSQLiteEngVer     : UnicodeString;
      fSQLiteEngBitness : UnicodeString;
      //
      fSQLiteDbFile     : UnicodeString;
      //
      function GetSQLiteEngineInfo: UnicodeString;
      procedure PrepareSQLiteEngInfo();
      procedure SetSQLiteDbFile( const AValue: string );
      procedure Initialize( const ASQLiteFileName: UnicodeString );
   public
      //
      constructor Create( const ASQLiteFileName: AnsiString = '' ); overload;
      constructor Create( const ASQLiteFileName: UnicodeString = '' ); overload;
      destructor Destroy; override;
      //
      property SQLiteEng        : string  read fSQLiteEngInt;
      property SQLiteEngVer     : string  read fSQLiteEngVer;
      property SQLiteEngBitness : string  read fSQLiteEngBitness;
      property SQLiteEngineInfo : string  read GetSQLiteEngineInfo;
      //
      property SQLiteDbFile     : string  read fSQLiteDbFile write SetSQLiteDbFile;
end;

function GetSQLiteEngineInfo( const ASQLiteFileName: AnsiString = '' ): AnsiString; overload;
function GetSQLiteEngineInfo( const ASQLiteFileName: UnicodeString = '' ): UnicodeString; overload;

implementation

uses SysUtils
   , dluFileInfo
   , dluFileLocator
   ;

const SQLiteEngDefaultName = 'sqlite3.dll';

function GetSQLiteEngineInfo( const ASQLiteFileName: AnsiString ): AnsiString;
begin
   with TuSQLiteInfo.Create( ASQLiteFileName ) do begin
      Result := AnsiString( SQLiteEngineInfo );
      Free;
   end;
end;

function GetSQLiteEngineInfo( const ASQLiteFileName: UnicodeString = '' ): UnicodeString;
begin
   with TuSQLiteInfo.Create( ASQLiteFileName ) do begin
      Result := SQLiteEngineInfo;
      Free;
   end;
end;


{ TuSQLiteInfo }

constructor TuSQLiteInfo.Create(const ASQLiteFileName: AnsiString);
begin
   inherited Create;
   Initialize( UnicodeString( ASQLiteFileName ) );
end;

constructor TuSQLiteInfo.Create(const ASQLiteFileName: UnicodeString);
begin
   inherited Create;
   Initialize( ASQLiteFileName );
end;

procedure TuSQLiteInfo.Initialize(const ASQLiteFileName: UnicodeString);
begin
   fSQLiteEngBitness := '';
   fSQLiteEngInt     := '';

   fSQLiteEng        := ASQLiteFileName;
   if fSQLiteEng <> '' then begin
      fSQLiteEng := ExpandFileName( fSQLiteEng );
      if FileExists( fSQLiteEng ) then fSQLiteEngInt := fSQLiteEng;
   end;

   PrepareSQLiteEngInfo();

   fSQLiteDbFile := '';
end;

destructor TuSQLiteInfo.Destroy;
begin
   inherited Destroy;
end;

procedure TuSQLiteInfo.PrepareSQLiteEngInfo;
  //var s : UnicodeString;
begin
   if fSQLiteEngInt = '' then begin
      if FileExists( SQLiteEngDefaultName ) then begin
         fSQLiteEngInt := ExtractFilePath( ParamStr(0) ) + SQLiteEngDefaultName;
      end else begin
         //if SearchForFile( GetEnvironmentVariable( UnicodeString('PATH') ), SQLiteEngDefaultName, s ) then begin
         //   fSQLiteEngInt := s;
         //end;
         fSQLiteEngInt := LookForFile( SQLiteEngDefaultName, true, [] );
      end;
   end;

   if fSQLiteEngInt <> '' then begin
      with dluFileInfo.GetFileInfo( UnicodeString( fSQLiteEngInt ) ) do begin
         fSQLiteEngVer := FileVersion;
      end;
      fSQLiteEngBitness := GetPETypeStr( WideString( fSQLiteEngInt ) );
   end;

end;

function TuSQLiteInfo.GetSQLiteEngineInfo: string;
begin
   Result := SQLiteEngVer + ' (' + fSQLiteEngBitness + ')';
end;

procedure TuSQLiteInfo.SetSQLiteDbFile( const AValue: string );
begin
  if fSQLiteDbFile = AValue
     then exit;
  fSQLiteDbFile := AValue;
end;

end.

