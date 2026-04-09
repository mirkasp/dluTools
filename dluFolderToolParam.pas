unit dluFolderToolParam;

{$I dluOptions.inc}

interface

uses Windows
   , Classes
   , Generics.Collections
   , dluSplitString
   ;

{$IFDEF FPC}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
type TWin32FindData = TWin32FindDataW;
type PWin32FindData = PWin32FindDataW;
{$ENDIF}

// Sygnatura metody-filtru przekazywanej z zewnątrz
type TFilterMethod = function( const AFindData: TWin32FindData ): boolean of object;

{ TuFolderToolParam }

type TuFolderToolParam = class
   strict private
      type TMaskMatchFunc  = function( const AFileName: PChar ): boolean of object;
      type TFilterProcList = {$IFDEF FPC}specialize {$ENDIF}TList<TFilterMethod>;

      var
         FFilterFuncs   : TFilterProcList;
         FHasMask       : boolean;      // czy zdefiniowano jakąkolwiek maskę?
         FFilePattern   : string;
         FPatternParts  : TUnicodeStringDynArray;
         FMaskMatchFunc : TMaskMatchFunc;
         FFreeOnRelease : boolean;

      procedure Initialize;
      procedure SetFilePattern( const AValue: string );
      function  GetIsEmptyMask: boolean; inline;

      // Strategie dopasowania maski — dobierane w SetFilePattern:
      function MatchPatternSingle( const AFileName: PChar ): boolean;       // pojedyncza maska
      function MatchPatternMulti( const AFileName: PChar ): boolean;        // wiele masek, Win7+
      function MatchPatternMultiLegacy( const AFileName: PChar ): boolean;  // wiele masek, pre-Win7

   public
      class function IsWin7OrAbove: boolean; static;

      constructor Create( const AFilePattern: string = '' );
      destructor  Destroy; override;

      procedure AddFilterProc( AProc: TFilterMethod );
      procedure ClearFilterProcs;

      // Zwraca TRUE jeśli wpis spełnia maskę ORAZ wszystkie zarejestrowane filtry
      function Matches( const AFindData: TWin32FindData ): boolean;

      // Jeśli TRUE, właściciel obiektu powinien go zwolnić po użyciu
      property FreeOnRelease : boolean read FFreeOnRelease write FFreeOnRelease;
      property FilePattern   : string  read FFilePattern;
      property IsEmptyMask   : boolean read GetIsEmptyMask;
end;

implementation

uses SysUtils;

// Flaga dla PathMatchSpecEx — zezwala na wzorce rozdzielane średnikiem
const PMSF_MULTIPLE = $00000001;

function PathMatchSpec(
   pszFile, pszSpec : PWideChar
): BOOL; stdcall; external 'shlwapi.dll' name 'PathMatchSpecW';

function PathMatchSpecEx(
   pszFile, pszSpec : PWideChar;
   dwFlags          : DWORD
): HRESULT; stdcall; external 'shlwapi.dll' name 'PathMatchSpecExW';

{ TuFolderToolParam }

class function TuFolderToolParam.IsWin7OrAbove: boolean;
begin
   Result := ( (Win32MajorVersion = 6) and (Win32MinorVersion >= 1) )
          or ( Win32MajorVersion >= 7 );
end;

constructor TuFolderToolParam.Create( const AFilePattern: string );
begin
   inherited Create;
   Initialize;
   SetFilePattern( AFilePattern );
end;

procedure TuFolderToolParam.Initialize;
begin
   FFilterFuncs   := TFilterProcList.Create;
   FFreeOnRelease := false;
   FHasMask       := false;
   FFilePattern   := '';
end;

destructor TuFolderToolParam.Destroy;
begin
   FFilterFuncs.Free;
   inherited;
end;

function TuFolderToolParam.GetIsEmptyMask: boolean;
begin
   Result := (FFilePattern = '')
          or (FFilePattern = '*')
          or (FFilePattern = '*.*');
end;

procedure TuFolderToolParam.SetFilePattern( const AValue: string );
begin
   FFilePattern := AValue;

   if GetIsEmptyMask then
      FPatternParts := nil
   else
      FPatternParts := dluSplitString.SplitString( AValue, ';' );

   FHasMask := Length( FPatternParts ) > 0;

   if Length( FPatternParts ) <= 1 then
      FMaskMatchFunc := {$IFDEF FPC}@{$ENDIF}MatchPatternSingle
   else if IsWin7OrAbove then
      FMaskMatchFunc := {$IFDEF FPC}@{$ENDIF}MatchPatternMulti
   else
      FMaskMatchFunc := {$IFDEF FPC}@{$ENDIF}MatchPatternMultiLegacy;
end;

function TuFolderToolParam.Matches( const AFindData: TWin32FindData ): boolean;
   var fp: TFilterMethod;
begin
   if FHasMask and Assigned( FMaskMatchFunc )
      then Result := FMaskMatchFunc( @AFindData.cFileName[0] )
      else Result := true;

   if not Result then
      Exit;

   // Wszystkie zarejestrowane filtry muszą zwrócić TRUE
   for fp in FFilterFuncs do
      if not fp( AFindData ) then
         Exit( false );
end;

procedure TuFolderToolParam.AddFilterProc( AProc: TFilterMethod );
begin
   if Assigned( AProc ) then
      FFilterFuncs.Add( AProc );
end;

procedure TuFolderToolParam.ClearFilterProcs;
begin
   FFilterFuncs.Clear;
end;

function TuFolderToolParam.MatchPatternSingle( const AFileName: PChar ): boolean;
begin
   Result := PathMatchSpec( AFileName, PChar( FFilePattern ) );
end;

function TuFolderToolParam.MatchPatternMulti( const AFileName: PChar ): boolean;
begin
   // PathMatchSpecEx obsługuje wzorce rozdzielane średnikiem natywnie (Win7+)
   Result := PathMatchSpecEx( AFileName, PChar( FFilePattern ), PMSF_MULTIPLE ) = S_OK;
end;

function TuFolderToolParam.MatchPatternMultiLegacy( const AFileName: PChar ): boolean;
   var i: integer;
begin
   Result := false;
   i      := Low( FPatternParts );
   while not Result and ( i <= High( FPatternParts ) ) do begin
      Result := PathMatchSpec( AFileName, PChar( FPatternParts[i] ) );
      Inc( i );
   end;
end;

end.
