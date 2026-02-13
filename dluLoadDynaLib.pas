unit dluLoadDynaLib;

{$mode ObjFPC}{$H+}
{$IFDEF DEBUG}
  {$DEFINE ENABLE_LOGGING}
{$ENDIF}

interface

uses
  Windows, SysUtils;

type
  EDynamicLibraryError = class(Exception);

  TLoadLibraryFlags = set of (
    llfSecureLoad,        // Użyj bezpiecznego ładowania (KB2533623)
    llfSearchApplicationDir,
    llfSearchSystemDir,
    llfLogLoading         // Włącz logowanie (tylko w trybie DEBUG)
  );

const
  DefaultLoadFlags = [llfSecureLoad, llfSearchApplicationDir, llfSearchSystemDir];

// Główna funkcja - ładowanie biblioteki z obsługą błędów
function DynamicLoadLibrary(const ALibLocation: string;
                            AFlags: TLoadLibraryFlags = DefaultLoadFlags): THandle;

// Alternatywna wersja zgodna wstecz
function DynamicLoadLibraryExW(const ALibLocation: string): THandle;
  deprecated 'Use DynamicLoadLibrary instead';

implementation

uses
  {$IFDEF ENABLE_LOGGING}
  Classes,
  {$ENDIF}
  SyncObjs;

const
  KERNEL32 = 'kernel32.dll';
  LOAD_LIBRARY_SEARCH_DEFAULT_DIRS     = $00001000;
  LOAD_LIBRARY_SEARCH_USER_DIRS        = $00000400;
  LOAD_LIBRARY_SEARCH_APPLICATION_DIR  = $00000200;
  LOAD_LIBRARY_SEARCH_SYSTEM32         = $00000800;

type
  TSetDefaultDllDirectories = function(DirectoryFlags: DWORD): BOOL; stdcall;
  TAddDllDirectory          = function(NewDirectory: LPCWSTR): Pointer; stdcall;
  TRemoveDllDirectory       = function(Cookie: Pointer): BOOL; stdcall;
  TLoadLibraryExW           = function(lpLibFileName: LPCWSTR;
                                       hFile: THandle;
                                       dwFlags: DWORD): HMODULE; stdcall;

  TKernel32API = record
    SetDefaultDllDirectories: TSetDefaultDllDirectories;
    AddDllDirectory: TAddDllDirectory;
    RemoveDllDirectory: TRemoveDllDirectory;
    LoadLibraryExW: TLoadLibraryExW;
    Available: Boolean;
  end;

var
  Kernel32API: TKernel32API;
  InitLock: TCriticalSection;
  IsInitialized: Boolean = False;

{$IFDEF ENABLE_LOGGING}
var
  LogMessages: TStringList;
  LogLock: TCriticalSection;

procedure Log(const Msg: string);
begin
  if not Assigned(LogLock) then Exit;

  LogLock.Enter;
  try
    if Assigned(LogMessages) then
    begin
      LogMessages.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' - ' + Msg);
      OutputDebugStringW(PWideChar(UnicodeString(Msg)));
    end;
  finally
    LogLock.Leave;
  end;
end;

function GetLoadLog: string;
begin
  if not Assigned(LogLock) then Exit('');

  LogLock.Enter;
  try
    if Assigned(LogMessages) then
      Result := LogMessages.Text
    else
      Result := '';
  finally
    LogLock.Leave;
  end;
end;
{$ELSE}
procedure Log(const Msg: string); inline;
begin
  OutputDebugString( PChar( Msg ) );
end;
{$ENDIF}

// Szybsza wersja sprawdzania pełnej ścieżki
function IsFullPath(const S: string): Boolean; inline;
begin
  Result := (Length(S) >= 3) and
            (S[2] in AllowDriveSeparators) and
            (S[3] in AllowDirectorySeparators);
end;

// Inicjalizacja API Kernel32 z thread-safety
procedure InitializeKernel32API;
var
  hKernel: HMODULE;
begin
  // Double-checked locking pattern
  if IsInitialized then Exit;

  InitLock.Enter;
  try
    if IsInitialized then Exit;

    FillChar(Kernel32API, SizeOf(Kernel32API), 0);

    hKernel := GetModuleHandleW(KERNEL32);
    if hKernel = 0 then
    begin
      Log('WARNING: Could not get kernel32.dll handle');
      Exit;
    end;

    // Ładowanie funkcji (dostępne od Windows 8 / Server 2012 z KB2533623)
    Pointer(Kernel32API.SetDefaultDllDirectories) :=
      GetProcAddress(hKernel, 'SetDefaultDllDirectories');
    Pointer(Kernel32API.AddDllDirectory) :=
      GetProcAddress(hKernel, 'AddDllDirectory');
    Pointer(Kernel32API.RemoveDllDirectory) :=
      GetProcAddress(hKernel, 'RemoveDllDirectory');
    Pointer(Kernel32API.LoadLibraryExW) :=
      GetProcAddress(hKernel, 'LoadLibraryExW');

    // API dostępne tylko jeśli wszystkie funkcje są obecne
    Kernel32API.Available :=
      Assigned(Kernel32API.SetDefaultDllDirectories) and
      Assigned(Kernel32API.AddDllDirectory) and
      Assigned(Kernel32API.LoadLibraryExW);

    if Kernel32API.Available then
      Log('Extended DLL loading API available')
    else
      Log('Using legacy DLL loading (Windows 7 or older)');

    IsInitialized := True;
  finally
    InitLock.Leave;
  end;
end;

// Bezpieczne ładowanie z użyciem nowego API
function SecureLoadLibrary(const WidePath: UnicodeString;
                          const WideDir: UnicodeString;
                          AFlags: TLoadLibraryFlags): THandle;
var
  DirCookie: Pointer;
  LoadFlags: DWORD;
begin
  Result := 0;
  DirCookie := nil;
  LoadFlags := 0;

  try
    // Ustaw flagi ładowania
    if llfSearchApplicationDir in AFlags then
      LoadFlags := LoadFlags or LOAD_LIBRARY_SEARCH_APPLICATION_DIR;
    if llfSearchSystemDir in AFlags then
      LoadFlags := LoadFlags or LOAD_LIBRARY_SEARCH_SYSTEM32;

    LoadFlags := LoadFlags or LOAD_LIBRARY_SEARCH_USER_DIRS;

    // Ustaw domyślne katalogi (raz dla całej aplikacji)
    if not Kernel32API.SetDefaultDllDirectories(
         LOAD_LIBRARY_SEARCH_DEFAULT_DIRS or LoadFlags) then
    begin
      Log('WARNING: SetDefaultDllDirectories failed, code: ' +
          IntToStr(GetLastError));
    end;

    // Dodaj katalog biblioteki do ścieżki wyszukiwania
    if WideDir <> '' then
    begin
      DirCookie := Kernel32API.AddDllDirectory(PWideChar(WideDir));
      if DirCookie = nil then
        Log('WARNING: AddDllDirectory failed for: ' + UTF8Encode(WideDir));
    end;

    // Załaduj bibliotekę
    Result := Kernel32API.LoadLibraryExW(
      PWideChar(WidePath),
      0,
      LoadFlags
    );

    if Result <> 0 then
      Log('Loaded (secure): ' + UTF8Encode(WidePath))
    else
      Log('FAILED (secure): ' + UTF8Encode(WidePath) +
          ', error: ' + IntToStr(GetLastError));

  finally
    // Wyczyść dodany katalog
    if (DirCookie <> nil) and Assigned(Kernel32API.RemoveDllDirectory) then
      Kernel32API.RemoveDllDirectory(DirCookie);
  end;
end;

// Główna funkcja ładowania
function DynamicLoadLibrary(const ALibLocation: string;
                            AFlags: TLoadLibraryFlags = DefaultLoadFlags): THandle;
var
  WidePath: UnicodeString;
  WideDir: UnicodeString;
  FullPath: Boolean;
  LastErr: DWORD;
begin
  Result := 0;

  // Walidacja wejścia
  if ALibLocation = '' then
  begin
    Log('ERROR: Empty library location');
    raise EDynamicLibraryError.Create('Library location cannot be empty');
  end;

  // Inicjalizacja (jeśli potrzebna)
  InitializeKernel32API;

  // Konwersja ścieżki (raz)
  WidePath := UTF8Decode(ALibLocation);
  FullPath := IsFullPath(ALibLocation);

  if llfLogLoading in AFlags then
    Log('Loading library: ' + ALibLocation +
        ' (full path: ' + BoolToStr(FullPath, True) + ')');

  // Ścieżka względna - standardowe ładowanie
  if not FullPath then
  begin
    Result := LoadLibraryW(PWideChar(WidePath));
    if Result <> 0 then
      Log('Loaded (standard): ' + ALibLocation)
    else
    begin
      LastErr := GetLastError;
      Log('FAILED (standard): ' + ALibLocation +
          ', error: ' + IntToStr(LastErr));
      if llfSecureLoad in AFlags then
        raise EDynamicLibraryError.CreateFmt(
          'Failed to load library "%s" (error %d)',
          [ALibLocation, LastErr]);
    end;
    Exit;
  end;

  // Pełna ścieżka - użyj bezpiecznego API jeśli dostępne
  if Kernel32API.Available and (llfSecureLoad in AFlags) then
  begin
    WideDir := UTF8Decode(ExtractFileDir(ALibLocation));
    Result := SecureLoadLibrary(WidePath, WideDir, AFlags);
  end
  else
  begin
    // Fallback dla starszych systemów
    Result := LoadLibraryW(PWideChar(WidePath));
    if Result <> 0 then
      Log('Loaded (legacy): ' + ALibLocation)
    else
    begin
      LastErr := GetLastError;
      Log('FAILED (legacy): ' + ALibLocation +
          ', error: ' + IntToStr(LastErr));
      if llfSecureLoad in AFlags then
        raise EDynamicLibraryError.CreateFmt(
          'Failed to load library "%s" (error %d)',
          [ALibLocation, LastErr]);
    end;
  end;
end;

// Kompatybilność wstecz
function DynamicLoadLibraryExW(const ALibLocation: string): THandle;
begin
  Result := DynamicLoadLibrary(ALibLocation, DefaultLoadFlags);
end;

initialization
  InitLock := TCriticalSection.Create;
  {$IFDEF ENABLE_LOGGING}
  LogLock := TCriticalSection.Create;
  LogMessages := TStringList.Create;
  {$ENDIF}

finalization
  {$IFDEF ENABLE_LOGGING}
  FreeAndNil(LogMessages);
  FreeAndNil(LogLock);
  {$ENDIF}
  FreeAndNil(InitLock);

end.
