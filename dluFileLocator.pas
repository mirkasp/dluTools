unit dluFileLocator;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch UNICODESTRINGS+}
  {$inline on}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Classes;

function LookForFile( const AFileName: String; const APathEnv: boolean; const AFolders: array of String ): String;
function SearchForFile( const ASearchPaths, AFileName: String; out VPath: String ): boolean; inline;

type TFileLocatorOption = (fl_current, fl_syspath);
type TFileLocatorOptions = set of TFileLocatorOption;

procedure FoldersWithFile( const AFileName: string; var FindList: TStringList; const Folders: array of AnsiString; const Options: TFileLocatorOptions );

implementation

uses SysUtils;

// ============================================================================
// Funkcja pomocnicza: buduje ścieżkę przeszukiwania
// ============================================================================
function BuildSearchPath(const AFolders: array of String; const IncludePATH: boolean): String;
  var LFolder: string;
      sb     : TStringBuilder;
      i      : Integer;
      PathEnv: string;
begin
   // Wczesne wyjście dla najprostszego przypadku
   if (Length(AFolders) = 0) and not IncludePATH then
      Exit('');

   sb := TStringBuilder.Create;
   try
      // Dodaj foldery z tablicy (pomijając puste)
      for i := Low(AFolders) to High(AFolders) do begin
          LFolder := Trim(AFolders[i]);
          if LFolder = '' then continue;

          if sb.Length > 0 then
             sb.Append(PathSeparator);
          sb.Append(LFolder);
      end;

      // Dodaj PATH jeśli wymagane
      if IncludePATH then begin
         PathEnv := GetEnvironmentVariable('PATH');
         if PathEnv <> '' then begin
            if sb.Length > 0 then
               sb.Append(PathSeparator);
            sb.Append(PathEnv);
         end;
      end;

      Result := sb.ToString;
   finally
     sb.Free;
   end;
end;

// ============================================================================
// Główna funkcja: szuka pliku w podanych folderach i opcjonalnie w PATH
// ============================================================================
function LookForFile(const AFileName: String; const APathEnv: boolean; const AFolders: array of String): String;
  var SearchPath: String;
begin
   // Walidacja wejścia
   if AFileName = '' then
      Exit('');

   // Optymalizacja: sprawdź czy plik istnieje w bieżącym katalogu
   if FileExists(AFileName) then
      Exit(ExpandFileName(AFileName));

   // Buduj ścieżkę przeszukiwania
   SearchPath := BuildSearchPath(AFolders, APathEnv);

   // Wyszukaj plik
   if SearchPath <> '' then
      Result := FileSearch(AFileName, SearchPath)
   else
      Result := '';
end;

// ============================================================================
// Wrapper do FileSearch z bardziej czytelną sygnaturą
// ============================================================================
function SearchForFile( const ASearchPaths, AFileName: String; out VPath: String ): boolean; inline;
begin
   VPath := '';

   if (AFileName = '') or (ASearchPaths = '') then
      Exit(False);

   VPath  := FileSearch(AFileName, ASearchPaths);
   Result := VPath <> '';
end;

// ============================================================================
// Znajduje wszystkie foldery zawierające dany plik
// ============================================================================
procedure FoldersWithFile( const AFileName: string;
                           var FindList: TStringList;
                           const Folders: array of AnsiString;
                           const Options: TFileLocatorOptions);
  var CheckedFolders: TStringList;
      CurrentDir    : AnsiString;
      FolderPath    : AnsiString;
      FilePath      : AnsiString;
      PathEnvFolders: TStringList;
      i             : Integer;

  // Funkcja pomocnicza: sprawdza i dodaje folder jeśli zawiera plik
  procedure CheckAndAddFolder(const AFolder: AnsiString);
    var NormalizedFolder: AnsiString;
  begin
     if AFolder = '' then Exit;

     NormalizedFolder := IncludeTrailingPathDelimiter( ExpandFileName(String(AFolder) ) );

     // Sprawdź czy folder był już sprawdzony (unikaj duplikatów)
     if CheckedFolders.IndexOf(String(NormalizedFolder)) >= 0 then
        Exit;

     CheckedFolders.Add(String(NormalizedFolder));

     // Sprawdź czy plik istnieje
     FilePath := NormalizedFolder + AFileName;
     if FileExists(String(FilePath)) then
        FindList.Add(String(NormalizedFolder));
  end;

begin
   // Walidacja wejścia
   if (AFileName = '') or not Assigned(FindList) then
      Exit;

   FindList.Clear;
   CheckedFolders := TStringList.Create;
   try
      CheckedFolders.Sorted := True;
      CheckedFolders.Duplicates := dupIgnore;

      // 1. Sprawdź bieżący katalog
      if fl_current in Options then begin
         CurrentDir := AnsiString(ExpandFileName('.'));
         CheckAndAddFolder(CurrentDir);
      end;

      // 2. Sprawdź podane foldery
      for i := Low(Folders) to High(Folders) do
         CheckAndAddFolder(Folders[i]);

      // 3. Sprawdź foldery z PATH
      if fl_syspath in Options then begin
         PathEnvFolders := TStringList.Create;
         try
            PathEnvFolders.Delimiter := PathSeparator;
            PathEnvFolders.StrictDelimiter := True;
            PathEnvFolders.DelimitedText := GetEnvironmentVariable('PATH');

            for i := 0 to PathEnvFolders.Count - 1 do
               CheckAndAddFolder(AnsiString(PathEnvFolders[i]));

         finally
            PathEnvFolders.Free;
         end;
      end;

   finally
      CheckedFolders.Free;
   end;
end;

end.
