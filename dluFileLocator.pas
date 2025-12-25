unit dluFileLocator;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch UNICODESTRINGS+}
  {$inline on}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}


interface

function LookForFile( const AFileName: String; const APathEnv: boolean; const AFolders: array of String ): String;
function SearchForFile( const ASearchPaths, AFileName: String; out VPath: String ): boolean; inline;

implementation

uses SysUtils
   ;

function LookForFile(const AFileName: String; const APathEnv: boolean; const AFolders: array of String): String;
  var LFolder : string;
      sb      : TUnicodeStringBuilder;
begin
   // Optymalizacja: szybkie wyjście
   if AFileName = '' then Exit('');

   sb := TUnicodeStringBuilder.Create;
   try
      // Dodaj foldery z tablicy
      for LFolder in AFolders do begin
         if LFolder = '' then continue;
         if sb.Length > 0 then sb.Append( String(PathSeparator) );
         sb.Append( LFolder);
      end;

      // Dodaj PATH
      if APathEnv then begin
         if sb.Length > 0 then sb.Append( String(PathSeparator) );
         sb.Append( GetEnvironmentVariable('PATH') );
      end;

      Result := SysUtils.FileSearch( AFileName, sb.ToString );

   finally
      sb.Free;
   end;
end;

function SearchForFile( const ASearchPaths, AFileName: String; out VPath: String ): boolean; inline;
begin
   VPath  := SysUtils.FileSearch( AFileName, ASearchPaths );
   Result := VPath <> '';
end;

end .

