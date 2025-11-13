unit dluFileLocator;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch UNICODESTRINGS+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}


interface

function LookForFile( const AFileName: String; const APathEnv: boolean; const AFolders: array of String ): String;
function SearchForFile( const ASearchPaths, AFileName: String; out VPath: String ): boolean;

implementation

uses SysUtils;

function LookForFile( const AFileName: String; const APathEnv: boolean; const AFolders: array of String ): String;
   var i : integer;
       sb: TUnicodeStringBuilder;
begin
   sb := TUnicodeStringBuilder.Create;
   try
      for i := 0 to High(AFolders) do
         sb.Append( IncludeTrailingPathDelimiter( AFolders[i] ) );

      if APathEnv then
         sb.Append(GetEnvironmentVariable('PATH'));

      if not SearchForFile( sb.ToString, AFileName, Result ) then
         Result := '';

   finally
      sb.Free;
   end;
end;

function SearchForFile( const ASearchPaths, AFileName: String; out VPath: String ): boolean;
begin
   VPath := SysUtils.FileSearch( AFileName, ASearchPaths );
   Result := VPath <> '';
end;

end .

