unit dluSysTools;

{$I dluOptions.inc}

interface

function OpenDocument( const ADocumentName : UnicodeString ): boolean; overload;
function OpenDocument( const ADocumentName : AnsiString    ): boolean; overload;

function xFormat( const AFormat: string; const AParams: array of const ): string;

implementation

uses SysUtils
  , Dialogs
  , Classes
{$IFDEF FPC}
  {$IFDEF UNIX}{$IFDEF UseCThreads}, cthreads {$ENDIF}{$ENDIF}
  , lclintf
  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
  //, fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
{$ELSE}
  , Windows
  , ShellApi
{$ENDIF}
  ;


function OpenDocument( const ADocumentName : UnicodeString ): boolean;
begin
{$IFDEF FPC}
   Result := OpenUrl( UTF8Encode( ADocumentName ) );
{$ELSE}
   Result := ShellExecute( GetDesktopWindow(),
                           'open',
                           PChar( ADocumentName ), nil, nil, SW_SHOWNORMAL) > 32;
{$ENDIF}
end;

function OpenDocument( const ADocumentName : AnsiString ): boolean;
begin
{$IFDEF FPC}
   Result := OpenUrl( ADocumentName );
{$ELSE}
   Result := ShellExecute( GetDesktopWindow(),
                           'open',
                           PChar( ADocumentName ), nil, nil, SW_SHOWNORMAL) > 32;
{$ENDIF}
end;

function xFormat( const AFormat: string; const AParams: array of const ): string;
begin
{$IFDEF FPC}
   Result := UnicodeFormat( AFormat, AParams );
{$ELSE}
   Result := Format( AFormat, AParams );
{$ENDIF}
end;



end.
