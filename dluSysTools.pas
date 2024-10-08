unit dluSysTools;

{$I dluOptions.inc}

interface

function OpenDocument( const ADocumentName : UnicodeString ): boolean; overload;
function OpenDocument( const ADocumentName : AnsiString    ): boolean; overload;

function xFormat( const AFormat: string; const AParams: array of const ): string;

function ConvertBytes(const ABytes: Int64): UnicodeString;

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


const DTSD = 1024;
const aDescription: array[ 1..6 ] of UnicodeString = ( 'Kb', 'Mb', 'Gb', 'Tb', 'Pb', 'Eb' );
const aPowers     : array[ 1..6 ] of Int64  = ( {Kb} DTSD,
                                                {Mb} DTSD * DTSD,
                                                {Gb} DTSD * DTSD * DTSD,
                                                {Tb} DTSD * DTSD * DTSD * 1014,
                                                {Pb} DTSD * DTSD * DTSD * DTSD * DTSD,
                                                {Eb} DTSD * DTSD * DTSD * DTSD * DTSD * DTSD );

function ConvertBytes(const ABytes: Int64): UnicodeString;
  var i: Integer;
begin
   Result := UnicodeFormat('%.0n b',[ ABytes + 0.1 - 0.1 ] );
   if ABytes >= DTSD then begin
      i := Low( aPowers );
      while (ABytes > aPowers[ i+1 ]) and (i < High(aPowers)) do Inc(i);
      Result := UTF8Decode( FormatFloat('###0.##', ABytes / aPowers[ i ] ) ) + ' ' + aDescription[ i ] +
                ' ('+Result+')';
   end;
end;



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
