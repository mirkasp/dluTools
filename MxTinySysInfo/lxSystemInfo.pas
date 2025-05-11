unit lxSystemInfo;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   {$modeswitch UNICODESTRINGS+}

   {$DEFINE dlu_Generics}
   {$DEFINE dlu_Unicode}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}
//
// http://delphiexamples.com/systeminfo/username.html
//

interface

type TuLocalization = ( ulEn, ulPl );

function GetUserName(): string;
function GetComputerName(): string;
function GetUserInfoShort(): string;

function GetMemoryInfo(const ABold: boolean = false; const ALng: TuLocalization = ulEn ): string;

implementation

//
// https://delphi.fandom.com/wiki/FreePascal_detection_and_versioning
//

uses Windows
   , SysUtils
{$IFDEF FPC}
  {$IFDEF UNIX}{$IFDEF UseCThreads}, cthreads {$ENDIF}{$ENDIF}
  //, Dialogs
  , Classes
  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
  //, fileinfo
  //, winpeimagereader {need this for reading exe info}
  //, elfreader        {needed for reading ELF executables}
  //, machoreader      {needed for reading MACH-O executables}
{$ELSE}
  , mkSysInfo
{$ENDIF}
  ;

type TuLangString = array[ TuLocalization ] of AnsiString;

const aLSTotal : TuLangString = ( 'total', 'razem' );
const aLSFree  : TuLangString = ( 'free',  'dostępne' );

const aBoldStart: array[ boolean ] of string = ( '', '<b>' );
const aBoldEnd  : array[ boolean ] of string = ( '', '</b>' );


{$IFDEF FPC}
type TMemoryStatusEx        = record
     dwLength               : DWORD;
     dwMemoryLoad           : DWORD;
     ullTotalPhys           : DWORDLONG;
     ullAvailPhys           : DWORDLONG;
     ullTotalPageFile       : DWORDLONG;
     ullAvailPageFile       : DWORDLONG;
     ullTotalVirtual        : DWORDLONG;
     ullAvailVirtual        : DWORDLONG;
     ullAvailExtendedVirtual: DWORDLONG;
end;

function GlobalMemoryStatusEx( var AParam:TMemoryStatusEx ): Boolean; stdcall; external 'Kernel32.dll' name 'GlobalMemoryStatusEx';
{$ENDIF}



function GetMemoryInfo(const ABold: boolean; const ALng: TuLocalization): string;
  var MS_Ex : TMemoryStatusEx;
begin
  MS_Ex.dwLength := SizeOf( TMemoryStatusEx );
  if GlobalMemoryStatusEx( MS_Ex ) then
     Result := String( Format( '%s%0.0n B%s %s, %s%0.0n B%s %s',
                               [ aBoldStart[ ABold ], 1.0 * MS_Ex.ullTotalPhys, aBoldEnd[ ABold ],
                                 aLSTotal[ aLng ],
                                 aBoldStart[ ABold ], 1.0 * MS_Ex.ullAvailPhys, aBoldEnd[ ABold ],
                                 aLSFree[ ALng ]
                               ] ) )
  else
      Result := UTF8Decode( SysErrorMessage( GetLastError() ) );

end;

function GetUserName(): string;
begin
   Result := SysUtils.GetEnvironmentVariable('USERNAME');
end;

function GetComputerName(): string;
begin
   Result := SysUtils.GetEnvironmentVariable('COMPUTERNAME');
end;

function GetUserInfoShort(): string;
begin
   Result := GetUserName() + '@' + GetComputerName();
end;


end.
