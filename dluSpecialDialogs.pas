unit dluSpecialDialogs;

{$I dluOptions.inc}

interface

type

  TWindowsDialogs = ( wdStartMenu,
                      wdRecycleBin,
                      wdRunFile,
                      wdUserManager,
                      wdWindowsAboutDlg,
                      wdWindowsShutdownDialog,
                      wdScreenSaver,
                      wdControlPanel,
                      wdSystemProperties,
                      wdSystemPropertiesAdvanced,
                      wdDisplayProperties,
                      wdThemesProperties,
                      wdPasswordsProperties,
                      wdPowerManagementProperties,
                      wdDateTimeProperties,
                      wdAccessibilityProperties,
                      wdSoftwareProperties,
                      wdDialProperties,
                      wdFontsProperties,
                      wdLocalRegionProperties,
                      wdPrinterProperties,
                      wdMouseProperties,
                      wdKeyboardProperties,
                      wdNetworkProperties,
                      wdInternetProperties,
                      wdModemProperties,
                      wdMultimediaProperties,
                      wdMailProperties,
                      wdScanCamProperties,
                      wdODBCProperties,
                      wdBDEProperties
                    );

procedure ShowWindowsSpecialDialog( const SpecialDialog: TWindowsDialogs; const AsAdmin: boolean = false );

implementation

uses Windows
   , Dialogs
   , SysUtils
   , ShellAPI
{$IFNDEF FPC}
   , ActiveX
   , ShlObj
   , Messages
{$ENDIF}
   ;

const shell32 = 'SHELL32.DLL';

{$IFDEF UNICODE}
const sShowWinAbout = 'ShellAboutW';
type PAplChar  = PWideChar;
type AplString = UnicodeString;
type TAPP_ShellExecuteInfo = ShellApi.SHELLEXECUTEINFOW;
type PAPP_ShellExecuteInfo = ^TAPP_ShellExecuteInfo;
function ShellExecuteEx( lpExecInfo: PAPP_ShellExecuteInfo ): Bool; external shell32 name 'ShellExecuteExW';
{$ELSE}
const sShowWinAbout = 'ShellAboutA';
type PAplChar  = PAnsiChar;
type AplString = AnsiString;
type TAPP_ShellExecuteInfo = ShellApi.SHELLEXECUTEINFOA;
type PAPP_ShellExecuteInfo = ^TAPP_ShellExecuteInfo;
function ShellExecuteEx( lpExecInfo: PAPP_ShellExecuteInfo ): Bool; external shell32 name 'ShellExecuteExA';
{$ENDIF}

//-----------Windows API functions to call windows dialogs-----------//

procedure RunFileDlg( OwnerWnd        : HWND;
                      Icon            : HICON;
                      lpstrDirectory  : PAplChar;
                      lpstrTitle      : PAplChar;
                      lpstrDescription: PAplChar;
                      Flags           : Longint ); stdcall; external shell32 Index 61;

function ShowWinAbout( hwndOwner       : HWND;
                       lpszApp         : PAplChar;
                       lpszOther       : PAplChar;
                       hIcon           : HICON ): DWORD; stdcall; external shell32 name sShowWinAbout; // 'ShellAboutA';
//--------------------------------------------------//

function OpenRecycleBin: Boolean;
  var recycleBinPIDL : PItemIDList = nil;
      exInfo         : TAPP_ShellExecuteInfo;
begin
   SHGetSpecialFolderLocation( 0, CSIDL_BITBUCKET, recycleBinPIDL ) ;
   exInfo := Default( TAPP_ShellExecuteInfo );
   //FillChar( exInfo, SizeOf(exInfo), 0 );
   with exInfo do begin
     cbSize   := Sizeof( exInfo );
     fMask    := SEE_MASK_IDLIST;
     nShow    := SW_SHOWNORMAL;
     lpVerb   := 'open';
     lpIDList := recycleBinPIDL;
   end;
   Result := ShellExecuteEx( @exInfo ) ;
  if not Result then RaiseLastOSError;
end;

const
   RFF_NOOPT         =    0;
   //RFF_NOBROWSE      =    1;
   //RFF_NODEFAULT     =    2;
   //RFF_CALCDIRECTORY =    4;
   //RFF_NOLABEL       =    8;
   //RFF_NOSEPARATEMEM =   14;

const sOperation : array[ boolean ] of AplString = ( 'open', 'runas' );

function ShowCustomRunDialog( OwnerWnd : HWND;
                              InitialDir, Title, Description: PAplChar;
                              flags: Integer): Boolean;
begin
   try
      RunFileDlg( OwnerWnd, 0, InitialDir, Title, Description, Flags );
      Result := TRUE;
   finally
   end;
end;

procedure OpenCPLDialogs( const cpl: AplString; const AsAdmin: boolean = false );
begin
   ShellExecute( GetCurrentProcess,
                 PAplChar( sOperation[ AsAdmin ] ),
                 'rundll32.exe',
                 PAplChar( 'shell32.dll, Control_RunDLL ' + cpl),
                 nil, sw_shownormal );
end;

procedure OpenEnvironmentVariables( const AsAdmin: boolean );
begin
   ShellExecute( GetCurrentProcess,
                 PAplChar( sOperation[ AsAdmin ] ),
                 'rundll32.exe',
                 'sysdm.cpl, EditEnvironmentVariables',
                 nil, sw_shownormal );
end;




procedure ShowWindowsSpecialDialog(const SpecialDialog: TWindowsDialogs; const AsAdmin: boolean );
begin
  case SpecialDialog of
    wdStartMenu: begin
       keybd_event(VK_LWIN, MapVirtualKey(VK_LWIN, 0), 0, 0);
       keybd_event(VK_LWIN, MapVirtualKey(VK_LWIN, 0), KEYEVENTF_KEYUP, 0)
    end;
    wdRecycleBin: begin
       OpenRecycleBin;
    end;
    wdRunFile: begin
       ShowCustomRunDialog( FindWindow('Shell_TrayWnd', NIL), NIL, NIL, NIL, RFF_NOOPT );
    end;
    wdUserManager: begin
       ShellExecute(0, 'open', 'musrmgr.exe', '', '', SW_SHOW);
    end;
    wdWindowsAboutDlg: begin
       ShowWinAbout( 0, nil, nil, 0);
    end;
    wdWindowsShutdownDialog:begin
       PostMessage( FindWindow('Progman', nil), WM_CLOSE, 0, 0);
    end;
    wdScreenSaver: begin
       SendMessage( GetDesktopWindow, WM_SYSCOMMAND, SC_SCREENSAVE, 0 );
    end;
    wdControlPanel: begin
       OpenCPLDialogs( '' );
    end;
    wdSystemProperties: begin
       OpenCPLDialogs('SYSDM.CPL');
    end;
    wdSystemPropertiesAdvanced: begin
       OpenEnvironmentVariables( AsAdmin );
    end;
    wdDisplayProperties: begin
       OpenCPLDialogs('DESK.CPL');
    end;
    wdThemesProperties: begin
       OpenCPLDialogs('THEMES.CPL');
    end;
    wdPasswordsProperties: begin
       OpenCPLDialogs('PASSWORD.CPL');
    end;
    wdPowerManagementProperties: begin
       OpenCPLDialogs('POWERCFG.CPL');
    end;
    wdDateTimeProperties: begin
       ShellExecute( GetCurrentProcess, 'open', 'control', 'date/time', nil, SW_SHOW)
    end;
    wdAccessibilityProperties: begin
       OpenCPLDialogs('ACCESS.CPL');
    end;
    wdSoftwareProperties: begin
       OpenCPLDialogs('APPWIZ.CPL');
    end;
    wdDialProperties: begin
       OpenCPLDialogs('TELEPHON.CPL');
    end;
    wdFontsProperties: begin
       OpenCPLDialogs('MAIN.CPL @3');
    end;
    wdLocalRegionProperties: begin
       OpenCPLDialogs('INTL.CPL');
    end;
    wdPrinterProperties: begin
       OpenCPLDialogs('MAIN.CPL @2');
    end;
    wdMouseProperties: begin
       OpenCPLDialogs('MAIN.CPL @0');
    end;
    wdKeyboardProperties: begin
       OpenCPLDialogs('MAIN.CPL @1');
    end;
    wdNetworkProperties: begin
       OpenCPLDialogs('NETCPL.CPL');
    end;
    wdInternetProperties: begin
       OpenCPLDialogs('INETCPL.CPL');
    end;
    wdModemProperties: begin
       OpenCPLDialogs('MODEM.CPL');
    end;
    wdMultimediaProperties: begin
       OpenCPLDialogs('MMSYS.CPL');
    end;
    wdMailProperties: begin
       OpenCPLDialogs('MLCFG32.CPL');
    end;
    wdScanCamProperties: begin
       OpenCPLDialogs('STICPL.CPL');
    end;
    wdODBCProperties: begin
       OpenCPLDialogs('ODBCCP32.CPL');
    end;
    wdBDEProperties: begin
       OpenCPLDialogs('BDEADMIN.CPL');
    end;
  end;
end;

end.
