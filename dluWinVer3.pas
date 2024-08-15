unit dluWinVer3;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

{$IFNDEF FPC}
   {$IF RTLVersion < 18}
      {$MESSAGE Warn 'Not tested on Delphi versions before 2007!'}
   {$IFEND}
{$ENDIF}

interface

uses Classes, dluLoadLibrary;

type

{ TWinVerSpec }

 TWinVerSpec = class
   strict private
     fAllProperties        : TStrings;
     //
     fName                 : WideString;
     fTechnicalInfo        : WideString;
     fPlatformId           : Cardinal;
     fMajorVersion         : Cardinal;
     fMinorVersion         : Cardinal;
     fBuildNumber          : Cardinal;
     fCSDVersion           : WideString;
     fServicePackMajor     : Word;
     fServicePackMinor     : Word;
     fSuiteMask            : Word;
     fProcessorArchitecture: Word;
     fProductType          : Byte;
     fProductInfo          : Cardinal;
     fMediaCenter          : boolean;
     fTabletPC             : boolean;
     fSuites               : TStrings;
     fLang                 : WideString;
     //
     fBuildLab           : string;        // TRegistry_ReadString(Reg, 'BuildLab');
     fBuildLabEx         : string;        // TRegistry_ReadString(Reg, 'BuildLabEx');
     fCSDBuildNumber     : string;        // TRegistry_ReadString(Reg, 'CSDBuildNumber');
     fCSDVersionReg      : string;        // TRegistry_ReadString(Reg, 'CSDVersion');
     fCurrentBuildNumber : string;        // TRegistry_ReadString(Reg, 'CurrentBuildNumber');
     fCurrentVersion     : string;        // TRegistry_ReadString(Reg, 'CurrentVersion');
     fEditionId          : string;        // TRegistry_ReadString(Reg, 'EditionId');
     fProductName        : string;        // TRegistry_ReadString(Reg, 'ProductName');
     fReleaseId          : string;        // TRegistry_ReadString(Reg, 'ReleaseId');
     fUBR                : integer;       // TRegistry_ReadInteger(Reg, 'UBR');
     fInstallDate        : TDateTime;

     procedure AppendToName( const AText: AnsiString ); overload;
     procedure AppendToName( const AText: WideString ); overload;
     procedure AppendToNameIf( const ACond: boolean; const AText_true, AText_false: WideString );
     procedure Prepare_Win32s_Platform;
     procedure Prepare_Win32_Windows_Platform;
     procedure Prepare_Win32_NT_Platform;
     procedure TryReadFromRegistry;
     function GetCompilationInfo: WideString;
     function GetWindowsRelease: UnicodeString;
   public
     constructor Create;
     destructor Destroy; override;
     //
     property AllProperties        : TStrings   read fAllProperties;
     //
     property TechnicalInfo        : WideString read fTechnicalInfo;
     property CompilationInfo      : WideString read GetCompilationInfo;
     //
     property Name                 : WideString read fName;
     property PlatformId           : Cardinal   read fPlatformId;
     property MajorVersion         : Cardinal   read fMajorVersion;
     property MinorVersion         : Cardinal   read fMinorVersion;
     property BuildNumber          : Cardinal   read fBuildNumber;
     property CSDVersion           : WideString read fCSDVersion;
     property ServicePackMajor     : Word       read fServicePackMajor;
     property ServicePackMinor     : Word       read fServicePackMinor;
     property SuiteMask            : Word       read fSuiteMask;
     property ProcessorArchitecture: Word       read fProcessorArchitecture;
     property ProductType          : Byte       read fProductType;
     property ProductInfo          : Cardinal   read fProductInfo;
     property MediaCenter          : boolean    read fMediaCenter;
     property TabletPC             : boolean    read fTabletPC;
     property Suites               : TStrings   read fSuites;
     property Lang                 : WideString read fLang;
     //
     property BuildLab             : string     read fBuildLab;
     property BuildLabEx           : string     read fBuildLabEx;
     property CSDBuildNumber       : string     read fCSDBuildNumber;
     property CSDVersionReg        : string     read fCSDVersionReg;
     property CurrentBuildNumber   : string     read fCurrentBuildNumber;
     property CurrentVersion       : string     read fCurrentVersion;
     property EditionId            : string     read fEditionId;
     property ProductName          : string     read fProductName;
     property ReleaseId            : string     read fReleaseId;
     property UBR                  : integer    read fUBR;
     property InstallDate          : TDateTime  read fInstallDate;
     //
     property WindowsRelease       : UnicodeString read GetWindowsRelease;

end;


var AppWinVer : TWinVerSpec;

implementation

uses Windows, SysUtils, Registry, dluDictionary;

const cLib_Kernel32 = 'Kernel32.dll';
const cLib_Ntdll    = 'ntdll.dll';

const VER_NT_WORKSTATION                          = 1;
const VER_NT_DOMAIN_CONTROLLER                    = 2;
const VER_NT_SERVER                               = 3;

// ProcessorArchitecture
const PROCESSOR_ARCHITECTURE_INTEL                = $0000;    // x86
const PROCESSOR_ARCHITECTURE_ARM                  = $0005;    // ARM
const PROCESSOR_ARCHITECTURE_IA64                 = $0006;    // Intel Itanium-based
const PROCESSOR_ARCHITECTURE_AMD64                = $0009;    // x64 (AMD or Intel)
const PROCESSOR_ARCHITECTURE_ARM64                = $000C;    // ARM64
const PROCESSOR_ARCHITECTURE_UNKNOWN              = $FFFF;    // Unknown architecture.

// Suite mask (wStiteMask)
const VER_SUITE_SMALLBUSINESS                     = $00000001;
const VER_SUITE_ENTERPRISE                        = $00000002;
const VER_SUITE_BACKOFFICE                        = $00000004;
const VER_SUITE_COMMUNICATIONS                    = $00000008;
const VER_SUITE_TERMINAL                          = $00000010;
const VER_SUITE_SMALLBUSINESS_RESTRICTED          = $00000020;
const VER_SUITE_EMBEDDEDNT                        = $00000040;
const VER_SUITE_DATACENTER                        = $00000080;
const VER_SUITE_SINGLEUSERTS                      = $00000100;
const VER_SUITE_PERSONAL                          = $00000200;
const VER_SUITE_BLADE                             = $00000400;
const VER_SUITE_EMBEDDED_RESTRICTED               = $00000800;
const VER_SUITE_SECURITY_APPLIANCE                = $00001000;
const VER_SUITE_STORAGE_SERVER                    = $00002000;
const VER_SUITE_COMPUTE_SERVER                    = $00004000;
const VER_SUITE_WH_SERVER                         = $00008000;
const VER_SUITE_MULTIUSERTS                       = $00020000;

// ProductInfo constans
const PRODUCT_UNDEFINED                           = $00000000;
const PRODUCT_ULTIMATE                            = $00000001;
const PRODUCT_HOME_BASIC                          = $00000002;
const PRODUCT_HOME_PREMIUM                        = $00000003;
const PRODUCT_ENTERPRISE                          = $00000004;
const PRODUCT_HOME_BASIC_N                        = $00000005;
const PRODUCT_BUSINESS                            = $00000006;  { Business }
const PRODUCT_STANDARD_SERVER                     = $00000007;
const PRODUCT_DATACENTER_SERVER                   = $00000008;
const PRODUCT_SMALLBUSINESS_SERVER                = $00000009;
const PRODUCT_ENTERPRISE_SERVER                   = $0000000A;
const PRODUCT_STARTER                             = $0000000B;
const PRODUCT_DATACENTER_SERVER_CORE              = $0000000C;
const PRODUCT_STANDARD_SERVER_CORE                = $0000000D;
const PRODUCT_ENTERPRISE_SERVER_CORE              = $0000000E;
const PRODUCT_ENTERPRISE_SERVER_IA64              = $0000000F;
const PRODUCT_BUSINESS_N                          = $00000010;
const PRODUCT_WEB_SERVER                          = $00000011;
const PRODUCT_CLUSTER_SERVER                      = $00000012;
const PRODUCT_HOME_SERVER                         = $00000013;
const PRODUCT_STORAGE_EXPRESS_SERVER              = $00000014;
const PRODUCT_STORAGE_STANDARD_SERVER             = $00000015;
const PRODUCT_STORAGE_WORKGROUP_SERVER            = $00000016;
const PRODUCT_STORAGE_ENTERPRISE_SERVER           = $00000017;
const PRODUCT_SERVER_FOR_SMALLBUSINESS            = $00000018;
const PRODUCT_SMALLBUSINESS_SERVER_PREMIUM        = $00000019;
const PRODUCT_HOME_PREMIUM_N                      = $0000001A;
const PRODUCT_ENTERPRISE_N                        = $0000001B;
const PRODUCT_ULTIMATE_N                          = $0000001C;
const PRODUCT_WEB_SERVER_CORE                     = $0000001D;
const PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT    = $0000001E;
const PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY      = $0000001F;
const PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING     = $00000020;
const PRODUCT_SERVER_FOUNDATION                   = $00000021;
const PRODUCT_HOME_PREMIUM_SERVER                 = $00000022;
const PRODUCT_SERVER_FOR_SMALLBUSINESS_V          = $00000023;
const PRODUCT_STANDARD_SERVER_V                   = $00000024;
const PRODUCT_DATACENTER_SERVER_V                 = $00000025;
const PRODUCT_ENTERPRISE_SERVER_V                 = $00000026;
const PRODUCT_DATACENTER_SERVER_CORE_V            = $00000027;
const PRODUCT_STANDARD_SERVER_CORE_V              = $00000028;
const PRODUCT_ENTERPRISE_SERVER_CORE_V            = $00000029;
const PRODUCT_HYPERV                              = $0000002A;
const PRODUCT_STORAGE_EXPRESS_SERVER_CORE         = $0000002B;
const PRODUCT_STORAGE_STANDARD_SERVER_CORE        = $0000002C;
const PRODUCT_STORAGE_WORKGROUP_SERVER_CORE       = $0000002D;
const PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE      = $0000002E;
const PRODUCT_STARTER_N                           = $0000002F;
const PRODUCT_PROFESSIONAL                        = $00000030;
const PRODUCT_PROFESSIONAL_N                      = $00000031;
const PRODUCT_SB_SOLUTION_SERVER                  = $00000032;
const PRODUCT_SERVER_FOR_SB_SOLUTIONS             = $00000033;
const PRODUCT_STANDARD_SERVER_SOLUTIONS           = $00000034;
const PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE      = $00000035;
const PRODUCT_SB_SOLUTION_SERVER_EM               = $00000036;
const PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM          = $00000037;
const PRODUCT_SOLUTION_EMBEDDEDSERVER             = $00000038;
const PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE        = $00000039;
const PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT       = $0000003B;
const PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL       = $0000003C;
const PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC    = $0000003D;
const PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC    = $0000003E;
const PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE   = $0000003F;
const PRODUCT_CLUSTER_SERVER_V                    = $00000040;
const PRODUCT_EMBEDDED                            = $00000041;
const PRODUCT_STARTER_E                           = $00000042;
const PRODUCT_HOME_BASIC_E                        = $00000043;
const PRODUCT_HOME_PREMIUM_E                      = $00000044;
const PRODUCT_PROFESSIONAL_E                      = $00000045;
const PRODUCT_ENTERPRISE_E                        = $00000046;
const PRODUCT_ULTIMATE_E                          = $00000047;
const PRODUCT_UNLICENSED                          = $ABCDABCD;

// 2020.06.11 addendum from
// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-mde2/d92ead8f-faf3-47a8-a341-1921dc2c463b
const PRODUCT_ENTERPRISE_EVALUATION               = $00000048;
const PRODUCT_MULTIPOINT_STANDARD_SERVER          = $0000004C;
const PRODUCT_MULTIPOINT_PREMIUM_SERVER           = $0000004D;
const PRODUCT_STANDARD_EVALUATION_SERVER          = $0000004F;
const PRODUCT_DATACENTER_EVALUATION_SERVER        = $00000050;
const PRODUCT_ENTERPRISE_N_EVALUATION             = $00000054;
const PRODUCT_EMBEDDED_AUTOMOTIVE                 = $00000055;
const PRODUCT_EMBEDDED_INDUSTRY_A                 = $00000056;
const PRODUCT_THINPC                              = $00000057;
const PRODUCT_EMBEDDED_A                          = $00000058;
const PRODUCT_EMBEDDED_INDUSTRY                   = $00000059;
const PRODUCT_EMBEDDED_E                          = $0000005A;
const PRODUCT_EMBEDDED_INDUSTRY_E                 = $0000005B;
const PRODUCT_EMBEDDED_INDUSTRY_A_E               = $0000005C;
const PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER = $0000005F;
const PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER  = $00000060;
const PRODUCT_CORE_ARM                            = $00000061;
const PRODUCT_CORE_N                              = $00000062;
const PRODUCT_CORE_COUNTRYSPECIFIC                = $00000063;
const PRODUCT_CORE_SINGLELANGUAGE                 = $00000064;
const PRODUCT_CORE                                = $00000065;
const PRODUCT_PROFESSIONAL_WMC                    = $00000067;
const PRODUCT_MOBILE_CORE                         = $00000068;
const PRODUCT_EMBEDDED_INDUSTRY_EVAL              = $00000069;
const PRODUCT_EMBEDDED_INDUSTRY_E_EVAL            = $0000006A;
const PRODUCT_EMBEDDED_EVAL                       = $0000006B;
const PRODUCT_EMBEDDED_E_EVAL                     = $0000006C;
const PRODUCT_NANO_SERVER                         = $0000006D;
const PRODUCT_CLOUD_STORAGE_SERVER                = $0000006E;
const PRODUCT_CORE_CONNECTED                      = $0000006F;
const PRODUCT_PROFESSIONAL_STUDENT                = $00000070;
const PRODUCT_CORE_CONNECTED_N                    = $00000071;
const PRODUCT_PROFESSIONAL_STUDENT_N              = $00000072;
const PRODUCT_CORE_CONNECTED_SINGLELANGUAGE       = $00000073;
const PRODUCT_CORE_CONNECTED_COUNTRYSPECIFIC      = $00000074;
const PRODUCT_CONNECTED_CAR                       = $00000075;
const PRODUCT_INDUSTRY_HANDHELD                   = $00000076;
const PRODUCT_PPI_PRO                             = $00000077;
const PRODUCT_ARM64_SERVER                        = $00000078;
const PRODUCT_EDUCATION                           = $00000079;
const PRODUCT_EDUCATION_N                         = $0000007A;
const PRODUCT_IOTUAP                              = $0000007B;
const PRODUCT_CLOUD_HOST_INFRASTRUCTURE_SERVER    = $0000007C;
const PRODUCT_ENTERPRISE_S                        = $0000007D;
const PRODUCT_ENTERPRISE_S_N                      = $0000007E;
const PRODUCT_PROFESSIONAL_S                      = $0000007F;
const PRODUCT_PROFESSIONAL_S_N                    = $00000080;
const PRODUCT_ENTERPRISE_S_EVALUATION             = $00000081;
const PRODUCT_ENTERPRISE_S_N_EVALUATION           = $00000082;
const PRODUCT_HOLOGRAPHIC                         = $00000087;
const PRODUCT_HOLOGRAPHIC_BUSINESS                = $00000088;

type UCHAR  = Byte;
type USHORT = Word;
type ULONG  = DWord;
type TWCharArray128 = array[ 0.. 127] of WideChar;

type TuOSVersionInfoExW = packed record
   dwOSVersionInfoSize: ULONG;
   dwMajorVersion     : ULONG;
   dwMinorVersion     : ULONG;
   dwBuildNumber      : ULONG;
   dwPlatformId       : ULONG;
   szCSDVersion       : TWCharArray128;
   wServicePackMajor  : USHORT;
   wServicePackMinor  : USHORT;
   wSuiteMask         : USHORT;
   wProductType       : UCHAR;
   wReserved          : UCHAR;
end;

type TGetVersionFunc = function( var AParam:TuOSVersionInfoExW ): Boolean; stdcall;
type TGetNativeSystemInfo = procedure( var AParam: Windows.TSystemInfo ); stdcall;
type TGetProductInfo = function( dwOSMajorVersion, dwOSMinorVersion, dwSpMajorVersion, dwSpMinorVersion: DWORD; var pdwReturnedProductType: DWORD ): boolean; stdcall;

function CustomVersionFunc( const ALibrary, AFuncName: string; var AParam: TuOSVersionInfoExW ): boolean;
  var DLLWnd : THandle = 0;
      xFunc  : pointer = nil;
begin
   Result := GetWindowsFunction( ALibrary, AFuncName, DllWnd, xFunc );
   if Result then begin
      AParam.dwOSVersionInfoSize := SizeOf( AParam );
      Result := TGetVersionFunc(xFunc)( AParam );
      FreeLibrary( DLLWnd );
   end;
end;

function GetVersionEx( var AOSVIEX: TuOSVersionInfoExW ): boolean;
begin
   Result := CustomVersionFunc( cLib_Kernel32, 'GetVersionExW', AOSVIEX );
end;

function RtlGetVersion( var AOSVIEX: TuOSVersionInfoExW ): boolean;
begin
   // https://docs.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/nf-wdm-rtlgetversion
   Result := CustomVersionFunc( cLib_Ntdll, 'RtlGetVersion', AOSVIEX );
end;


procedure GetNativeSysInfo( var ASysInfo: Windows.TSystemInfo );
  var LibHandle : THandle;
      xFunc  : pointer;
begin
   xFunc := nil;
   // https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getnativesysteminfo
   if GetWindowsFunction( cLib_Kernel32, 'GetNativeSystemInfo', LibHandle{%H-}, xFunc ) then begin
      TGetNativeSystemInfo(xFunc)( ASysInfo );
      FreeLibrary( LibHandle );
   end;
end;

function GetProductInfo( dwOSMajorVersion, dwOSMinorVersion, dwSpMajorVersion, dwSpMinorVersion: DWORD; var pdwReturnedProductType: DWORD ): boolean;
  var LibHandle : THandle;
      xFunc     : pointer;
begin
   xFunc := nil;
   LibHandle := 0;
   Result := dluLoadLibrary.GetWindowsFunction( cLib_Kernel32, 'GetProductInfo', LibHandle, xFunc );
   if Result then begin
      Result := TGetProductInfo(xFunc)( dwOSMajorVersion, dwOSMinorVersion, dwSpMajorVersion, dwSpMinorVersion, pdwReturnedProductType );
      Windows.FreeLibrary( LibHandle );
   end;
end;

function GetLocaleInformation( Flag : LCTYPE ): WideString;
  var Buffer : PWideChar;
      Size   : integer;
begin
    Size := GetLocaleInfoW( LOCALE_USER_DEFAULT, Flag, nil, 0);
    GetMem( Buffer, Size * SizeOf(WideChar) );
    try
       if GetLocaleInfoW( LOCALE_USER_DEFAULT, Flag, Buffer, Size ) <= 0
          then Result := ''
          else Result := Copy( Buffer, 1, Size );
    finally
       FreeMem(Buffer);
    end;
end;

function GetProductFromRegistry(): WideString;
begin
   Result := '';
   with TRegistry.Create do
     try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly( 'SYSTEM\CurrentControlSet\' + 'Control\ProductOptions') then
           try
              Result := {%H-}UpperCase( ReadString( 'ProductType' ) );
              if Result = 'WINNT'    then Result := 'Workstation';
              if Result = 'SERVERNT' then Result := 'Server';
           finally
              CloseKey;
           end;
     finally
        Free;
     end;
end;


//function GetArchitectureAsText( const AProcessorArchitecture: Word ): WideString;
//begin
//   case AProcessorArchitecture of
//      PROCESSOR_ARCHITECTURE_INTEL   : Result := '32-bit';
//      PROCESSOR_ARCHITECTURE_ARM     : Result := 'ARM';
//      PROCESSOR_ARCHITECTURE_IA64    : Result := 'Intel Itanium-based';
//      PROCESSOR_ARCHITECTURE_AMD64   : Result := '64-bit';
//      PROCESSOR_ARCHITECTURE_ARM64   : Result := 'ARM64';
//      PROCESSOR_ARCHITECTURE_UNKNOWN : Result := 'Unknown architecture';
//      else Result := 'Unknown value (' + IntToStr(AProcessorArchitecture) {%H-}+ ')';
//   end;
//end;

function UnknownSystem( const AText: WideString; AParam: TuOSVersionInfoExW ): WideString;
begin
   with AParam do begin
      Result := WideString( Format( '%s [%d.%d.%d prod. $%x]', [ AText, dwMajorVersion, dwMinorVersion, dwBuildNumber, wProductType ] ) );
   end;
end;

var ArchitectureDict : IuDictionary;
var SuiteDict        : IuDictionary;
var ProductDict      : IuDictionary;

constructor TWinVerSpec.Create;
begin
   fName         := 'Windows';
   fPlatformId   := Win32Platform;
   fMajorVersion := Win32MajorVersion;
   fMinorVersion := Win32MinorVersion;
   fBuildNumber  := Win32BuildNumber;
   fCSDVersion   := {%H-}Win32CSDVersion;

   fSuites       := nil;

   case fPlatformId of

     // Windows3.1(Win32s)
     VER_PLATFORM_WIN32s        : Prepare_Win32s_Platform;

     // Windows9x?
     VER_PLATFORM_WIN32_WINDOWS : Prepare_Win32_Windows_Platform;

     // WindowsNT?
     VER_PLATFORM_WIN32_NT      : Prepare_Win32_NT_Platform;

   end;

   fLang         := GetLocaleInformation( LOCALE_SABBREVCTRYNAME );
   AppendToName( fLang );

   //fTechnicalInfo:= {$IFDEF FPC}WideFormat{$ELSE}Format{$ENDIF}( '%d.%d.%d, platform=%d, csd="%s", SP=%d.%d, suite=0x%s, product_type=%d, product_info=%d, lang="%s"',
   //                               [ fMajorVersion, fMinorVersion, fBuildNumber, fPlatformId,
   //                                 fCSDVersion, fServicePackMajor, fServicePackMinor,
   //                                 IntToHex( fSuiteMask, 2*SizeOf(fSuitemask) ), fProductType, fProductInfo, fLang ] );

   fTechnicalInfo:= {$IFDEF FPC}WideFormat{$ELSE}Format{$ENDIF}( '%d.%d.%d, platform=%d, csd="%s", SP=%d.%d, suite=0x%s, product_type=%d, product_info=%d, lang="%s"',
                                  [ fMajorVersion, fMinorVersion, fBuildNumber, fPlatformId,
                                    fCSDVersion, fServicePackMajor, fServicePackMinor,
                                    IntToHex( fSuiteMask, 2*SizeOf(fSuitemask) ), fProductType, fProductInfo, fLang ] );


   fAllProperties:= TStringList.Create;
   with fAllProperties do begin
      BeginUpdate;
      AddPair( 'PlatformId',             IntToStr( fPlatformId ) );
      AddPair( 'MajorVersion',           IntToStr( fMajorVersion ) );
      AddPair( 'MinorVersion',           IntToStr( fMinorVersion ) );
      AddPair( 'BuildNumber',            IntToStr( fBuildNumber ) );
      AddPair( 'CSDVersion',             UTF8Encode( fCSDVersion ) );
      AddPair( 'ServicePackMajor',       IntToStr( fServicePackMajor ) );
      AddPair( 'ServicePackMinor',       IntToStr( fServicePackMinor ) );
      AddPair( 'SuiteMask',              IntToStr( fSuiteMask ) );
      AddPair( 'ProcessorArchitecture',  IntToStr( fProcessorArchitecture ) );
      AddPair( 'ProductType',            IntToStr( fProductType ) );
      AddPair( 'ProductInfo',            IntToStr( fProductInfo) );
      AddPair( 'MediaCenter',            IntToStr( Integer( fMediaCenter )));
      AddPair( 'TabletPC',               IntToStr( Integer( fTabletPC )) );
      AddPair( 'Suites',                 fSuites.Text );
      AddPair( 'Lang',                   UTF8Encode( fLang ) );
      AddPair( 'Reg.BuildLab',           fBuildLab           );
      AddPair( 'Reg.BuildLabEx',         fBuildLabEx         );
      AddPair( 'Reg.CSDBuildNumber',     fCSDBuildNumber     );
      AddPair( 'Reg.CSDVersion',         fCSDVersionReg      );
      AddPair( 'Reg.CurrentBuildNumber', fCurrentBuildNumber );
      AddPair( 'Reg.CurrentVersion',     fCurrentVersion );
      AddPair( 'Reg.EditionId',          fEditionId );
      AddPair( 'Reg.ProductName',        fProductName );
      AddPair( 'Reg.ReleaseId',          fReleaseId );
      AddPair( 'Reg.UBR',                IntToStr( fUBR ) );
      AddPair( 'Reg.InstallDate',        DateTimeToStr( fInstallDate ) );
      EndUpdate;
    end;
end;

destructor TWinVerSpec.Destroy;
begin
   fAllProperties.Free;
   if Assigned( fSuites ) then fSuites.Free;
   inherited Destroy;
end;


function TWinVerSpec.GetCompilationInfo: WideString;
  var s : string;
begin
   s := '';
   if fUBR > 0 then s := '.' + IntToStr(fUbr);
   Result := Trim( {$IFDEF FPC}WideFormat{$ELSE}Format{$ENDIF}( '%d.%d.%d%s %s',
                   [ fMajorVersion, fMinorVersion, fBuildNumber, s, fCSDVersion ] ) );
end;

function TWinVerSpec.GetWindowsRelease: UnicodeString;
begin
   Result := '';
   // https://learn.microsoft.com/en-us/windows/release-health/release-information
   // https://learn.microsoft.com/en-us/windows/release-health/windows11-release-information
   if (fMajorVersion = 10) and (fMinorVersion = 0) and (fProductType = VER_NT_WORKSTATION) then begin
        case fBuildNumber of
           // windows 11
           22000 : Result := '21H2';
           22621 : Result := '22H2';
           22631 : Result := '23H2';
           26100 : Result := '24H2';
           // windows 10
           19045 : Result := '22H2';
           19044 : Result := '21H2';
           19043 : Result := '21H1';
           19042 : Result := '20H2';
           19041 : Result := '2004';
           18363 : Result := '1909';
           18362 : Result := '1903';
           17763 : Result := '1809';
           17134 : Result := '1803';
           16299 : Result := '1709';
           15063 : Result := '1703';
           14393 : Result := '1607';
           10586 : Result := '1511';
           10240 : Result := '1507';

           else    Result := '????';
        end;
   end;
end;

{
https://stackoverflow.com/questions/8144599/getting-the-windows-version
https://wiki.freepascal.org/WindowsVersion
}
procedure TWinVerSpec.Prepare_Win32_NT_Platform;
  var osi : TuOSVersionInfoExW;
      nsi : Windows.TSystemInfo;
      n   : integer;
begin
   osi := Default( TuOSVersionInfoExW );
   if not GetVersionEx( osi ) then begin
      self.fName := 'Unknown';
      exit;
   end;
   // informacje mogą być niepoprawne dla Windows 10, jesli nie ma ustawionego manifestu
   // skorzystajmy więc z funkcji RtlGetVersion
   if osi.dwMajorVersion >= 6  then begin
      RtlGetVersion( osi );
   end;

   fName             := 'Windows';
   fMajorVersion     := osi.dwMajorVersion;
   fMinorVersion     := osi.dwMinorVersion;
   fBuildNumber      := osi.dwBuildNumber;
   fPlatformId       := osi.dwPlatformId;
   fCSDVersion       := osi.szCSDVersion;
   fServicePackMajor := osi.wServicePackMajor;
   fServicePackMinor := osi.wServicePackMinor ;
   fSuiteMask        := osi.wSuiteMask;
   fProductType      := osi.wProductType;

   if fMajorVersion <= 4 then begin
      AppendToName( ' NT ' + IntToStr( fMajorVersion)+'.'+IntToStr( fMinorVersion) {%H-});
   end;

   if fMajorVersion = 5 then begin

      //  5.0 => Windows 2000
      //  5.1 => Windows XP
      //  5.2 => Windows XP64 or Windows 2003 Server

      case fMinorVersion of
         0 : begin
                AppendToName( '2000' );
                AppendToNameIf( fProductType = VER_NT_WORKSTATION, 'Professional', 'Server' );
         end;

         1 : begin
                if fProductType = VER_NT_WORKSTATION then begin
                   AppendToName( 'XP' );
                   AppendToNameIf( (fSuiteMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL,
                                   'Home Edition',
                                   'Professional' );
                end else
                   AppendToName( 'Server 2003' );
             end;

         2 : begin
                if ( fProductType = VER_NT_WORKSTATION) then begin
                   AppendToName( 'XP x64 Professional' )
                end else
                   if (fSuiteMask and VER_SUITE_WH_SERVER) = VER_SUITE_WH_SERVER then
                      AppendToName( 'Home Server' )
                   else begin
                      AppendToName( 'Server 2003' );
                      n := GetSystemMetrics( SM_SERVERR2 );
                      AppendToNameIf( n <> 0, 'R2 build '+IntToStr(n){%H-}, '' );
                   end;
             end
         else AppendToName( '???' );
      end;

   end;

   if fMajorVersion = 6 then begin

      //  6.0 => Windows Vista or Windows 2008 Server
      //  6.1 => Windows 7 or Windows 2008 Server R2
      //  6.2 => Windows 8 or Windows Server 2012
      //  6.3 => Windows 8.1 or Windows Server 2012 RS

      case fMinorVersion of
         0 : AppendToNameIf( fProductType = VER_NT_WORKSTATION, 'Vista ', 'Server 2008'   );
         1 : AppendToNameIf( fProductType = VER_NT_WORKSTATION, '7 ',     'Server 2008 R2' );
         2 : AppendToNameIf( fProductType = VER_NT_WORKSTATION, '8 ',     'Server 2012'    );
         3 : AppendToNameIf( fProductType = VER_NT_WORKSTATION, '8.1 ',   'Server 2012 RS' );
        else fName := UnknownSystem( 'Unknown Windows version ', osi );
      end {case};

   end;

   if fMajorVersion = 10 then begin

      TryReadFromRegistry( );

      // https://learn.microsoft.com/en-us/windows/release-health/windows11-release-information
      if fMinorVersion = 0 then begin
         if fProductType = VER_NT_WORKSTATION then begin
            if fBuildNumber < 22000
               then AppendToName( '10' )
               else AppendToName( '11' );
            AppendToName( GetWindowsRelease() );
         end else AppendToName( 'Server 2016' );
      end else
         fName := UnknownSystem( 'Unknown Windows version', osi );


      //case fMinorVersion of
      //   0 : AppendToNameIf( fProductType = VER_NT_WORKSTATION, '10', 'Server 2016' );
      //  else fName := UnknownSystem( 'Unknown Windows version', osi );
      //end;

   end;

   if fMajorVersion > 4 then begin

      if GetProductInfo( fMajorVersion, fMinorVersion, fServicePackMajor, fServicePackMajor, fProductInfo ) then begin
         AppendToName( ProductDict.Value( fProductInfo ) );
      end else if fProductType = VER_NT_WORKSTATION then begin
         case fSuiteMask of
            512 : AppendToName( 'Personal' );
            768 : AppendToName( 'Home Premium' );
            else  AppendToName( 'Professional' );
         end;
      end else if fProductType = VER_NT_DOMAIN_CONTROLLER then begin
         AppendToName( 'Domain Controller' );
      end else if fProductType = VER_NT_SERVER then begin
         if fSuiteMask = VER_SUITE_DATACENTER then begin
            AppendToName( 'DataCenter Server' )
         end else if fSuiteMask = VER_SUITE_ENTERPRISE then begin
            AppendToName( 'Advanced Server' )
         end else
            AppendToName( 'Server' );
      end else begin
         AppendToName( GetProductFromRegistry() );
      end;


      GetNativeSystemInfo( {$IFDEF FPC}@{$ENDIF}nsi );
      fProcessorArchitecture := nsi.wProcessorArchitecture;
      fMediaCenter := GetSystemMetrics( SM_MEDIACENTER ) <> 0;
      fTabletPC    := GetSystemMetrics( SM_TABLETPC ) <> 0;

      AppendToName( ArchitectureDict.Value( fProcessorArchitecture ) );
      AppendToNameIf( fMediaCenter, 'Media Center', '' );
      AppendToNameIf( fTabletPC, 'Tablet PC', '' );

      fSuites := SuiteDict.GetMasksList( fSuiteMask );

   end;

end;

procedure TWinVerSpec.TryReadFromRegistry;
  const cNoSecsInDay  = 86400;
begin
   // We need to read the real registry, not the 32 bit view, because some of the entries
   // don't exist there.
   with TRegistry.Create( KEY_READ or KEY_WOW64_64KEY ) do begin
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly( 'SOFTWARE\Microsoft\Windows NT\CurrentVersion' ) then begin
         fBuildLab           := ReadString( 'BuildLab'           );
         fBuildLabEx         := ReadString( 'BuildLabEx'         );
         fCSDBuildNumber     := ReadString( 'CSDBuildNumber'     );
         fCSDVersionReg      := ReadString( 'CSDVersion'         );
         fCurrentBuildNumber := ReadString( 'CurrentBuildNumber' );
         fCurrentVersion     := ReadString( 'CurrentVersion'     );
         fEditionId          := ReadString( 'EditionId'          );
         fProductName        := ReadString( 'ProductName'        );
         fReleaseId          := ReadString( 'ReleaseId'          );
         fUBR                := ReadInteger( 'UBR'               );
         fInstallDate        := EncodeDate( 1970, 1, 1 ) + ReadInteger( 'InstallDate' ) / cNoSecsInDay;
      end;
      Free;
   end;
end;

procedure TWinVerSpec.AppendToName(const AText: AnsiString);
begin
  AppendToName( WideString( AText ) );
end;

procedure TWinVerSpec.AppendToName(const AText: WideString);
begin
   if AText <> '' then self.fName := self.fName + ' ' + AText;
end;

procedure TWinVerSpec.AppendToNameIf(const ACond: boolean; const AText_true, AText_false: WideString );
begin
   if ACond
      then self.AppendToName( AText_true )
      else self.AppendToName( AText_false );
end;

procedure TWinVerSpec.Prepare_Win32s_Platform;
begin
    AppendToName( {$IFDEF FPC}WideFormat{$ELSE}Format{$ENDIF}(' %d.%d (Win32s)',[ fMajorVersion, fMinorVersion ] ) );
end;

procedure TWinVerSpec.Prepare_Win32_Windows_Platform;
begin
   case fMinorVersion of

     // Windows95
     00: begin
            AppendToName( '95' );
            case LOWORD( fBuildNumber ) of
                1111..1111: AppendToName( 'OSR2'   );
                1212..1213: AppendToName( 'OSR2.1' );
                1214..9999: AppendToName( 'OSR2.5' );
            end;
         end;

     // Windows98
     10: begin
            AppendToName( '98' );
            case LOWORD( fBuildNumber ) of
                2222..9999: AppendToName( 'Second Edition' );
            end;
         end;

     // WindowsMe
     90: AppendToName( 'Me' );
   end;
end;

const cProfessional  = 'Professional';
const cBusiness      = 'Business Edition';
const cEnterpriseEd  = 'Enterprise Edition';
const cServerEnterpriseEdition = 'Server Enterprise Edition ';
const cHomeBasic     = 'Home Basic';
const cHomePremium   = 'Home Premium ';

const cStorageServer = 'Storage Server';
const cUltimate      = 'Ultimate Edition';
const cWebServer     = 'Web Server';
const cMultipoint    = 'Multipoint';
initialization

    ArchitectureDict := TuxDictionary.Create();
    with ArchitectureDict do begin
      Add( PROCESSOR_ARCHITECTURE_INTEL,   '32-bit' );
      Add( PROCESSOR_ARCHITECTURE_ARM,     'ARM' );
      Add( PROCESSOR_ARCHITECTURE_IA64,    'Intel Itanium-based' );
      Add( PROCESSOR_ARCHITECTURE_AMD64,   '64-bit' );
      Add( PROCESSOR_ARCHITECTURE_ARM64,   'ARM64' );
      Add( PROCESSOR_ARCHITECTURE_UNKNOWN, 'Unknown architecture' );
   end;

    SuiteDict := TuxDictionary.Create();
    with SuiteDict do begin
      Add( VER_SUITE_SMALLBUSINESS,             'Microsoft Small Business Server was once installed on the system, but may have been upgraded to another version of Windows. For more information about this flag bit, see the following Remarks section.' );
      Add( VER_SUITE_ENTERPRISE,                'Windows Server 2008 Enterprise, Windows Server 2003, Enterprise Edition, or Windows 2000 Advanced Server is installed.' );
      Add( VER_SUITE_BACKOFFICE,                'Microsoft BackOffice components are installed.' );
      Add( VER_SUITE_COMMUNICATIONS,            'SUITE_COMMUNICATIONS' );
      Add( VER_SUITE_TERMINAL,                  'Terminal Services is installed. '+sLineBreak+
                                                'This value is always set. '+sLineBreak+
                                                'If VER_SUITE_TERMINAL is set but VER_SUITE_SINGLEUSERTS is not set, the operating system is running in application server mode.' );
      Add( VER_SUITE_SMALLBUSINESS_RESTRICTED,  'SUITE_SMALLBUSINESS_RESTRICTED' );
      Add( VER_SUITE_EMBEDDEDNT,                'Windows XP Embedded is installed.' );
      Add( VER_SUITE_DATACENTER,                'Windows Server 2008 Datacenter, Windows Server 2003, Datacenter Edition, or Windows 2000 Datacenter Server is installed.' );
      Add( VER_SUITE_SINGLEUSERTS,              'Remote Desktop is supported, but only one interactive session is supported. This value is set unless the system is running in application server mode.' );
      Add( VER_SUITE_PERSONAL,                  'Windows Vista Home Premium, Windows Vista Home Basic, or Windows XP Home Edition is installed.' );
      Add( VER_SUITE_BLADE,                     'Windows Server 2003, Web Edition is installed.' );
      Add( VER_SUITE_EMBEDDED_RESTRICTED,       'SUITE_EMBEDDED_RESTRICTED' );
      Add( VER_SUITE_SECURITY_APPLIANCE,        'SUITE_SECURITY_APPLIANCE' );
      Add( VER_SUITE_STORAGE_SERVER,            'Windows Storage Server 2003 R2 or Windows Storage Server 2003 is installed. ' );
      Add( VER_SUITE_COMPUTE_SERVER,            'Windows Server 2003, Compute Cluster Edition is installed. ' );
      Add( VER_SUITE_WH_SERVER,                 'Windows Home Server' );
      Add( VER_SUITE_MULTIUSERTS,               'Multi-user' );
    end;

    ProductDict := TuxDictionary.Create();
    with ProductDict do begin
      Add( (* $00000000 *) PRODUCT_UNDEFINED,                           'An unknown' );
      Add( (* $00000001 *) PRODUCT_ULTIMATE,                            cUltimate );
      Add( (* $00000002 *) PRODUCT_HOME_BASIC,                          cHomeBasic );
      Add( (* $00000003 *) PRODUCT_HOME_PREMIUM,                        cHomePremium + 'Edition' );
      Add( (* $00000004 *) PRODUCT_ENTERPRISE,                          cEnterpriseEd );
      Add( (* $00000005 *) PRODUCT_HOME_BASIC_N,                        cHomeBasic );
      Add( (* $00000006 *) PRODUCT_BUSINESS,                            cBusiness );
      Add( (* $00000007 *) PRODUCT_STANDARD_SERVER,                     'Server Standard Edition (full installation)' );
      Add( (* $00000008 *) PRODUCT_DATACENTER_SERVER,                   'Server Datacenter Edition (full installation)' );
      Add( (* $00000009 *) PRODUCT_SMALLBUSINESS_SERVER,                'Small Business Server' );
      Add( (* $0000000A *) PRODUCT_ENTERPRISE_SERVER,                   cServerEnterpriseEdition + '(full installation)' );
      Add( (* $0000000B *) PRODUCT_STARTER,                             'Starter Edition' );
      Add( (* $0000000C *) PRODUCT_DATACENTER_SERVER_CORE,              'Server Datacenter Edition (core installation)' );
      Add( (* $0000000D *) PRODUCT_STANDARD_SERVER_CORE,                'Server Standard Edition (core installation)' );
      Add( (* $0000000E *) PRODUCT_ENTERPRISE_SERVER_CORE,              cServerEnterpriseEdition + '(core installation)' );
      Add( (* $0000000F *) PRODUCT_ENTERPRISE_SERVER_IA64,              cServerEnterpriseEdition + 'for Itanium-based Systems' );

      Add( (* $00000010 *) PRODUCT_BUSINESS_N,                          cBusiness );
      Add( (* $00000011 *) PRODUCT_WEB_SERVER,                          cWebServer );
      Add( (* $00000012 *) PRODUCT_CLUSTER_SERVER,                      'Cluster Server Edition' );
      Add( (* $00000013 *) PRODUCT_HOME_SERVER,                         'Home Server Edition' );
      Add( (* $00000014 *) PRODUCT_STORAGE_EXPRESS_SERVER,              cStorageServer + 'Express Edition' );
      Add( (* $00000015 *) PRODUCT_STORAGE_STANDARD_SERVER,             cStorageServer + 'Standard Edition' );
      Add( (* $00000016 *) PRODUCT_STORAGE_WORKGROUP_SERVER,            cStorageServer + 'Workgroup Edition' );
      Add( (* $00000017 *) PRODUCT_STORAGE_ENTERPRISE_SERVER,           cStorageServer + 'Enterprise Edition' );
      Add( (* $00000018 *) PRODUCT_SERVER_FOR_SMALLBUSINESS,            'Server for Small Business Edition' );
      Add( (* $00000019 *) PRODUCT_SMALLBUSINESS_SERVER_PREMIUM,        'Small Business Server Premium Edition' );
      Add( (* $0000001A *) PRODUCT_HOME_PREMIUM_N,                      cHomePremium + 'Edition' );
      Add( (* $0000001B *) PRODUCT_ENTERPRISE_N,                        cEnterpriseEd );
      Add( (* $0000001C *) PRODUCT_ULTIMATE_N,                          cUltimate );
      Add( (* $0000001D *) PRODUCT_WEB_SERVER_CORE,                     cWebServer + ' (core installation)' );
      Add( (* $0000001E *) PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT,    'Windows Essential Business Server Management Server Edition' );
      Add( (* $0000001F *) PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY,      'Windows Essential Business Server Security Server Edition' );

      Add( (* $00000020 *) PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING,     'Windows Essential Business Server Messaging Server Edition' );
      Add( (* $00000021 *) PRODUCT_SERVER_FOUNDATION,                   'Server Foundation' );
      Add( (* $00000022 *) PRODUCT_HOME_PREMIUM_SERVER,                 cHomePremium + 'Server Edition' );
      Add( (* $00000023 *) PRODUCT_SERVER_FOR_SMALLBUSINESS_V,          'SERVER_FOR_SMALLBUSINESS_V' );
      Add( (* $00000024 *) PRODUCT_STANDARD_SERVER_V,                   'STANDARD_SERVER_V' );
      Add( (* $00000025 *) PRODUCT_DATACENTER_SERVER_V,                 'DATACENTER_SERVER_V' );
      Add( (* $00000026 *) PRODUCT_ENTERPRISE_SERVER_V,                 'ENTERPRISE_SERVER_V' );
      Add( (* $00000027 *) PRODUCT_DATACENTER_SERVER_CORE_V,            'DATACENTER_SERVER_CORE_V' );
      Add( (* $00000028 *) PRODUCT_STANDARD_SERVER_CORE_V,              'STANDARD_SERVER_CORE_V' );
      Add( (* $00000029 *) PRODUCT_ENTERPRISE_SERVER_CORE_V,            'ENTERPRISE_SERVER_CORE_V' );
      Add( (* $0000002A *) PRODUCT_HYPERV,                              'Hyper-V Server Edition' );
      Add( (* $0000002B *) PRODUCT_STORAGE_EXPRESS_SERVER_CORE,         'STORAGE_EXPRESS_SERVER_CORE' );
      Add( (* $0000002C *) PRODUCT_STORAGE_STANDARD_SERVER_CORE,        'STORAGE_STANDARD_SERVER_CORE' );
      Add( (* $0000002D *) PRODUCT_STORAGE_WORKGROUP_SERVER_CORE,       'STORAGE_WORKGROUP_SERVER_CORE' );
      Add( (* $0000002E *) PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE,      'STORAGE_ENTERPRISE_SERVER_CORE' );
      Add( (* $0000002F *) PRODUCT_STARTER_N,                           'STARTER_N' );

      Add( (* $00000030 *) PRODUCT_PROFESSIONAL,                        cProfessional );
      Add( (* $00000031 *) PRODUCT_PROFESSIONAL_N,                      cProfessional );
      Add( (* $00000032 *) PRODUCT_SB_SOLUTION_SERVER,                  'SB_SOLUTION_SERVER' );
      Add( (* $00000033 *) PRODUCT_SERVER_FOR_SB_SOLUTIONS,             'SERVER_FOR_SB_SOLUTIONS' );
      Add( (* $00000034 *) PRODUCT_STANDARD_SERVER_SOLUTIONS,           'STANDARD_SERVER_SOLUTIONS' );
      Add( (* $00000035 *) PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE,      'STANDARD_SERVER_SOLUTIONS_CORE' );
      Add( (* $00000036 *) PRODUCT_SB_SOLUTION_SERVER_EM,               'SB_SOLUTION_SERVER_EM' );
      Add( (* $00000037 *) PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM,          'SERVER_FOR_SB_SOLUTIONS_EM' );
      Add( (* $00000038 *) PRODUCT_SOLUTION_EMBEDDEDSERVER,             'SOLUTION_EMBEDDEDSERVER' );
      Add( (* $00000039 *) PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE,        'SOLUTION_EMBEDDEDSERVER_CORE' );
      Add( (* $0000003B *) PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT,       'ESSENTIALBUSINESS_SERVER_MGMT' );
      Add( (* $0000003C *) PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL,       'ESSENTIALBUSINESS_SERVER_ADDL' );
      Add( (* $0000003D *) PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC,    'ESSENTIALBUSINESS_SERVER_MGMTSVC' );
      Add( (* $0000003E *) PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC,    'ESSENTIALBUSINESS_SERVER_ADDLSVC' );
      Add( (* $0000003F *) PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE,   'SMALLBUSINESS_SERVER_PREMIUM_CORE' );

      Add( (* $00000040 *) PRODUCT_CLUSTER_SERVER_V,                    'CLUSTER_SERVER_V' );
      Add( (* $00000041 *) PRODUCT_EMBEDDED,                            'EMBEDDED' );
      Add( (* $00000042 *) PRODUCT_STARTER_E,                           'STARTER_E' );
      Add( (* $00000043 *) PRODUCT_HOME_BASIC_E,                        'HOME_BASIC_E' );
      Add( (* $00000044 *) PRODUCT_HOME_PREMIUM_E,                      'HOME_PREMIUM_E' );
      Add( (* $00000045 *) PRODUCT_PROFESSIONAL_E,                      'PROFESSIONAL_E' );
      Add( (* $00000046 *) PRODUCT_ENTERPRISE_E,                        'ENTERPRISE_E' );
      Add( (* $00000047 *) PRODUCT_ULTIMATE_E,                          'ULTIMATE_E' );
      Add( (* $00000048 *) PRODUCT_ENTERPRISE_EVALUATION,               'Enterprise Evaluation' );
      Add( (* $0000004C *) PRODUCT_MULTIPOINT_STANDARD_SERVER,          cMultipoint + ' Standard Server' );
      Add( (* $0000004D *) PRODUCT_MULTIPOINT_PREMIUM_SERVER,           cMultipoint + ' Premium Server' );
      Add( (* $0000004F *) PRODUCT_STANDARD_EVALUATION_SERVER,          'Server Standard (evaluation installation)'   );

      Add( (* $00000050 *) PRODUCT_DATACENTER_EVALUATION_SERVER,        'Server Datacenter (evaluation installation)' );
      Add( (* $00000054 *) PRODUCT_ENTERPRISE_N_EVALUATION,             'Enterprise N Evaluation'                     );
      Add( (* $00000055 *) PRODUCT_EMBEDDED_AUTOMOTIVE,                 'EMBEDDED_AUTOMOTIVE'                         );
      Add( (* $00000056 *) PRODUCT_EMBEDDED_INDUSTRY_A,                 'EMBEDDED_INDUSTRY_A'                         );
      Add( (* $00000057 *) PRODUCT_THINPC,                              'THINPC'                                      );
      Add( (* $00000058 *) PRODUCT_EMBEDDED_A,                          'EMBEDDED_A'                                  );
      Add( (* $00000059 *) PRODUCT_EMBEDDED_INDUSTRY,                   'EMBEDDED_INDUSTRY'                           );
      Add( (* $0000005A *) PRODUCT_EMBEDDED_E,                          'EMBEDDED_E'                                  );
      Add( (* $0000005B *) PRODUCT_EMBEDDED_INDUSTRY_E,                 'EMBEDDED_INDUSTRY_E'                         );
      Add( (* $0000005C *) PRODUCT_EMBEDDED_INDUSTRY_A_E,               'EMBEDDED_INDUSTRY_A_E'                       );
      Add( (* $0000005F *) PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER, 'STORAGE_WORKGROUP_EVALUATION_SERVER'         );

      Add( (* $00000060 *) PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER,  'STORAGE_STANDARD_EVALUATION_SERVER'          );
      Add( (* $00000061 *) PRODUCT_CORE_ARM,                            'CORE_ARM'                                    );
      Add( (* $00000062 *) PRODUCT_CORE_N,                              'CORE_N'                                      );
      Add( (* $00000063 *) PRODUCT_CORE_COUNTRYSPECIFIC,                'CORE_COUNTRYSPECIFIC'                        );
      Add( (* $00000064 *) PRODUCT_CORE_SINGLELANGUAGE,                 'CORE_SINGLELANGUAGE'                         );
      Add( (* $00000065 *) PRODUCT_CORE,                                'CORE'                                        );
      Add( (* $00000067 *) PRODUCT_PROFESSIONAL_WMC,                    cProfessional + ' with Media Center'          );
      Add( (* $00000068 *) PRODUCT_MOBILE_CORE,                         'MOBILE_CORE'                                 );
      Add( (* $00000069 *) PRODUCT_EMBEDDED_INDUSTRY_EVAL,              'EMBEDDED_INDUSTRY_EVAL'                      );
      Add( (* $0000006A *) PRODUCT_EMBEDDED_INDUSTRY_E_EVAL,            'EMBEDDED_INDUSTRY_E_EVAL'                    );
      Add( (* $0000006B *) PRODUCT_EMBEDDED_EVAL,                       'EMBEDDED_EVAL'                               );
      Add( (* $0000006C *) PRODUCT_EMBEDDED_E_EVAL,                     'EMBEDDED_E_EVAL'                             );
      Add( (* $0000006D *) PRODUCT_NANO_SERVER,                         'NANO_SERVER'                                 );
      Add( (* $0000006E *) PRODUCT_CLOUD_STORAGE_SERVER,                'CLOUD_STORAGE_SERVER'                        );
      Add( (* $0000006F *) PRODUCT_CORE_CONNECTED,                      'CORE_CONNECTED'                              );

      Add( (* $00000070 *) PRODUCT_PROFESSIONAL_STUDENT,                'PROFESSIONAL_STUDENT'                        );
      Add( (* $00000071 *) PRODUCT_CORE_CONNECTED_N,                    'CORE_CONNECTED_N'                            );
      Add( (* $00000072 *) PRODUCT_PROFESSIONAL_STUDENT_N,              'PROFESSIONAL_STUDENT_N'                      );
      Add( (* $00000073 *) PRODUCT_CORE_CONNECTED_SINGLELANGUAGE,       'CORE_CONNECTED_SINGLELANGUAGE'               );
      Add( (* $00000074 *) PRODUCT_CORE_CONNECTED_COUNTRYSPECIFIC,      'CORE_CONNECTED_COUNTRYSPECIFIC'              );
      Add( (* $00000075 *) PRODUCT_CONNECTED_CAR,                       'CONNECTED_CAR'                               );
      Add( (* $00000076 *) PRODUCT_INDUSTRY_HANDHELD,                   'INDUSTRY_HANDHELD'                           );
      Add( (* $00000077 *) PRODUCT_PPI_PRO,                             'PPI_PRO'                                     );
      Add( (* $00000078 *) PRODUCT_ARM64_SERVER,                        'ARM64_SERVER'                                );
      Add( (* $00000079 *) PRODUCT_EDUCATION,                           'EDUCATION'                                   );
      Add( (* $0000007A *) PRODUCT_EDUCATION_N,                         'EDUCATION_N'                                 );
      Add( (* $0000007B *) PRODUCT_IOTUAP,                              'IOTUAP'                                      );
      Add( (* $0000007C *) PRODUCT_CLOUD_HOST_INFRASTRUCTURE_SERVER,    'CLOUD_HOST_INFRASTRUCTURE_SERVER'            );
      Add( (* $0000007D *) PRODUCT_ENTERPRISE_S,                        'ENTERPRISE_S'                                );
      Add( (* $0000007E *) PRODUCT_ENTERPRISE_S_N,                      'ENTERPRISE_S_N'                              );
      Add( (* $0000007F *) PRODUCT_PROFESSIONAL_S,                      'PROFESSIONAL_S'                              );

      Add( (* $00000080 *) PRODUCT_PROFESSIONAL_S_N,                    'PROFESSIONAL_S_N'                            );
      Add( (* $00000081 *) PRODUCT_ENTERPRISE_S_EVALUATION,             'ENTERPRISE_S_EVALUATION'                     );
      Add( (* $00000082 *) PRODUCT_ENTERPRISE_S_N_EVALUATION,           'ENTERPRISE_S_N_EVALUATION'                   );
      Add( (* $00000087 *) PRODUCT_HOLOGRAPHIC,                         'HOLOGRAPHIC'                                 );
      Add( (* $00000088 *) PRODUCT_HOLOGRAPHIC_BUSINESS,                'HOLOGRAPHIC BUSINESS'                        );

      Add( (* $ABCDABCD *) PRODUCT_UNLICENSED,                          'Unlicensed'                                  );
    end;
    AppWinVer := TWinVerSpec.Create;

finalization

    AppWinVer.Free;
end.

