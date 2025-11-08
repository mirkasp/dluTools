unit lxKeyValueArray;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$codepage UTF8}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

{$IFNDEF FPC}
   {$IF RTLVersion < 18}
      {$MESSAGE Warn 'Not tested on Delphi versions before 2007!'}
   {$IFEND}
{$ENDIF}

interface

uses Classes;


function ProductForKey( const AKey: Cardinal ): string;
function ArchitectureForKey( const AKey: Cardinal ): string;

procedure GetSuitesList( AKey: Cardinal; ASuites: TStrings );
procedure GetWin1xReleaseInfo( const ABuildNumber: Cardinal; out XVersion, XReleaseId: string );

implementation

uses Windows, SysUtils;

type TKeyValueRec = record
       Key        : Cardinal;
       Name       : String;
     end;
type TKeyValueArray = array of TKeyValueRec;

function BinarySearch( const ATable: TKeyValueArray; const AKey: Cardinal): string;
  var i, j, m: integer;
begin
   i := Low(ATable);
   j := High(ATable);
   while i <= j do begin
       m := (i + j) div 2;
       if ATable[m].Key = AKey then begin
          Result := ATable[m].Name;
          Exit;
       end else if ATable[m].Key < AKey then begin
          i := m + 1;
       end else begin
          j := m - 1;
       end;
   end ;
   Result := Format( 'Unknown value for key 0x%X', [ AKey ] );
end ;


// ProductInfo constans
const PRODUCT_UNDEFINED                           = $00000000;
const PRODUCT_ULTIMATE                            = $00000001;
const PRODUCT_HOME_BASIC                          = $00000002;
const PRODUCT_HOME_PREMIUM                        = $00000003;
const PRODUCT_ENTERPRISE                          = $00000004;
const PRODUCT_HOME_BASIC_N                        = $00000005;
const PRODUCT_BUSINESS                            = $00000006;
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
const PRODUCT_SERVERRDSH                          = $000000AF;     // + 2025.03.30

const cProfessional            = 'Professional';
const cBusiness                = 'Business Edition';
const cEnterpriseEd            = 'Enterprise Edition';
const cServerEnterpriseEdition = 'Server Enterprise Edition ';
const cHomeBasic               = 'Home Basic';
const cHomePremium             = 'Home Premium ';

const cStorageServer           = 'Storage Server';
const cUltimate                = 'Ultimate Edition';
const cWebServer               = 'Web Server';
const cMultipoint              = 'Multipoint';


const MAX_PRODUCTS = 124;
const ProductsDict: array[ 0..MAX_PRODUCTS-1 ] of TKeyValueRec = (
        ( Key: (* $00000000 *) PRODUCT_UNDEFINED;                           Name : 'An unknown' ),
        ( Key: (* $00000001 *) PRODUCT_ULTIMATE;                            Name : cUltimate ),
        ( Key: (* $00000002 *) PRODUCT_HOME_BASIC;                          Name : cHomeBasic ),
        ( Key: (* $00000003 *) PRODUCT_HOME_PREMIUM;                        Name : cHomePremium + 'Edition' ),
        ( Key: (* $00000004 *) PRODUCT_ENTERPRISE;                          Name : cEnterpriseEd ),
        ( Key: (* $00000005 *) PRODUCT_HOME_BASIC_N;                        Name : cHomeBasic ),
        ( Key: (* $00000006 *) PRODUCT_BUSINESS;                            Name : cBusiness ),
        ( Key: (* $00000007 *) PRODUCT_STANDARD_SERVER;                     Name : 'Server Standard Edition (full installation)' ),
        ( Key: (* $00000008 *) PRODUCT_DATACENTER_SERVER;                   Name : 'Server Datacenter Edition (full installation)' ),
        ( Key: (* $00000009 *) PRODUCT_SMALLBUSINESS_SERVER;                Name : 'Small Business Server' ),
        ( Key: (* $0000000A *) PRODUCT_ENTERPRISE_SERVER;                   Name : cServerEnterpriseEdition + '(full installation)' ),
        ( Key: (* $0000000B *) PRODUCT_STARTER;                             Name : 'Starter Edition' ),
        ( Key: (* $0000000C *) PRODUCT_DATACENTER_SERVER_CORE;              Name : 'Server Datacenter Edition (core installation)' ),
        ( Key: (* $0000000D *) PRODUCT_STANDARD_SERVER_CORE;                Name : 'Server Standard Edition (core installation)' ),
        ( Key: (* $0000000E *) PRODUCT_ENTERPRISE_SERVER_CORE;              Name : cServerEnterpriseEdition + '(core installation)' ),
        ( Key: (* $0000000F *) PRODUCT_ENTERPRISE_SERVER_IA64;              Name : cServerEnterpriseEdition + 'for Itanium-based Systems' ),

        ( Key: (* $00000010 *) PRODUCT_BUSINESS_N;                          Name : cBusiness ),
        ( Key: (* $00000011 *) PRODUCT_WEB_SERVER;                          Name : cWebServer ),
        ( Key: (* $00000012 *) PRODUCT_CLUSTER_SERVER;                      Name : 'Cluster Server Edition' ),
        ( Key: (* $00000013 *) PRODUCT_HOME_SERVER;                         Name : 'Home Server Edition' ),
        ( Key: (* $00000014 *) PRODUCT_STORAGE_EXPRESS_SERVER;              Name : cStorageServer + 'Express Edition' ),
        ( Key: (* $00000015 *) PRODUCT_STORAGE_STANDARD_SERVER;             Name : cStorageServer + 'Standard Edition' ),
        ( Key: (* $00000016 *) PRODUCT_STORAGE_WORKGROUP_SERVER;            Name : cStorageServer + 'Workgroup Edition' ),
        ( Key: (* $00000017 *) PRODUCT_STORAGE_ENTERPRISE_SERVER;           Name : cStorageServer + 'Enterprise Edition' ),
        ( Key: (* $00000018 *) PRODUCT_SERVER_FOR_SMALLBUSINESS;            Name : 'Server for Small Business Edition' ),
        ( Key: (* $00000019 *) PRODUCT_SMALLBUSINESS_SERVER_PREMIUM;        Name : 'Small Business Server Premium Edition' ),
        ( Key: (* $0000001A *) PRODUCT_HOME_PREMIUM_N;                      Name : cHomePremium + 'Edition' ),
        ( Key: (* $0000001B *) PRODUCT_ENTERPRISE_N;                        Name : cEnterpriseEd ),
        ( Key: (* $0000001C *) PRODUCT_ULTIMATE_N;                          Name : cUltimate ),
        ( Key: (* $0000001D *) PRODUCT_WEB_SERVER_CORE;                     Name : cWebServer + ' (core installation)' ),
        ( Key: (* $0000001E *) PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT;    Name : 'Windows Essential Business Server Management Server Edition' ),
        ( Key: (* $0000001F *) PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY;      Name : 'Windows Essential Business Server Security Server Edition' ),

        ( Key: (* $00000020 *) PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING;     Name : 'Windows Essential Business Server Messaging Server Edition' ),
        ( Key: (* $00000021 *) PRODUCT_SERVER_FOUNDATION;                   Name : 'Server Foundation'              ),
        ( Key: (* $00000022 *) PRODUCT_HOME_PREMIUM_SERVER;                 Name : cHomePremium + 'Server Edition'  ),
        ( Key: (* $00000023 *) PRODUCT_SERVER_FOR_SMALLBUSINESS_V;          Name : 'SERVER_FOR_SMALLBUSINESS_V'     ),
        ( Key: (* $00000024 *) PRODUCT_STANDARD_SERVER_V;                   Name : 'STANDARD_SERVER_V'              ),
        ( Key: (* $00000025 *) PRODUCT_DATACENTER_SERVER_V;                 Name : 'DATACENTER_SERVER_V'            ),
        ( Key: (* $00000026 *) PRODUCT_ENTERPRISE_SERVER_V;                 Name : 'ENTERPRISE_SERVER_V'            ),
        ( Key: (* $00000027 *) PRODUCT_DATACENTER_SERVER_CORE_V;            Name : 'DATACENTER_SERVER_CORE_V'       ),
        ( Key: (* $00000028 *) PRODUCT_STANDARD_SERVER_CORE_V;              Name : 'STANDARD_SERVER_CORE_V'         ),
        ( Key: (* $00000029 *) PRODUCT_ENTERPRISE_SERVER_CORE_V;            Name : 'ENTERPRISE_SERVER_CORE_V'       ),
        ( Key: (* $0000002A *) PRODUCT_HYPERV;                              Name : 'Hyper-V Server Edition'         ),
        ( Key: (* $0000002B *) PRODUCT_STORAGE_EXPRESS_SERVER_CORE;         Name : 'STORAGE_EXPRESS_SERVER_CORE'    ),
        ( Key: (* $0000002C *) PRODUCT_STORAGE_STANDARD_SERVER_CORE;        Name : 'STORAGE_STANDARD_SERVER_CORE'   ),
        ( Key: (* $0000002D *) PRODUCT_STORAGE_WORKGROUP_SERVER_CORE;       Name : 'STORAGE_WORKGROUP_SERVER_CORE'  ),
        ( Key: (* $0000002E *) PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE;      Name : 'STORAGE_ENTERPRISE_SERVER_CORE' ),
        ( Key: (* $0000002F *) PRODUCT_STARTER_N;                           Name : 'STARTER_N' ),

        ( Key: (* $00000030 *) PRODUCT_PROFESSIONAL;                        Name : cProfessional                       ),
        ( Key: (* $00000031 *) PRODUCT_PROFESSIONAL_N;                      Name : cProfessional                       ),
        ( Key: (* $00000032 *) PRODUCT_SB_SOLUTION_SERVER;                  Name : 'SB_SOLUTION_SERVER'                ),
        ( Key: (* $00000033 *) PRODUCT_SERVER_FOR_SB_SOLUTIONS;             Name : 'SERVER_FOR_SB_SOLUTIONS'           ),
        ( Key: (* $00000034 *) PRODUCT_STANDARD_SERVER_SOLUTIONS;           Name : 'STANDARD_SERVER_SOLUTIONS'         ),
        ( Key: (* $00000035 *) PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE;      Name : 'STANDARD_SERVER_SOLUTIONS_CORE'    ),
        ( Key: (* $00000036 *) PRODUCT_SB_SOLUTION_SERVER_EM;               Name : 'SB_SOLUTION_SERVER_EM'             ),
        ( Key: (* $00000037 *) PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM;          Name : 'SERVER_FOR_SB_SOLUTIONS_EM'        ),
        ( Key: (* $00000038 *) PRODUCT_SOLUTION_EMBEDDEDSERVER;             Name : 'SOLUTION_EMBEDDEDSERVER'           ),
        ( Key: (* $00000039 *) PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE;        Name : 'SOLUTION_EMBEDDEDSERVER_CORE'      ),
        ( Key: (* $0000003B *) PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT;       Name : 'ESSENTIALBUSINESS_SERVER_MGMT'     ),
        ( Key: (* $0000003C *) PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL;       Name : 'ESSENTIALBUSINESS_SERVER_ADDL'     ),
        ( Key: (* $0000003D *) PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC;    Name : 'ESSENTIALBUSINESS_SERVER_MGMTSVC'  ),
        ( Key: (* $0000003E *) PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC;    Name : 'ESSENTIALBUSINESS_SERVER_ADDLSVC'  ),
        ( Key: (* $0000003F *) PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE;   Name : 'SMALLBUSINESS_SERVER_PREMIUM_CORE' ),

        ( Key: (* $00000040 *) PRODUCT_CLUSTER_SERVER_V;                    Name : 'CLUSTER_SERVER_V'                            ),
        ( Key: (* $00000041 *) PRODUCT_EMBEDDED;                            Name : 'EMBEDDED'                                    ),
        ( Key: (* $00000042 *) PRODUCT_STARTER_E;                           Name : 'STARTER_E'                                   ),
        ( Key: (* $00000043 *) PRODUCT_HOME_BASIC_E;                        Name : 'HOME_BASIC_E'                                ),
        ( Key: (* $00000044 *) PRODUCT_HOME_PREMIUM_E;                      Name : 'HOME_PREMIUM_E'                              ),
        ( Key: (* $00000045 *) PRODUCT_PROFESSIONAL_E;                      Name : 'PROFESSIONAL_E'                              ),
        ( Key: (* $00000046 *) PRODUCT_ENTERPRISE_E;                        Name : 'ENTERPRISE_E'                                ),
        ( Key: (* $00000047 *) PRODUCT_ULTIMATE_E;                          Name : 'ULTIMATE_E'                                  ),
        ( Key: (* $00000048 *) PRODUCT_ENTERPRISE_EVALUATION;               Name : 'Enterprise Evaluation'                       ),
        ( Key: (* $0000004C *) PRODUCT_MULTIPOINT_STANDARD_SERVER;          Name : cMultipoint + ' Standard Server'              ),
        ( Key: (* $0000004D *) PRODUCT_MULTIPOINT_PREMIUM_SERVER;           Name : cMultipoint + ' Premium Server'               ),
        ( Key: (* $0000004F *) PRODUCT_STANDARD_EVALUATION_SERVER;          Name : 'Server Standard (evaluation installation)'   ),

        ( Key: (* $00000050 *) PRODUCT_DATACENTER_EVALUATION_SERVER;        Name : 'Server Datacenter (evaluation installation)' ),
        ( Key: (* $00000054 *) PRODUCT_ENTERPRISE_N_EVALUATION;             Name : 'Enterprise N Evaluation'                     ),
        ( Key: (* $00000055 *) PRODUCT_EMBEDDED_AUTOMOTIVE;                 Name : 'EMBEDDED_AUTOMOTIVE'                         ),
        ( Key: (* $00000056 *) PRODUCT_EMBEDDED_INDUSTRY_A;                 Name : 'EMBEDDED_INDUSTRY_A'                         ),
        ( Key: (* $00000057 *) PRODUCT_THINPC;                              Name : 'THINPC'                                      ),
        ( Key: (* $00000058 *) PRODUCT_EMBEDDED_A;                          Name : 'EMBEDDED_A'                                  ),
        ( Key: (* $00000059 *) PRODUCT_EMBEDDED_INDUSTRY;                   Name : 'EMBEDDED_INDUSTRY'                           ),
        ( Key: (* $0000005A *) PRODUCT_EMBEDDED_E;                          Name : 'EMBEDDED_E'                                  ),
        ( Key: (* $0000005B *) PRODUCT_EMBEDDED_INDUSTRY_E;                 Name : 'EMBEDDED_INDUSTRY_E'                         ),
        ( Key: (* $0000005C *) PRODUCT_EMBEDDED_INDUSTRY_A_E;               Name : 'EMBEDDED_INDUSTRY_A_E'                       ),
        ( Key: (* $0000005F *) PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER; Name : 'STORAGE_WORKGROUP_EVALUATION_SERVER'         ),

        ( Key: (* $00000060 *) PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER;  Name : 'STORAGE_STANDARD_EVALUATION_SERVER'          ),
        ( Key: (* $00000061 *) PRODUCT_CORE_ARM;                            Name : 'CORE_ARM'                                    ),
        ( Key: (* $00000062 *) PRODUCT_CORE_N;                              Name : 'CORE_N'                                      ),
        ( Key: (* $00000063 *) PRODUCT_CORE_COUNTRYSPECIFIC;                Name : 'CORE_COUNTRYSPECIFIC'                        ),
        ( Key: (* $00000064 *) PRODUCT_CORE_SINGLELANGUAGE;                 Name : 'CORE_SINGLELANGUAGE'                         ),
        ( Key: (* $00000065 *) PRODUCT_CORE;                                Name : 'CORE'                                        ),
        ( Key: (* $00000067 *) PRODUCT_PROFESSIONAL_WMC;                    Name : cProfessional + ' with Media Center'          ),
        ( Key: (* $00000068 *) PRODUCT_MOBILE_CORE;                         Name : 'MOBILE_CORE'                                 ),
        ( Key: (* $00000069 *) PRODUCT_EMBEDDED_INDUSTRY_EVAL;              Name : 'EMBEDDED_INDUSTRY_EVAL'                      ),
        ( Key: (* $0000006A *) PRODUCT_EMBEDDED_INDUSTRY_E_EVAL;            Name : 'EMBEDDED_INDUSTRY_E_EVAL'                    ),
        ( Key: (* $0000006B *) PRODUCT_EMBEDDED_EVAL;                       Name : 'EMBEDDED_EVAL'                               ),
        ( Key: (* $0000006C *) PRODUCT_EMBEDDED_E_EVAL;                     Name : 'EMBEDDED_E_EVAL'                             ),
        ( Key: (* $0000006D *) PRODUCT_NANO_SERVER;                         Name : 'NANO_SERVER'                                 ),
        ( Key: (* $0000006E *) PRODUCT_CLOUD_STORAGE_SERVER;                Name : 'CLOUD_STORAGE_SERVER'                        ),
        ( Key: (* $0000006F *) PRODUCT_CORE_CONNECTED;                      Name : 'CORE_CONNECTED'                              ),

        ( Key: (* $00000070 *) PRODUCT_PROFESSIONAL_STUDENT;                Name : 'PROFESSIONAL_STUDENT'                        ),
        ( Key: (* $00000071 *) PRODUCT_CORE_CONNECTED_N;                    Name : 'CORE_CONNECTED_N'                            ),
        ( Key: (* $00000072 *) PRODUCT_PROFESSIONAL_STUDENT_N;              Name : 'PROFESSIONAL_STUDENT_N'                      ),
        ( Key: (* $00000073 *) PRODUCT_CORE_CONNECTED_SINGLELANGUAGE;       Name : 'CORE_CONNECTED_SINGLELANGUAGE'               ),
        ( Key: (* $00000074 *) PRODUCT_CORE_CONNECTED_COUNTRYSPECIFIC;      Name : 'CORE_CONNECTED_COUNTRYSPECIFIC'              ),
        ( Key: (* $00000075 *) PRODUCT_CONNECTED_CAR;                       Name : 'CONNECTED_CAR'                               ),
        ( Key: (* $00000076 *) PRODUCT_INDUSTRY_HANDHELD;                   Name : 'INDUSTRY_HANDHELD'                           ),
        ( Key: (* $00000077 *) PRODUCT_PPI_PRO;                             Name : 'PPI_PRO'                                     ),
        ( Key: (* $00000078 *) PRODUCT_ARM64_SERVER;                        Name : 'ARM64_SERVER'                                ),
        ( Key: (* $00000079 *) PRODUCT_EDUCATION;                           Name : 'EDUCATION'                                   ),
        ( Key: (* $0000007A *) PRODUCT_EDUCATION_N;                         Name : 'EDUCATION_N'                                 ),
        ( Key: (* $0000007B *) PRODUCT_IOTUAP;                              Name : 'IOTUAP'                                      ),
        ( Key: (* $0000007C *) PRODUCT_CLOUD_HOST_INFRASTRUCTURE_SERVER;    Name : 'CLOUD_HOST_INFRASTRUCTURE_SERVER'            ),
        ( Key: (* $0000007D *) PRODUCT_ENTERPRISE_S;                        Name : 'ENTERPRISE_S'                                ),
        ( Key: (* $0000007E *) PRODUCT_ENTERPRISE_S_N;                      Name : 'ENTERPRISE_S_N'                              ),
        ( Key: (* $0000007F *) PRODUCT_PROFESSIONAL_S;                      Name : 'PROFESSIONAL_S'                              ),

        ( Key: (* $00000080 *) PRODUCT_PROFESSIONAL_S_N;                    Name : 'PROFESSIONAL_S_N'                            ),
        ( Key: (* $00000081 *) PRODUCT_ENTERPRISE_S_EVALUATION;             Name : 'ENTERPRISE_S_EVALUATION'                     ),
        ( Key: (* $00000082 *) PRODUCT_ENTERPRISE_S_N_EVALUATION;           Name : 'ENTERPRISE_S_N_EVALUATION'                   ),
        ( Key: (* $00000087 *) PRODUCT_HOLOGRAPHIC;                         Name : 'HOLOGRAPHIC'                                 ),
        ( Key: (* $00000088 *) PRODUCT_HOLOGRAPHIC_BUSINESS;                Name : 'HOLOGRAPHIC BUSINESS'                        ),

        ( Key: (* $000000AF *) PRODUCT_SERVERRDSH;                          Name : 'SERVER RDSH'                                 ),

        ( Key: (* $ABCDABCD *) PRODUCT_UNLICENSED;                          Name : 'Unlicensed'                                  )
);

function ProductForKey( const AKey: Cardinal ): string;
begin
   Result := BinarySearch( ProductsDict, AKey );
end ;

// ProcessorArchitecture
// https://learn.microsoft.com/en-us/uwp/api/windows.system.processorarchitecture?view=winrt-26100
const PROCESSOR_ARCHITECTURE_X86                  = $0000;    // The x86 processor architecture.
const PROCESSOR_ARCHITECTURE_ARM                  = $0005;    // The ARM processor architecture.
const PROCESSOR_ARCHITECTURE_IA64                 = $0006;    // Intel Itanium-based  architecture.
const PROCESSOR_ARCHITECTURE_X64                  = $0009;    // The x64 processor architecture (AMD or Intel).
const PROCESSOR_ARCHITECTURE_NEUTRAL              = $000B;    // A neutral processor architecture.
const PROCESSOR_ARCHITECTURE_ARM64                = $000C;    // The Arm64 processor architecture
const PROCESSOR_ARCHITECTURE_X86ONARM64           = $000E;    // The Arm64 processor architecture emulating the X86 architecture
const PROCESSOR_ARCHITECTURE_UNKNOWN              = $FFFF;    // An unknown processor architecture.

const MAX_ARCHIT = 8;
const ArchitecturesDict: array[ 0..MAX_ARCHIT-1 ] of TKeyValueRec = (
        ( Key: (*   $0000   *) PROCESSOR_ARCHITECTURE_X86;                  Name : '32-bit'              ),
        ( Key: (*   $0005   *) PROCESSOR_ARCHITECTURE_ARM;                  Name : 'ARM'                 ),
        ( Key: (*   $0006   *) PROCESSOR_ARCHITECTURE_IA64;                 Name : 'Intel Itanium-based' ),
        ( Key: (*   $0009   *) PROCESSOR_ARCHITECTURE_X64;                  Name : '64-bit'              ),
        ( Key: (*   $000B   *) PROCESSOR_ARCHITECTURE_NEUTRAL;              Name : 'NEUTRAL'             ),
        ( Key: (*   $000C   *) PROCESSOR_ARCHITECTURE_ARM64;                Name : 'ARM64'               ),
        ( Key: (*   $000E   *) PROCESSOR_ARCHITECTURE_X86ONARM64;           Name : 'ARM64_Emu_X86'       ),
        ( Key: (*   $FFFF   *) PROCESSOR_ARCHITECTURE_UNKNOWN;              Name : 'UNKNOWN'             )
);

function ArchitectureForKey( const AKey: Cardinal ): string;
begin
   Result := BinarySearch( ArchitecturesDict, AKey );
end ;


// Suite mask (wStiteMask)
const VER_SUITE_SMALLBUSINESS                     = Windows.VER_SUITE_SMALLBUSINESS;             // $00000001;
const VER_SUITE_ENTERPRISE                        = Windows.VER_SUITE_ENTERPRISE;                // $00000002;
const VER_SUITE_BACKOFFICE                        = Windows.VER_SUITE_BACKOFFICE;                // $00000004;
const VER_SUITE_COMMUNICATIONS                    = Windows.VER_SUITE_COMMUNICATIONS;            // $00000008;
const VER_SUITE_TERMINAL                          = Windows.VER_SUITE_TERMINAL;                  // $00000010;
const VER_SUITE_SMALLBUSINESS_RESTRICTED          = Windows.VER_SUITE_SMALLBUSINESS_RESTRICTED;  // $00000020;
const VER_SUITE_EMBEDDEDNT                        = Windows.VER_SUITE_EMBEDDEDNT;                // $00000040;
const VER_SUITE_DATACENTER                        = Windows.VER_SUITE_DATACENTER;                // $00000080;
const VER_SUITE_SINGLEUSERTS                      = Windows.VER_SUITE_SINGLEUSERTS;              // $00000100;
const VER_SUITE_PERSONAL                          = Windows.VER_SUITE_PERSONAL;                  // $00000200;
const VER_SUITE_BLADE                             = Windows.VER_SUITE_BLADE;                     // $00000400;
const VER_SUITE_EMBEDDED_RESTRICTED               = Windows.VER_SUITE_EMBEDDED_RESTRICTED;       // $00000800;

const VER_SUITE_SECURITY_APPLIANCE                = $00001000;
const VER_SUITE_STORAGE_SERVER                    = $00002000;
const VER_SUITE_COMPUTE_SERVER                    = $00004000;
const VER_SUITE_WH_SERVER                         = $00008000;
const VER_SUITE_MULTIUSERTS                       = $00020000;

const MAX_SUITES = 17;
const SuitesDict: array[ 0..MAX_SUITES-1 ] of TKeyValueRec = (
        ( Key: (* $00000001 *) VER_SUITE_SMALLBUSINESS;                     Name : 'Microsoft Small Business Server was once installed on the system, but may have been upgraded to another version of Windows. For more information about this flag bit, see the following Remarks section.' ),
        ( Key: (* $00000002 *) VER_SUITE_ENTERPRISE;                        Name : 'Windows Server 2008 Enterprise, Windows Server 2003, Enterprise Edition, or Windows 2000 Advanced Server is installed.' ),
        ( Key: (* $00000004 *) VER_SUITE_BACKOFFICE;                        Name : 'Microsoft BackOffice components are installed.' ),
        ( Key: (* $00000008 *) VER_SUITE_COMMUNICATIONS;                    Name : 'SUITE_COMMUNICATIONS' ),
        ( Key: (* $00000010 *) VER_SUITE_TERMINAL;                          Name : 'Terminal Services is installed. ' + sLineBreak +
                                                                                   'This value is always set. ' + sLineBreak +
                                                                                   'If VER_SUITE_TERMINAL is set but VER_SUITE_SINGLEUSERTS is not set, the operating system is running in application server mode.' ),
        ( Key: (* $00000020 *) VER_SUITE_SMALLBUSINESS_RESTRICTED;          Name : 'SUITE_SMALLBUSINESS_RESTRICTED' ),
        ( Key: (* $00000040 *) VER_SUITE_EMBEDDEDNT;                        Name : 'Windows XP Embedded is installed.' ),
        ( Key: (* $00000080 *) VER_SUITE_DATACENTER;                        Name : 'Windows Server 2008 Datacenter, Windows Server 2003, Datacenter Edition, or Windows 2000 Datacenter Server is installed.' ),
        ( Key: (* $00000100 *) VER_SUITE_SINGLEUSERTS;                      Name : 'Remote Desktop is supported, but only one interactive session is supported. This value is set unless the system is running in application server mode.' ),
        ( Key: (* $00000200 *) VER_SUITE_PERSONAL;                          Name : 'Windows Vista Home Premium, Windows Vista Home Basic, or Windows XP Home Edition is installed.' ),
        ( Key: (* $00000400 *) VER_SUITE_BLADE;                             Name : 'Windows Server 2003, Web Edition is installed.' ),
        ( Key: (* $00000800 *) VER_SUITE_EMBEDDED_RESTRICTED;               Name : 'SUITE_EMBEDDED_RESTRICTED' ),
        ( Key: (* $00001000 *) VER_SUITE_SECURITY_APPLIANCE;                Name : 'SUITE_SECURITY_APPLIANCE' ),
        ( Key: (* $00002000 *) VER_SUITE_STORAGE_SERVER;                    Name : 'Windows Storage Server 2003 R2 or Windows Storage Server 2003 is installed. ' ),
        ( Key: (* $00004000 *) VER_SUITE_COMPUTE_SERVER;                    Name : 'Windows Server 2003, Compute Cluster Edition is installed. ' ),
        ( Key: (* $00008000 *) VER_SUITE_WH_SERVER;                         Name : 'Windows Home Server' ),
        ( Key: (* $00020000 *) VER_SUITE_MULTIUSERTS;                       Name : 'Multi-user' )
);

procedure GetSuitesList( AKey: Cardinal; ASuites: TStrings) ;
  var i: integer;
begin
   ASuites.Clear;
   for i:=Low(SuitesDict) to High(SuitesDict) do begin
       if (AKey and SuitesDict[i].Key) = SuitesDict[i].Key
          then ASuites.Add( SuitesDict[i].Name );
   end;
end;

//-------------------------------------------------------------------------------------------
type TWin10orLaterReleaseInfo = record
       MinBuild : LongWord;
       Version  : String;  // Np. '11' lub '10'
       ReleaseId: String;  // Np. '24H2', '22H2'
end;

// Tablica z wersjami, KONIECZNIE posortowana od najwyższego numeru kompilacji do najniższego
const MAX_RELEASES = 20;
const asWindowsRelease: array[ 0..MAX_RELEASES-1 ] of TWin10orLaterReleaseInfo = (
      ( MinBuild: 26200;  Version: '11';  ReleaseId: '25H2' ),
      ( MinBuild: 26100;  Version: '11';  ReleaseId: '24H2' ),
      ( MinBuild: 22631;  Version: '11';  ReleaseId: '23H2' ),
      ( MinBuild: 22621;  Version: '11';  ReleaseId: '22H2' ),
      ( MinBuild: 22000;  Version: '11';  ReleaseId: '21H2' ),
      ( MinBuild: 19045;  Version: '10';  ReleaseId: '22H2' ),
      ( MinBuild: 19044;  Version: '10';  ReleaseId: '21H2' ),
      ( MinBuild: 19043;  Version: '10';  ReleaseId: '21H1' ),
      ( MinBuild: 19042;  Version: '10';  ReleaseId: '20H2' ),
      ( MinBuild: 19041;  Version: '10';  ReleaseId: '2004' ),
      ( MinBuild: 18363;  Version: '10';  ReleaseId: '1909' ),
      ( MinBuild: 18362;  Version: '10';  ReleaseId: '1903' ),
      ( MinBuild: 17763;  Version: '10';  ReleaseId: '1809' ),
      ( MinBuild: 17134;  Version: '10';  ReleaseId: '1803' ),
      ( MinBuild: 16299;  Version: '10';  ReleaseId: '1709' ),
      ( MinBuild: 15063;  Version: '10';  ReleaseId: '1703' ),
      ( MinBuild: 14393;  Version: '10';  ReleaseId: '1607' ),
      ( MinBuild: 10586;  Version: '10';  ReleaseId: '1511' ),
      ( MinBuild: 10240;  Version: '10';  ReleaseId: '1507' ),
      ( MinBuild: 0;      Version: '10';  ReleaseId: '????' ) // Wartość domyślna
);

procedure GetWin1xReleaseInfo( const ABuildNumber: Cardinal; out XVersion, XReleaseId: string );
  var i: Integer;
begin
   // Pętla znajduje pierwszą pasującą wersję (dlatego tablica musi być posortowana malejąco)
   for i := Low(asWindowsRelease) to High(asWindowsRelease) do begin
       with asWindowsRelease[i] do begin
           if ABuildNumber >= MinBuild then begin
              XVersion   := Version;
              XReleaseId := ReleaseId;
              Exit;
           end ;
      end;
   end;
   with asWindowsRelease[High(asWindowsRelease)] do begin
       XVersion   := Version;
       XReleaseId := ReleaseId;
   end ;
end;



end.

