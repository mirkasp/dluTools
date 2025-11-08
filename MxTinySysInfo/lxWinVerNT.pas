unit lxWinVerNT;

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

uses Classes
   , lxWinVer4
   , JwaWindows;

{ TWindowsNTVersion }
type TWindowsNTVersion = class( TWindowsVersion )
   strict private
      FServicePackMajor     : Word;
      FServicePackMinor     : Word;
      FSuiteMask            : Word;
      FProductType          : Byte;
      FProductInfo          : Cardinal;
      FProcessorArchitecture: Word;
      FMediaCenter          : boolean;
      FTabletPC             : boolean;

      FBuildLab             : string;        // TRegistry_ReadString(Reg, 'BuildLab');
      FBuildLabEx           : string;        // TRegistry_ReadString(Reg, 'BuildLabEx');
      FCSDBuildNumber       : string;        // TRegistry_ReadString(Reg, 'CSDBuildNumber');
      FCSDVersionReg        : string;        // TRegistry_ReadString(Reg, 'CSDVersion');
      FCurrentBuildNumber   : string;        // TRegistry_ReadString(Reg, 'CurrentBuildNumber');
      FCurrentVersion       : string;        // TRegistry_ReadString(Reg, 'CurrentVersion');
      FEditionId            : string;        // TRegistry_ReadString(Reg, 'EditionId');
      FProductName          : string;        // TRegistry_ReadString(Reg, 'ProductName');
      FProductId            : String;        // TRegistry_ReadString( 'ProductId'          );
      FRegisteredOwner      : String;        // TRegistry_ReadString( 'RegisteredOwner'    );
      FSystemRoot           : String;        // TRegistry_ReadString( 'SystemRoot'         );
      FReleaseId            : string;        // TRegistry_ReadString(Reg, 'ReleaseId');
      FUBR                  : integer;       // TRegistry_ReadInteger(Reg, 'UBR');
      FInstallDate          : TDateTime;

      FSuites               : TStrings;
      //
      procedure Prepare_Win32_NT_Platform;
      procedure FetchVersionDataFromAPI(out osi: TOSVersionInfoExW);
      procedure FetchAdditionalSystemDetails;
      function GetProductFromRegistry(): String;
      procedure TryReadFromRegistry;
      procedure IdentifyWinNT4AndEarlier();
      procedure IdentifyWin2000AndXP();
      procedure IdentifyWinVistaTo8(const osi: TOSVersionInfoExW);
      procedure IdentifyWin10AndLater(const osi: TOSVersionInfoExW);
      function UnknownSystem( const AParam: TOSVersionInfoExW ): String;
      procedure AppendToNameIf( const ACond: boolean; const AText_true, AText_false: String );
      function GetWindowsRelease: String;
   protected
      function GetCompilationInfo: String; override;
      function GetTechnicalInfo: String; override;
   public
      constructor Create(); override;
      destructor Destroy; override;
      function GetFullName(): string; override;
      procedure AllProperties( AStrings: TStrings) ; override;
      //
      property ServicePackMajor     : Word       read FServicePackMajor;
      property ServicePackMinor     : Word       read FServicePackMinor;
      property SuiteMask            : Word       read FSuiteMask;
      property ProductType          : Byte       read FProductType;
      property ProductInfo          : Cardinal   read FProductInfo;
      property ProcessorArchitecture: Word       read FProcessorArchitecture;
      property MediaCenter          : boolean    read FMediaCenter;
      property TabletPC             : boolean    read FTabletPC;
      //
      property BuildLab             : string     read FBuildLab;
      property BuildLabEx           : string     read FBuildLabEx;
      property CSDBuildNumber       : string     read FCSDBuildNumber;
      property CSDVersionReg        : string     read FCSDVersionReg;
      property CurrentBuildNumber   : string     read FCurrentBuildNumber;
      property CurrentVersion       : string     read FCurrentVersion;
      property EditionId            : string     read FEditionId;
      property ProductName          : string     read FProductName;
      property ProductId            : string     read FProductId;
      property RegisteredOwner      : string     read FRegisteredOwner;
      property SystemRoot           : string     read FSystemRoot;
      property ReleaseId            : string     read FReleaseId;
      property UBR                  : integer    read FUBR;
      property InstallDate          : TDateTime  read FInstallDate;
      //
      property WindowsRelease       : String     read GetWindowsRelease;

end;

implementation

uses SysUtils
   , Windows
   , Registry
   , DateUtils
   , lxKeyValueArray
   ;

const CLIB_KERNEL32 = 'Kernel32.dll';
//const cLib_Ntdll    = 'ntdll.dll';

const VER_SUITE_WH_SERVER                         = $00008000;

//
//  https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getversionexw
//
function GetVersionEx( var lpVersionInformation: TOSVersionInfoExW ): LongInt; stdcall; external CLIB_KERNEL32 name 'GetVersionExW';

//
//  https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getproductinfo
//
function GetProductInfo(dwOSMajorVersion, dwOSMinorVersion, dwSpMajorVersion, dwSpMinorVersion: DWORD; var pdwReturnedProductType: DWORD): BOOL; stdcall; external CLIB_KERNEL32 name 'GetProductInfo';

{ TWindowsNTVersion }

constructor TWindowsNTVersion.Create();
begin
   inherited;
   FPlatformId := VER_PLATFORM_WIN32_NT;
   FSuites     := TStringList.Create;
   Prepare_Win32_NT_Platform;
end;

destructor TWindowsNTVersion.Destroy;
begin
   FSuites.Free;
   inherited Destroy;
end;

function TWindowsNTVersion.GetFullName(): string;
begin
   AppendToName( Lang );
   Result := FTempName;
end;

procedure TWindowsNTVersion.AllProperties( AStrings: TStrings) ;
begin
   if Assigned( AStrings ) then begin
      inherited AllProperties( AStrings);
      with AStrings do begin
          BeginUpdate;
          AddPair( 'ServicePackMajor',       IntToStr( FServicePackMajor ) );
          AddPair( 'ServicePackMinor',       IntToStr( FServicePackMinor ) );
          AddPair( 'SuiteMask',              IntToStr( FSuiteMask ) );
          AddPair( 'ProcessorArchitecture',  Format( '%d [%s]',
                                                     [ FProcessorArchitecture,
                                                       ArchitectureForKey( FProcessorArchitecture )
                                                     ] ) );
          AddPair( 'ProductType',            IntToStr( FProductType ) );
          AddPair( 'ProductInfo',            Format( '%d [%s]',
                                                     [ FProductInfo,
                                                       ProductForKey( FProductInfo)
                                                     ] ) );
          AddPair( 'MediaCenter',            IntToStr( Integer( FMediaCenter )));
          AddPair( 'TabletPC',               IntToStr( Integer( FTabletPC ))   );
          AddPair( 'Suites',                 FSuites.Text                      );
          AddPair( 'Reg.BuildLab',           FBuildLab                         );
          AddPair( 'Reg.BuildLabEx',         FBuildLabEx                       );
          AddPair( 'Reg.CSDBuildNumber',     FCSDBuildNumber                   );
          AddPair( 'Reg.CSDVersion',         FCSDVersionReg                    );
          AddPair( 'Reg.CurrentBuildNumber', FCurrentBuildNumber               );
          AddPair( 'Reg.CurrentVersion',     FCurrentVersion                   );
          AddPair( 'Reg.InstallDate',        DateTimeToStr( fInstallDate, true ) );
          AddPair( 'Reg.EditionId',          FEditionId                        );
          AddPair( 'Reg.ProductId',          FProductId                        );
          AddPair( 'Reg.ProductName',        FProductName                      );
          AddPair( 'Reg.RegisteredOwner',    FRegisteredOwner                  );
          AddPair( 'Reg.ReleaseId',          FReleaseId                        );
          AddPair( 'Reg.SystemRoot',         FSystemRoot                       );
          AddPair( 'Reg.UBR',                IntToStr( fUBR )                  );
          EndUpdate;
      end;
   end;

end ;

procedure TWindowsNTVersion.Prepare_Win32_NT_Platform;
  var osi : TOSVersionInfoExW;
begin
   FetchVersionDataFromAPI(osi);
   case FMajorVersion of
       0..4: IdentifyWinNT4AndEarlier;
          5: IdentifyWin2000AndXP;
          6: IdentifyWinVistaTo8( osi );
         10: IdentifyWin10AndLater( osi );
       else  FTempName := UnknownSystem( osi );
   end;

   if FMajorVersion > 4 then begin
      FetchAdditionalSystemDetails;
   end;

end;

procedure TWindowsNTVersion.FetchVersionDataFromAPI(out osi: TOSVersionInfoExW);
begin
   osi := Default( TOSVersionInfoExW );
   osi.dwOSVersionInfoSize := SizeOf( osi );
   if GetVersionEx( osi ) = 0 then begin
      self.FTempName := 'Unknown';
      Exit;
   end;

   // informacje mogą być niepoprawne dla Windows 10, jesli nie ma ustawionego manifestu
   // skorzystajmy więc z funkcji RtlGetVersion
   if osi.dwMajorVersion >= 6  then begin
      RtlGetVersion( @osi );
   end ;

   FTempName         := 'Windows';
   FMajorVersion     := osi.dwMajorVersion;
   FMinorVersion     := osi.dwMinorVersion;
   FBuildNumber      := osi.dwBuildNumber;
   FPlatformId       := osi.dwPlatformId;
   FCSDVersion       := osi.szCSDVersion;

   FServicePackMajor := osi.wServicePackMajor;
   FServicePackMinor := osi.wServicePackMinor ;
   FSuiteMask        := osi.wSuiteMask;
   FProductType      := osi.wProductType;

end;

procedure TWindowsNTVersion.FetchAdditionalSystemDetails;
  var nsi: TSystemInfo;
begin
   if GetProductInfo( FMajorVersion, FMinorVersion, FServicePackMajor, FServicePackMajor, FProductInfo ) then begin
      AppendToName( ProductForKey( FProductInfo ) );

   end else if FProductType = VER_NT_WORKSTATION then begin
      case FSuiteMask of
         512 : AppendToName( 'Personal' );
         768 : AppendToName( 'Home Premium' );
         else  AppendToName( 'Professional' );
      end;

   end else if FProductType = VER_NT_DOMAIN_CONTROLLER then begin
      AppendToName( 'Domain Controller' );

   end else if FProductType = VER_NT_SERVER then begin
      if fSuiteMask = VER_SUITE_DATACENTER then begin
         AppendToName( 'DataCenter Server' )
      end else if fSuiteMask = VER_SUITE_ENTERPRISE then begin
         AppendToName( 'Advanced Server' )
      end else
         AppendToName( 'Server' );

   end else begin
      AppendToName( GetProductFromRegistry() );

   end;

   nsi := Default( TSystemInfo );
   GetNativeSystemInfo( @nsi );
   FProcessorArchitecture := nsi.wProcessorArchitecture;
   FMediaCenter := GetSystemMetrics( SM_MEDIACENTER ) <> 0;
   FTabletPC    := GetSystemMetrics( SM_TABLETPC ) <> 0;

   AppendToName( ArchitectureForKey( FProcessorArchitecture ) );
   AppendToNameIf( fMediaCenter, 'Media Center', '' );
   AppendToNameIf( fTabletPC, 'Tablet PC', '' );

   GetSuitesList( FSuiteMask, FSuites );
end;

function TWindowsNTVersion.GetProductFromRegistry(): String;
  const cRegKey = 'SYSTEM\CurrentControlSet\' + 'Control\ProductOptions';
begin
   Result := '';
   with TRegistry.Create( KEY_READ ) do begin
      try
         RootKey := HKEY_LOCAL_MACHINE;
         if OpenKeyReadOnly( cRegKey ) then begin
            try
               Result := UpperCase( ReadString( 'ProductType' ) );
               if Result = 'WINNT'    then Result := 'Workstation';
               if Result = 'SERVERNT' then Result := 'Server';
            finally
               CloseKey;
            end;
         end;
      finally
         Free;
      end;
   end ;
end;

procedure TWindowsNTVersion.TryReadFromRegistry;
  const cNoSecsInDay  = 86400;
  const cRegKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
begin
   // We need to read the real registry, not the 32 bit view, because some of the entries
   // don't exist there.
   with TRegistry.Create( KEY_READ or KEY_WOW64_64KEY ) do begin
      try
         RootKey := HKEY_LOCAL_MACHINE;
         if OpenKeyReadOnly( cRegKey ) then begin
            try
               FBuildLab           := ReadString( 'BuildLab'           );
               FBuildLabEx         := ReadString( 'BuildLabEx'         );
               FCSDBuildNumber     := ReadString( 'CSDBuildNumber'     );
               FCSDVersionReg      := ReadString( 'CSDVersion'         );
               FCurrentBuildNumber := ReadString( 'CurrentBuildNumber' );
               FCurrentVersion     := ReadString( 'CurrentVersion'     );
               FEditionId          := ReadString( 'EditionId'          );
               FProductName        := ReadString( 'ProductName'        );
               FProductId          := ReadString( 'ProductId'          );
               FRegisteredOwner    := ReadString( 'RegisteredOwner'    );
               FSystemRoot         := ReadString( 'SystemRoot'         );
               FReleaseId          := ReadString( 'ReleaseId'          );
               FUBR                := ReadInteger( 'UBR'               );
               // data instalacji jest zapisana jako timestamp Unixa, czyli liczba sekund, które minęły od 1.01.1970.
               FInstallDate        := EncodeDate( 1970, 1, 1 ) + ReadInteger( 'InstallDate' ) / cNoSecsInDay;

             finally
               CloseKey;
            end ;
         end;
     finally
         Free;
     end;
  end;
end ;

procedure TWindowsNTVersion.IdentifyWinNT4AndEarlier();
begin
   AppendToName( ' NT ' + IntToStr( FMajorVersion)+'.'+IntToStr( FMinorVersion) );
end ;

procedure TWindowsNTVersion.IdentifyWin2000AndXP();
  var n: integer;
begin
   //  5.0 => Windows 2000
   //  5.1 => Windows XP
   //  5.2 => Windows XP64 or Windows 2003 Server

   case FMinorVersion of
       0 : begin
              AppendToName( '2000' );
              AppendToNameIf( FProductType = VER_NT_WORKSTATION, 'Professional', 'Server' );
           end;

       1 : begin
              if FProductType = VER_NT_WORKSTATION then begin
                 AppendToName( 'XP' );
                 AppendToNameIf( (FSuiteMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL,
                                 'Home Edition',
                                 'Professional' );
              end else
                 AppendToName( 'Server 2003' );
           end;

       2 : begin
              if ( FProductType = VER_NT_WORKSTATION) then begin
                 AppendToName( 'XP x64 Professional' )
              end else
                 if (FSuiteMask and VER_SUITE_WH_SERVER) = VER_SUITE_WH_SERVER then
                    AppendToName( 'Home Server' )
                 else begin
                    AppendToName( 'Server 2003' );
                    n := GetSystemMetrics( SM_SERVERR2 );
                    AppendToNameIf( n <> 0, 'R2 build '+IntToStr(n), '' );
                 end;
           end
      else AppendToName( '???' );
   end;
end ;

procedure TWindowsNTVersion.IdentifyWinVistaTo8(const osi: TOSVersionInfoExW);
  const cServer = 'Server ';
begin
   //  6.0 => Windows Vista or Windows 2008 Server
   //  6.1 => Windows 7 or Windows 2008 Server R2
   //  6.2 => Windows 8 or Windows Server 2012
   //  6.3 => Windows 8.1 or Windows Server 2012 RS

   case FMinorVersion of
      0 : AppendToNameIf( FProductType = VER_NT_WORKSTATION, 'Vista ', cServer + '2008'    );
      1 : AppendToNameIf( FProductType = VER_NT_WORKSTATION, '7 ',     cServer + '2008 R2' );
      2 : AppendToNameIf( FProductType = VER_NT_WORKSTATION, '8 ',     cServer + '2012'    );
      3 : AppendToNameIf( FProductType = VER_NT_WORKSTATION, '8.1 ',   cServer + '2012 RS' );
     else FTempName := UnknownSystem( osi );
   end {case};

end ;

procedure TWindowsNTVersion.IdentifyWin10AndLater(const osi: TOSVersionInfoExW);
begin
   TryReadFromRegistry( );

   // https://learn.microsoft.com/en-us/windows/release-health/windows11-release-information
   if FMinorVersion = 0 then begin
      if FProductType = VER_NT_WORKSTATION
         then AppendToName( GetWindowsRelease() )
         else AppendToName( 'Server 2016' );
   end else
      FTempName := UnknownSystem( osi );

end;

function TWindowsNTVersion.UnknownSystem(const AParam: TOSVersionInfoExW): String;
begin
   with AParam do begin
      Result := Format( 'Unknown Windows version [%d.%d.%d prod. $%x]', [ dwMajorVersion, dwMinorVersion, dwBuildNumber, wProductType ] );
   end;
end ;

procedure TWindowsNTVersion.AppendToNameIf(const ACond: boolean; const AText_true, AText_false: String);
begin
   if ACond
      then self.AppendToName( AText_true )
      else self.AppendToName( AText_false );
end;

function TWindowsNTVersion.GetWindowsRelease: String;
  var sVersion, sReleaseId: string;
begin
   Result := '';
   // https://learn.microsoft.com/en-us/windows/release-health/windows11-release-information
   if (FMajorVersion = 10) and (FMinorVersion = 0) and (FProductType = VER_NT_WORKSTATION) then begin
       GetWin1xReleaseInfo( FBuildNumber, sVersion, sReleaseId );
       AppendToName( sVersion );
       AppendToName( sReleaseId );
   end;
end;

function TWindowsNTVersion.GetCompilationInfo: String;
  var s : string = '';
begin
   s := '';
   if fUBR > 0 then s := '.' + IntToStr(fUbr);
   Result := Trim( Format( '%d.%d.%d%s %s', [FMajorVersion, FMinorVersion, FBuildNumber, s, FCSDVersion ] ) );
end ;

function TWindowsNTVersion.GetTechnicalInfo: String;
begin
  Result := inherited GetTechnicalInfo +
            Format( ', SP=%d.%d, suite=0x%s, product_type=%d, product_info=%d, lang="%s"',
                    [ FServicePackMajor, FServicePackMinor,
                      IntToHex( FSuiteMask, 2*SizeOf(FSuitemask) ),
                      FProductType, FProductInfo, FLang ] );
end;



end.

