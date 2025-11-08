unit lxWinVer4;

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

{ TWindowsVersion }
type TWindowsVersion = class

   protected
      FPlatformId           : Cardinal;
      FMajorVersion         : Cardinal;
      FMinorVersion         : Cardinal;
      FBuildNumber          : Cardinal;
      FCSDVersion           : String;
      FLang                 : String;
      //
      FTempName : string;
      procedure AppendToName( const txt: string );
      function GetFullName(): string; virtual;
      {%H-}constructor Create(); virtual;
      function GetCompilationInfo(): String; virtual;
      function GetTechnicalInfo(): String; virtual;
   public
      class function GetWinver(): TWindowsVersion; overload;
      //
      procedure AllProperties( AStrings: TStrings ); virtual;
      //
      property FullName             : String     read GetFullName;
      property PlatformId           : Cardinal   read FPlatformId;
      property MajorVersion         : Cardinal   read FMajorVersion;
      property MinorVersion         : Cardinal   read FMinorVersion;
      property BuildNumber          : Cardinal   read FBuildNumber;
      property CSDVersion           : String     read FCSDVersion;
      property Lang                 : String     read FLang;
      //
      property CompilationInfo      : String     read GetCompilationInfo;
      property TechnicalInfo        : String     read GetTechnicalInfo;

end;


implementation

uses SysUtils
   , Windows
   , lxWinVer32
   , lxWinVerNT
   ;

var WindowsVersion: TWindowsVersion = nil;


function GetLocaleInformation( Flag : LCTYPE ): String;
  var Buffer : PWideChar;
      Size   : integer;
begin
   Size := GetLocaleInfoW( LOCALE_USER_DEFAULT, Flag, nil, 0);
   GetMem( Buffer, Size * SizeOf(WideChar) );
   try
      if GetLocaleInfoW( LOCALE_USER_DEFAULT, Flag, Buffer, Size ) <= 0
         then Result := ''
         else Result := UTF8Encode( Copy( Buffer, 1, Size ) );
   finally
      FreeMem(Buffer);
   end;
end;




{ TWindowsVersion }


constructor TWindowsVersion.Create();
begin
   inherited Create();

   FMajorVersion     := Win32MajorVersion;
   FMinorVersion     := Win32MinorVersion;
   FBuildNumber      := Win32BuildNumber;
   FCSDVersion       := Win32CSDVersion;

   FTempName         := '';
   FLang             := GetLocaleInformation( LOCALE_SABBREVCTRYNAME );

end;

class function TWindowsVersion.GetWinver(): TWindowsVersion;
begin
   if Assigned( WindowsVersion ) then Result := WindowsVersion
   else begin
       case Win32Platform of
          // Windows3.1(Win32s)
          VER_PLATFORM_WIN32s        : Result := TWindows32sVersion.Create();
          // Windows9x?
          VER_PLATFORM_WIN32_WINDOWS : Result := TWindows9xVersion.Create();
          // WindowsNT?
          VER_PLATFORM_WIN32_NT      : Result := TWindowsNTVersion.Create();
          // other/unknown?
          else raise Exception.CreateFmt( 'Unknown OS version (%d.%d.%d.%d.%d [%s])',
                                           [Win32Platform, Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion] );
       end;

   end;
end;

function TWindowsVersion.GetCompilationInfo: String;
begin
   Result := Trim( Format( '%d.%d.%d%s %s', [FMajorVersion, FMinorVersion, FBuildNumber, FCSDVersion ] ) );
end;

function TWindowsVersion.GetTechnicalInfo: String;
begin
   Result := Format( '%d.%d.%d, platform=%d, csd="%s"', [ FMajorVersion, FMinorVersion, FBuildNumber, FPlatformId, FCSDVersion ] );
end ;

procedure TWindowsVersion.AppendToName(const txt: string);
  var s : string;
begin
   s := TrimLeft(TrimRight(txt));
   if FTempName = '' then FTempName := s
   else begin
     if FTempName[ Length( FTempName ) ] <> ' ' then FTempName := FTempName + ' ';
     FTempName := FTempName + s;
   end ;
end ;

function TWindowsVersion.GetFullName: string;
begin
   Result := 'Windows';
end;
procedure TWindowsVersion.AllProperties( AStrings: TStrings) ;
begin
   if not Assigned( AStrings ) then Exit;
   with AStrings do begin
      BeginUpdate;
      AddPair( 'PlatformId',             IntToStr( FPlatformId ) );
      AddPair( 'MajorVersion',           IntToStr( FMajorVersion ) );
      AddPair( 'MinorVersion',           IntToStr( FMinorVersion ) );
      AddPair( 'BuildNumber',            IntToStr( FBuildNumber ) );
      AddPair( 'CSDVersion',             UTF8Encode( FCSDVersion ) );
      AddPair( 'Lang',                   UTF8Encode( FLang ) );
      EndUpdate;
   end;
end;

finalization

  if Assigned( WindowsVersion ) then WindowsVersion.Free;

end.

