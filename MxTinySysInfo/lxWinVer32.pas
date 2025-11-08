unit lxWinVer32;

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

uses lxWinVer4;

{ TWindows32sVersion }
type TWindows32sVersion = class( TWindowsVersion )
   public
      constructor Create(); override;
      function GetFullName(): string; override;
end;

{ TWindows9xVersion }
type TWindows9xVersion = class( TWindowsVersion )
   public
      constructor Create(); override;
      function GetFullName(): string; override;
end;


implementation

uses SysUtils
   , Windows
   ;

{ TWindows32sVersion }

constructor TWindows32sVersion.Create();
begin
   inherited;
   FPlatformId := VER_PLATFORM_WIN32s;
end;

function TWindows32sVersion.GetFullName(): string;
begin
   AppendToName( FLang );
   Result := inherited + Format(' %d.%d (Win32s)',[ FMajorVersion, FMinorVersion ] );
end;

{ TWindows9xVersion }

constructor TWindows9xVersion.Create();
begin
   inherited;
   FPlatformId := VER_PLATFORM_WIN32_WINDOWS;
end ;

function TWindows9xVersion.GetFullName(): string;
begin
   FTempName := '';
   case FMinorVersion of

        // Windows95
        00: begin
               AppendToName( '95' );
               case LOWORD( FBuildNumber ) of
                   1111..1111: AppendToName( 'OSR2'   );
                   1212..1213: AppendToName( 'OSR2.1' );
                   1214..9999: AppendToName( 'OSR2.5' );
                   else        AppendToName( IntToStr( LOWORD( FBuildNumber ) ) );
               end;
            end;

        // Windows98
        10: begin
               AppendToName( '98' );
               case LOWORD( FBuildNumber ) of
                   2222..9999: AppendToName( 'Second Edition' );
                   else        AppendToName( IntToStr( LOWORD( FBuildNumber ) ) );
               end;
            end;

        // WindowsMe
        90: begin
               AppendToName( 'Me' );
            end

       else begin
               AppendToName( 'Unknown version' );
            end;
   end;

   AppendToName( FLang );
   Result := inherited + ' ' + FTempName;
end;


end.

