unit dluAppInfo;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch UNICODESTRINGS+}
  //{$modeswitch ADVANCEDRECORDS+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

type TAppCaptionOption = ( apcHideVersion,
                           apcEnglish          //
                         );

type TAppCaptionOptions = set of TAppCaptionOption;

function GetAppCaption( const AppShortName: String; const AOptions: TAppCaptionOptions = [] ): String; overload;
function GetAppCaption( const AppShortName: String; const AHideVersion: boolean ): String; overload;

implementation

uses dluSysInfo
   , dluFileInfo
{$IFDEF WINDOWS}
   , Windows
{$ENDIF}
{$IFNDEF FPC}
   , SysUtils
   , mkSysInfo
{$ENDIF}
   ;

//function BoolAsStr( const ACondition: boolean; const ATrueStr, AFalseStr: string ): string; inline;
//begin
//   if ACondition then Result := ATrueStr else Result := AFalseStr;
//end;

function GetAppCaption( const AppShortName: String; const AOptions: TAppCaptionOptions ): String;
  const DEBUG_MODE = {$IFOPT D+} ', DEBUG mode' {$ELSE} '' {$endif};
        acVersionStr   : array[ boolean ] of Char = ( 'w', 'v' );
        acInsideIdeStr : array[ boolean ] of String = ( '', ', IDE' );
begin
   Result := AppShortName;
   if not (apcHideVersion in AOptions) then begin
      Result := Result + ' '
               + acVersionStr[ apcEnglish in AOptions ] + '.' + dluFileInfo.GetFileVersion()
               + ' [' + dluSysInfo.CompilerVersionAsString()
      {$IFDEF FPC}
               {$IFDEF WINDOWS}
               + acInsideIdeStr[ IsDebuggerPresent() ]
               {$ENDIF}
      {$ELSE}
               + acInsideIdeStr[ mkSysInfo.IsInsideDelphiIDE() ]
      {$ENDIF}
               + DEBUG_MODE
               + ']';
   end;
end;

function GetAppCaption( const AppShortName: String; const AHideVersion: boolean ): String;
  var apc : TAppCaptionOptions;
begin
   apc := [];
   if AHideVersion then apc := apc + [apcHideVersion];
   Result := GetAppCaption( AppShortName, apc );
end;

end.
