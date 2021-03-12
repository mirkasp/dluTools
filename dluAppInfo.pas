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
{$IFNDEF FPC}
   , SysUtils
   , mkSysInfo
{$ENDIF}
   ;

const acVersionStr : array[ boolean ] of Char = ( 'w', 'v' );

function GetAppCaption( const AppShortName: String; const AOptions: TAppCaptionOptions ): String;
  var s : string;
begin
   s := ' ' + acVersionStr[ apcEnglish in AOptions ] + '.';
{$IFDEF FPC}
   Result := AppShortName;
   if not (apcHideVersion in AOptions) then begin
      Result := Result + s
               + dluFileInfo.GetFileInfo().FileVersion {$IFDEF DEBUG} + ' [DEBUG mode]' {$ENDIF}
               + ' [' + dluSysInfo.CompilerVersionAsString();
      //if IsInsideDelphiIDE then Append( ' IDE' );
      Result := Result + ']';
   end;
{$ELSE}
   with TStringBuilder.Create do begin
      Append( AppShortName );
      if not (apcHideVersion in AOptions) then begin
         Append( s );
         Append( dluFileInfo.GetFileVersion() {$IFDEF DEBUG} + ' [DEBUG mode]' {$ENDIF} );
         Append( ' [' + dluSysInfo.CompilerVersionAsString() );
         if mkSysInfo.IsInsideDelphiIDE then Append( ' IDE' );
         Append( ']' );
      end;
      Result := ToString;
      Free;
   end;
{$ENDIF}
end;

function GetAppCaption( const AppShortName: String; const AHideVersion: boolean ): String;
  var apc : TAppCaptionOptions;
begin
   apc := [];
   if AHideVersion then apc := apc + [apcHideVersion];
   Result := GetAppCaption( AppShortName, apc );
end;

end.
