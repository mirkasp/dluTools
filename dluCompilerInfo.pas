unit dluCompilerInfo;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   {$modeswitch UNICODESTRINGS+}

   {$DEFINE dlu_Generics}
   {$DEFINE dlu_Unicode}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

function GetFPUinfo(): string;
function GetTargetCPU(): string;
function GetTargetPlatform(): string;
function InsideIDE(): boolean;

function CompilerVersionAsString: string;

implementation

uses Windows;

function GetFPUinfo(): string;
//
// https://wiki.freepascal.org/Platform_defines
//
begin
   Result :=
   {$IFDEF FPC}
             'Compiler FPU info:'
       {$IFDEF FPUSOFT}     + sLineBreak + '[FPUSOFT] Software emulation of FPU (all types)' {$ENDIF}
       {$IFDEF FPUSSE64}    + sLineBreak + '[FPUSSE64] SSE64 FPU on Intel I386 and higher, AMD64.' {$ENDIF}
       {$IFDEF FPUSSE}      + sLineBreak + '[FPUSSE] SSE instructions on Intel I386 and higher.' {$ENDIF}
       {$IFDEF FPUSSE2}     + sLineBreak + '[FPUSSE2] SSE 2 instructions on Intel I386 and higher.' {$ENDIF}
       {$IFDEF FPUSSE3}     + sLineBreak + '[FPUSSE3] SSE 3 instructions on Intel I386 and higher, AMD64.' {$ENDIF}
       {$IFDEF FPULIBGCC}   + sLineBreak + '[FPULIBGCC] GCC library FPU emulation on ARM and M68K.' {$ENDIF}
       {$IFDEF FPU68881}    + sLineBreak + '[FPU68881] 68881 on M68K.' {$ENDIF}
       {$IFDEF FPUFPA}      + sLineBreak + '[FPUFPA] FPA on ARM.' {$ENDIF}
       {$IFDEF FPUFPA10}    + sLineBreak + '[FPUFPA10] FPA 10 on ARM.' {$ENDIF}
       {$IFDEF FPUFPA11}    + sLineBreak + '[FPUFPA11] FPA 11 on ARM.' {$ENDIF}
       {$IFDEF FPUVFP}      + sLineBreak + '[FPUVFP] VFP on ARM.' {$ENDIF}
       {$IFDEF FPUX87}      + sLineBreak + '[FPUX87] X87 FPU on Intel I386 and higher.' {$ENDIF}
       {$IFDEF FPUITANIUM}  + sLineBreak + '[FPUITANIUM] On Intel Itanium.' {$ENDIF}
       {$IFDEF FPUSTANDARD} + sLineBreak + '[FPUSTANDARD] On PowerPC (32/64 bit).' {$ENDIF}
       {$IFDEF FPUHARD}     + sLineBreak + '[FPUHARD] On Sparc.' {$ENDIF}
   {$ELSE}
             'no info...'
   {$ENDIF}
   ;
end;

function GetTargetPlatform(): string;
begin
  Result := 'Target Platform info:'
{$IFDEF FPC}
  {$IFDEF CONSOLE }     + sLineBreak + '[CONSOLE] Defined if an application is being compiled as a console application.' {$ENDIF}
  {$IFDEF IOS }         + sLineBreak + '[IOS] Defined if the target platform is iOS.' {$ENDIF}
  {$IFDEF IOS32 }       + sLineBreak + '[IOS32] Defined if the target platform is iOS32.' {$ENDIF}
  {$IFDEF IOS64 }       + sLineBreak + '[IOS64] Defined if the target platform is iOS64.' {$ENDIF}
  {$IFDEF NATIVECODE }  + sLineBreak + '[NATIVECODE] Since Delphi.Net' {$ENDIF}
  {$IFDEF MSWINDOWS }   + sLineBreak + '[MSWINDOWS] Indicates that the operating environment is Windows. Use MSWINDOWS to test for any flavor of the Windows platform instead of WIN32.' {$ENDIF}
  {$IFDEF WIN32 }       + sLineBreak + '[WIN32] Target platform is the native 32-bit Windows platform.' {$ENDIF}
  {$IFDEF WIN64 }       + sLineBreak + '[WIN64] Target platform is 64-bit Windows.' {$ENDIF}
  {$IFDEF MACOS }       + sLineBreak + '[MACOS] Target platform is Mac OS X.' {$ENDIF}
  {$IFDEF MACOS32 }     + sLineBreak + '[MACOS32] Target platform is 32-bit Mac OS X.' {$ENDIF}
  {$IFDEF MACOS64 }     + sLineBreak + '[MACOS64] Target platform is 64-bit Mac OS X.' {$ENDIF}
  {$IFDEF LINUX }       + sLineBreak + '[LINUX] Since Kylix' {$ENDIF}
  {$IFDEF LINUX32 }     + sLineBreak + '[LINUX32] Since Kylix' {$ENDIF}
  {$IFDEF POSIX }       + sLineBreak + '[POSIX] Since Kylix' {$ENDIF}
  {$IFDEF POSIX32 }     + sLineBreak + '[POSIX32] Since Kylix' {$ENDIF}
  {$IFDEF POSIX64 }     + sLineBreak + '[POSIX64] Since Kylix' {$ENDIF}
  {$IFDEF ANDROID }     + sLineBreak + '[ANDROID]Defined if the target platform is Android.' {$ENDIF}
  {$IFDEF ANDROID32 }   + sLineBreak + '[ANDROID32] Since XE8/iOSarm64' {$ENDIF}
{$ELSE}

{$ENDIF}
end;


//
// http://docwiki.embarcadero.com/RADStudio/Seattle/en/Conditional_compilation_(Delphi)
//

function GetTargetCPU(): string;
  const FPT = 'Free Pascal target is';
begin
  Result := 'Target CPU info:'
{$IFDEF FPC}
  {$IFDEF CPU86       } + sLineBreak + '[CPU86] ' + FPT + ' an Intel 80x86 or compatible (16 and 32 bit).' {$ENDIF}
  {$IFDEF CPU87       } + sLineBreak + '[CPU87] ' + FPT + ' an Intel 80x86 or compatible (16 and 32 bit).' {$ENDIF}
  {$IFDEF CPU386      } + sLineBreak + '[CPU386] ' + FPT + ' an Intel 80386 or later.' {$ENDIF}
  {$IFDEF CPUI386     } + sLineBreak + '[CPUI386] ' + FPT + ' an Intel 80386 or later.' {$ENDIF}
  {$IFDEF CPU68K      } + sLineBreak + '[CPU68K] ' + FPT + ' a Motorola 680x0 or compatible.' {$ENDIF}
  {$IFDEF CPUM68K     } + sLineBreak + '[CPUM68K] ' + FPT + ' a Motorola 680x0 or compatible.' {$ENDIF}
  {$IFDEF CPUM68020   } + sLineBreak + '[CPUM68020] ' + FPT + ' a Motorola 68020 or later.' {$ENDIF}
  {$IFDEF CPU68       } + sLineBreak + '[CPU68] ' + FPT + ' a Motorola 680x0 or compatible.' {$ENDIF}
  {$IFDEF CPUSPARC32  } + sLineBreak + '[CPUSPARC32] ' + FPT + ' a SPARC v7 or compatible.' {$ENDIF}
  {$IFDEF CPUSPARC    } + sLineBreak + '[CPUSPARC] ' + FPT + ' a SPARC v7 or compatible.' {$ENDIF}
  {$IFDEF CPUALPHA    } + sLineBreak + '[CPUALPHA] ' + FPT + ' an Alpha AXP or compatible.' {$ENDIF}
  {$IFDEF CPUPOWERPC  } + sLineBreak + '[CPUPOWERPC] ' + FPT + ' a 32-bit or 64-bit PowerPC or compatible.' {$ENDIF}
  {$IFDEF CPUPOWERPC32} + sLineBreak + '[CPUPOWERPC32] ' + FPT + ' a 32-bit PowerPC or compatible.' {$ENDIF}
  {$IFDEF CPUPOWERPC64}	+ sLineBreak + '[CPUPOWERPC64] ' + FPT + ' a 64-bit PowerPC or compatible.' {$ENDIF}
  {$IFDEF CPUX86_64   }	+ sLineBreak + '[CPUX86_64] ' + FPT + ' a AMD64 or Intel 64-bit processor.' {$ENDIF}
  {$IFDEF CPUAMD64    }	+ sLineBreak + '[CPUAMD64] ' + FPT + ' a AMD64 or Intel 64-bit processor.' {$ENDIF}
  {$IFDEF CPUX64      }	+ sLineBreak + '[CPUX64] ' + FPT + ' a AMD64 or Intel 64-bit processor' {$ENDIF}
  {$IFDEF CPUIA64     }	+ sLineBreak + '[CPUIA64] ' + FPT + ' a Intel itanium 64-bit processor.' {$ENDIF}
  {$IFDEF CPUARM      }	+ sLineBreak + '[CPUARM] ' + FPT + ' an ARM 32-bit processor.' {$ENDIF}
  {$IFDEF CPUAVR      }	+ sLineBreak + '[CPUAVR] ' + FPT + ' an AVR 16-bit processor.' {$ENDIF}
  {$IFDEF CPU16       }	+ sLineBreak + '[CPU16] ' + FPT + ' a 16-bit CPU.' {$ENDIF}
  {$IFDEF CPU32       }	+ sLineBreak + '[CPU32] ' + FPT + ' a 32-bit CPU.' {$ENDIF}
  {$IFDEF CPU64       }	+ sLineBreak + '[CPU64] ' + FPT + ' a 64-bit CPU.' {$ENDIF}
  {$IFDEF CPUI8086    } + sLineBreak + '[CPUI8086] indicates a 16-bit x86 target (i8086)' {$ENDIF}
{$ELSE}
  {$IFDEF CPU386      } + sLineBreak + '[CPU386] Intel 386 or later.' {$ENDIF}
  {$IFDEF CPUX86      } + sLineBreak + '[CPUX86] Intel 386 or later on any platform.' {$ENDIF}
  {$IFDEF CPUX64      } + sLineBreak + '[CPUX64] The CPU supports the x86-64 instruction set, and is in a 64-bit environment.' {$ENDIF}
  {$IFDEF CPU32BITS   } + sLineBreak + '[CPU32BITS] The CPU is in a 32-bit environment.' {$ENDIF}
  {$IFDEF CPU64BITS   } + sLineBreak + '[CPU64BITS] The CPU is in a 64-bit environment.' {$ENDIF}
  {$IFDEF CPUARM      } + sLineBreak + '[CPUARM] The CPU is based on the ARM architecture.' {$ENDIF}
  {$IFDEF CPUARM32    } + sLineBreak + '[CPUARM32] The CPU is in a 32-bit ARM environment.' {$ENDIF}
  {$IFDEF CPUARM64    } + sLineBreak + '[CPUARM64] The CPU is in a 64-bit ARM environment.' {$ENDIF}
{$ENDIF}
end;

function InsideIDE(): boolean;
begin
{$IFDEF FPC}
   Result := IsDebuggerPresent();
{$ELSE}
   {$WARN SYMBOL_PLATFORM OFF}
   Result := (System.DebugHook <> 0);
   {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
end;

function CompilerVersionAsString: string;
begin
{$IFDEF FPC}
{$PUSH}
{$MACRO ON}
    Result := 'Free Pascal ' + {$INCLUDE %FPCVERSION%}
    {$ifdef FPC_HAS_MANAGEMENT_OPERATORS}+' MOP'{$endif}
{$POP}
{$ELSE}
    Result := 'unknown';
    {$ifdef VER130 } Result := 'Delphi 5'             {$endif}
    {$ifdef KYLIX3 } Result := 'Kylix 3'              {$endif}
    {$ifdef VER80 }  Result := 'Delphi 1'              {$endif}
    {$ifdef VER90 }  Result := 'Delphi 2'              {$endif}
    {$ifdef VER100 } Result := 'Delphi 3'             {$endif}
    {$ifdef VER120 } Result := 'Delphi 4'             {$endif}
    {$ifdef VER130 } Result := 'Delphi 5'             {$endif}
    {$ifdef VER140 } Result := 'Delphi 6'             {$endif}
    {$ifdef VER150 } Result := 'Delphi 7'             {$endif}
    {$ifdef VER160 } Result := 'Delphi 8'             {$endif}
    {$ifdef VER170 } Result := 'Delphi 2005'          {$endif}
    {$ifdef VER180 } Result := 'Delphi 2006'          {$endif}
    {$ifdef VER185 } Result := 'Delphi 2007'          {$endif}
    {$ifdef VER190 } Result := 'Delphi 2007 for .Net' {$endif}
    {$ifdef VER200 } Result := 'Delphi 2009'       {$endif}
    {$ifdef VER210 } Result := 'Delphi 2010'       {$endif}
    {$ifdef VER220 } Result := 'Delphi XE'         {$endif}
    {$ifdef VER230 } Result := 'Delphi XE2'        {$endif}
    {$ifdef VER240 } Result := 'Delphi XE3'        {$endif}
    {$ifdef VER250 } Result := 'Delphi XE4'        {$endif}
    {$ifdef VER260 } Result := 'Delphi XE5'        {$endif}
    {$ifdef VER265 } Result := 'AppMethod 1'       {$endif}
    {$ifdef VER270 } Result := 'Delphi XE6'        {$endif}
    {$ifdef VER280 } Result := 'Delphi XE7'        {$endif}
    {$ifdef VER290 } Result := 'Delphi XE8'        {$endif}
    {$ifdef VER300 } Result := 'Delphi 10 Seattle' {$endif}
    {$ifdef VER310 } Result := 'Delphi 10.1 Berlin'{$endif}
    {$ifdef VER320 } Result := 'Delphi 10.2 Tokyo' {$endif}
    {$ifdef VER330 } Result := 'Delphi 10.3 Rio'   {$endif}
    {$ifdef VER340 } Result := 'Delphi 10.4 Sidney'{$endif}
    {$ifdef VER350 } Result := 'Delphi 11 Alexandria'{$endif}
    {$ifdef VER360 } Result := 'Delphi 12 Athens'{$endif}
{$ENDIF FPC}
{$IFDEF CPU64} +', 64 bit' {$ELSE} +', 32 bit' {$ENDIF}

end;



end.

