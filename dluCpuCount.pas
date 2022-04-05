unit dluCpuCount;
(******************************************************************************
2021.05.02
From web page: "Example of multi-threaded application: array of threads"
https://wiki.lazarus.freepascal.org/Example_of_multi-threaded_application:_array_of_threads

Note: The function definition used below for FPsysctl() is for FPC v3.0.4
      For FPC v3.2.0 and v3.3.1 (trunk) the first argument is no longer pchar but pcint.
      Adjust the code example accordingly.

Version 0.1   2021.05.02
+ initial, direct copy from webpage
*******************************************************************************)
{$mode objfpc}{$H+}

interface

//returns number of cores: a computer with two hyperthreaded cores will report 4
function GetLogicalCpuCount: Integer;

implementation

{$IF defined(windows)}
uses windows;
{$endif}

{$IF defined(darwin)}
uses ctypes, sysctl;
{$endif}

{$IFDEF Linux}
uses ctypes;

const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}


function GetLogicalCpuCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
  //returns total number of processors available to system including logical hyperthreaded processors
  var i                    : Integer;
      ProcessAffinityMask  : DWORD_PTR;
      SystemAffinityMask   : DWORD_PTR;
      Mask                 : DWORD;
      SystemInfo           : SYSTEM_INFO;
begin
   ProcessAffinityMask := Default( DWORD_PTR );
   SystemAffinityMask  := Default( DWORD_PTR );
   if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask) then begin
      Result := 0;
      for i := 0 to 31 do begin
         Mask := DWord(1) shl i;
         if (ProcessAffinityMask and Mask)<>0 then
            inc(Result);
      end;
   end else begin
      //can't get the affinity mask so we just report the total number of processors
      SystemInfo := Default( SYSTEM_INFO );
      GetSystemInfo( SystemInfo );
      Result := SystemInfo.dwNumberOfProcessors;
   end;
end;
{$ELSEIF defined(UNTESTEDsolaris)}
begin
   t = sysconf(_SC_NPROC_ONLN);
end;
{$ELSEIF defined(freebsd) or defined(darwin)}
  var mib: array[0..1] of cint;
      len: cint;
      status: integer;
begin
   mib[0] := CTL_HW;
   mib[1] := HW_NCPU;
   len    := sizeof(Result);
   status := fpsysctl( pchar(@mib), Length(mib), @Result, @len, Nil, 0);
   if status <> 0 then WriteLn('Error in fpsysctl()');
end;
{$ELSEIF defined(linux)}
begin
   Result := sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ELSE}
begin
   Result := 1;
end;
{$ENDIF}
end.
