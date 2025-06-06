unit dluOtherInstance;

{$mode ObjFPC}{$H+}

interface

function IsOtherInstance(const AppId: string; out RetCode: DWORD): boolean; overload;
function IsOtherInstance(const AppId: string; out RetStr: string): boolean; overload;

implementation

uses SysUtils, Windows;

var MutexHandle: THandle = 0; // Handle to the created or opened mutex

function IsOtherInstance(const AppId: string; out RetCode: DWORD): boolean;
begin
  // 1. Attempt to create a named mutex.
  //    The first parameter (nil) means no security attributes.
  //    The second parameter (False) means the calling thread does not initially own the mutex.
  //    The third parameter is the name of the mutex.
  MutexHandle := CreateMutex(nil, False, PAnsiChar(AppId) );

  RetCode := GetLastError;
  Result  := (RetCode = ERROR_ALREADY_EXISTS) or (MutexHandle = 0)
end;

function IsOtherInstance(const AppId: string; out RetStr: string): boolean;
  var rc : DWORD = 0;
begin
   RetStr := '';
   Result := IsOtherInstance( AppId, rc );
   if Result then begin
      if rc = ERROR_ALREADY_EXISTS
         then RetStr := 'Only one instance of application is allowed!'
         else RetStr := 'Mutex Creation Error = ' + IntToStr(rc) + sLineBreak + SysErrorMessage(rc);
   end;
end;

finalization
   if MutexHandle <> 0 then begin
      CloseHandle(MutexHandle);
   end;
end.

