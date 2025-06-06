unit dluFileOwner;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch UNICODESTRINGS+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Classes;

function FileOwner( const FileName: UnicodeString; out Domain, Owner: UnicodeString; out RetCode: Cardinal ): boolean;
function FileOwner( const FileName: UnicodeString; out Domain, Owner: UnicodeString): boolean;
function FileOwner( const FileName: UnicodeString ): UnicodeString; overload;
function FileOwner( const FileName: AnsiString ): AnsiString; overload;

function FileOwnerEx( const AFileName: UnicodeString) : UnicodeString;

implementation

uses
{$IFDEF MSWINDOWS}
   Windows,
{$ENDIF}
   SysUtils
   ;

{$IFDEF MSWINDOWS}

function ConvertSidToStringSid( Sid: PSID; out StringSid: PChar): BOOL; stdcall;  external 'ADVAPI32.DLL' name {$IFDEF UNICODE} 'ConvertSidToStringSidW'{$ELSE} 'ConvertSidToStringSidA'{$ENDIF};

function SIDToString(ASID: PSID): string;
  var StringSid : PChar;
begin
//   if not ConvertSidToStringSid( ASID, StringSid ) then RaiseLastWin32Error;
   if not ConvertSidToStringSid( ASID, StringSid ) then RaiseLastOSError;
   Result := string(StringSid);
end;

// https://forum.lazarus.freepascal.org/index.php?topic=20235.0

function GetFileOwner( const FileName: string; out Domain, Owner: string; out RetCode: Cardinal ): boolean;
  var SecDescr     : PSecurityDescriptor;
      OwnerSID     : PSID;
      OwnerDefault : BOOL;
      SizeNeeded   : DWORD;

      peUse        : SID_NAME_USE;
      cchDomain    : DWORD;
      cchName      : DWORD;
      wName        : String = '';
      wDomain      : String = '';

begin
    Result := false;
    Owner  := '';
    Domain := '';

    // local variables
    SizeNeeded := 0;
    OwnerSID   := nil;
    peUse      := SidTypeUser;

    // Get Length
    GetFileSecurityW( PChar(FileName),
                      OWNER_SECURITY_INFORMATION,
                      nil, SizeNeeded,
                      {$IFDEF FPC}@{$ENDIF}SizeNeeded );
    RetCode := GetLastError();
    if RetCode = ERROR_INSUFFICIENT_BUFFER
       then GetMem( SecDescr, SizeNeeded )
       else begin
          Owner  := UnicodeString( SysErrorMessage( RetCode ) );
          Domain := '';
          Result := true;
          exit;
       end;

    if not GetFileSecurityW( PChar(FileName),
                             OWNER_SECURITY_INFORMATION,
                             SecDescr, SizeNeeded,
                             {$IFDEF FPC}@{$ENDIF}SizeNeeded )
    then begin
       RetCode := GetLastError();
       FreeMem( SecDescr );
       exit;
    end;

    if not GetSecurityDescriptorOwner( SecDescr,
                                       OwnerSID,
                                       {$IFDEF FPC}@{$ENDIF}OwnerDefault )
    then begin
       RetCode := GetLastError();
       FreeMem( SecDescr );
       exit;
    end;

    cchName   := 0;
    cchDomain := 0;

    // Get Length
    LookupAccountSidW( nil, OwnerSID, nil, cchName, nil, cchDomain, peUse );
    RetCode := GetLastError();
    if RetCode = ERROR_INSUFFICIENT_BUFFER then begin
       SetLength( wName,   cchName - 1  );
       SetLength( wDomain, cchDomain - 1 );
       if LookupAccountSidW( nil, OwnerSID, PChar(wName), cchName, PChar(wDomain), cchDomain, peUse) then begin
          Owner  := wName;
          Domain := wDomain;
          Result := true;
       end else
          RetCode := GetLastError();
    end else begin
      Owner  := SIDToString( OwnerSID );
      Domain := '';
      Result := true;
    end;
    FreeMem( SecDescr );
end;
{$ELSE}
function GetFileOwner(const FileName: string; out Domain, Owner: string; out RetCode: Cardinal ): boolean;
begin
   Domain  := '';
   Owner   := '';
   RetCode := 0;
   Result  := true;
end;
{$ENDIF}


{----------------------------}
function FileOwner(const FileName: string; out Domain, Owner: string; out RetCode: Cardinal ): boolean;
begin
  Result := GetFileOwner( FileName, Domain, Owner, RetCode );
  if Result then begin
     Domain := UnicodeString( Domain );  // UTF8Encode( Domain );
     Owner  := UnicodeString( Owner );   // UTF8Encode( Owner );
  end;
end;

function FileOwner(const FileName: string; out Domain, Owner: string): boolean;
  var RetCode: Cardinal;
begin
  Result := FileOwner( FileName, Domain, Owner, RetCode );
end;

function FileOwner(const FileName: string ): string; overload;
  var sDomain: string;
begin
   if FileOwner( FileName, sDomain, Result ) then begin
      if Length( sDomain ) > 0 then Result := Result + '@' + sDomain;
   end else
      Result := '';
end;


function FileOwner( const FileName: AnsiString ): AnsiString; overload;
begin
   Result := AnsiString( FileOwner( UnicodeString( FileName ) ) );
end;

function FileOwnerEx(const AFileName: UnicodeString): UnicodeString;
  var sDomain : UnicodeString;
      RetCode : Cardinal;
begin
   Result  := '';
   if dluFileOwner.FileOwner( AFileName, sDomain, Result, RetCode )
      then Result := Result + '@' + sDomain
      else Result := 'Error - 0x' + UnicodeString( IntToHex( RetCode, 8 ) )
end;


end.
