unit dluFileOwner;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch UNICODESTRINGS+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Classes;

function FileOwner( const FileName: String; out Domain, Owner: String; out RetCode: Cardinal ): boolean; overload;
function FileOwner( const FileName: String; out Domain, Owner: String ): boolean; overload;

function FileOwnerMini( const AFileName: UnicodeString ): UnicodeString; overload;
function FileOwnerMini( const AFileName: AnsiString ): AnsiString; overload;

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
   StringSid := nil;
   if not ConvertSidToStringSid( ASID, StringSid )
      then RaiseLastOSError;
   try
       Result := StringSid;
   finally
       if StringSid <> nil then
          LocalFree( {%H-}HLOCAL( StringSid ) );
   end;


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

    if RetCode <> ERROR_INSUFFICIENT_BUFFER then begin
       Owner  := '';
       Domain := '';
       Result := false;
       exit;
    end;

    GetMem( SecDescr, SizeNeeded );
    try
        if not GetFileSecurityW( PChar(FileName),
                                 OWNER_SECURITY_INFORMATION,
                                 SecDescr, SizeNeeded,
                                 {$IFDEF FPC}@{$ENDIF}SizeNeeded )
        then begin
           RetCode := GetLastError();
           exit;
        end;

        if not GetSecurityDescriptorOwner( SecDescr,
                                           OwnerSID,
                                           {$IFDEF FPC}@{$ENDIF}OwnerDefault )
        then begin
           RetCode := GetLastError();
           exit;
        end;

        cchName   := 0;
        cchDomain := 0;

        // Get Length
        LookupAccountSidW( nil, OwnerSID, nil, cchName, nil, cchDomain, peUse );
        RetCode := GetLastError();
        if RetCode = ERROR_INSUFFICIENT_BUFFER then begin

           SetLength( wName,   cchName   ); // Alokuj bufor
           SetLength( wDomain, cchDomain ); // Alokuj bufor

           if LookupAccountSidW( nil, OwnerSID, PChar(wName), cchName, PChar(wDomain), cchDomain, peUse ) then begin
              Owner  := PChar( wName   );   // <--- POPRAWKA (odczytuje do znaku null)
              Domain := PChar( wDomain );   // <--- POPRAWKA (odczytuje do znaku null)
              Result := true;
           end else
              RetCode := GetLastError();
        end else begin
           Owner  := SIDToString( OwnerSID );
           Domain := '';
           Result := true;
        end;

    finally
        FreeMem( SecDescr );
    end;
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
function FileOwner(const FileName: String; out Domain, Owner: String; out RetCode: Cardinal ): boolean;
begin
  Result := GetFileOwner( FileName, Domain, Owner, RetCode );
end;

function FileOwner(const FileName: String; out Domain, Owner: String): boolean;
  var RetCode: Cardinal;
begin
  Result := FileOwner( FileName, Domain, Owner, RetCode );
end;

function FileOwnerMini( const AFileName: UnicodeString ): UnicodeString;
  var sDomain: UnicodeString = '';
      sOwner : UnicodeString = '';
      RetCode: Cardinal;
begin
   if GetFileOwner( AFileName, sDomain, sOwner, RetCode )  then begin
      Result := sOwner;
      if Length(sDomain) > 0 then Result := Result + '@' + sDomain;
   end else begin
      Result := UnicodeFormat( '%s [0x%s]', [ SysErrorMessage( RetCode ), IntToHex(RetCode, 8) ] );
   end;
end;

function FileOwnerMini( const AFileName: AnsiString) : AnsiString;
begin
   Result := AnsiString( FileOwnerMini( UnicodeString( AFileName ) ) );
end;



end.
