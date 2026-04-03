unit dluFileOwner;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$MODESWITCH UNICODESTRINGS+}
{$ENDIF}

interface

/// <summary>
/// Cache management for file owner lookups.
/// </summary>
procedure EnableOwnerCache;
procedure DisableOwnerCache;
procedure ClearOwnerCache;

/// <summary>
/// Retrieves the domain and owner name of a specified file.
/// Returns True if successful. Uses cache if previously enabled.
/// </summary>
function GetFileOwner(const AFileName: String; out ADomain, AOwner: String): Boolean;

/// <summary>
/// Returns a formatted string "DOMAIN\User" or just "User".
/// Overloaded for UnicodeString and AnsiString.
/// </summary>
function GetFileOwnerStr(const AFileName: UnicodeString): UnicodeString; overload;
function GetFileOwnerStr(const AFileName: AnsiString): AnsiString; overload;

implementation

uses
  SysUtils,
  Windows,
  Generics.Collections;

type
  // Dictionary specialization for FPC (OBJFPC mode)
  TCacheDictionary = specialize TDictionary<string, string>;

var
  GOwnerCache: TCacheDictionary = nil;

// Required import to convert SID to string for cache keys
function ConvertSidToStringSidW(Sid: PSID; out StringSid: LPWSTR): BOOL; stdcall; external 'advapi32.dll';

procedure EnableOwnerCache;
begin
  if not Assigned(GOwnerCache) then
    GOwnerCache := TCacheDictionary.Create;
end;

procedure DisableOwnerCache;
begin
  if Assigned(GOwnerCache) then
    FreeAndNil(GOwnerCache);
end;

procedure ClearOwnerCache;
begin
  if Assigned(GOwnerCache) then
    GOwnerCache.Clear;
end;

function SafeSIDToString(ASID: PSID): String;
var
  StringSid: LPWSTR;
begin
  Result := '';
  if Assigned(ASID) then begin
    if ConvertSidToStringSidW(ASID, StringSid) then begin
      try
        Result := StringSid;
      finally
        LocalFree(HLOCAL(StringSid));
      end;
    end;
  end;
end;

function GetFileOwner(const AFileName: String; out ADomain, AOwner: String): Boolean;
var
  SecDesc      : TBytes = nil;
  SizeNeeded   : DWORD  = 0;
  OwnerSID     : PSID   = nil;
  OwnerDefault : BOOL;
  PeUse        : SID_NAME_USE;
  CchName,
  CchDomain    : DWORD;
  BufName      : array of WideChar = nil;
  BufDomain    : array of WideChar = nil;
  SIDKey       : string;
  CachedVal    : string;
  SepPos       : Integer;
begin
  Result  := False;
  ADomain := '';
  AOwner  := '';

  // 1. Retrieve Security Descriptor size and data
  GetFileSecurityW(PWideChar(UnicodeString(AFileName)), OWNER_SECURITY_INFORMATION, nil, 0, @SizeNeeded);
  if (GetLastError() <> ERROR_INSUFFICIENT_BUFFER) and (SizeNeeded = 0) then Exit;

  SetLength(SecDesc, SizeNeeded);
  if not GetFileSecurityW(PWideChar(UnicodeString(AFileName)), OWNER_SECURITY_INFORMATION, @SecDesc[0], SizeNeeded, @SizeNeeded) then Exit;

  OwnerDefault := False;
  if not GetSecurityDescriptorOwner(@SecDesc[0], OwnerSID, @OwnerDefault) or (OwnerSID = nil) then Exit;

  // 2. Handle caching based on SID
  SIDKey := SafeSIDToString(OwnerSID);
  if Assigned(GOwnerCache) and GOwnerCache.TryGetValue(SIDKey, CachedVal) then begin
    SepPos := Pos('\', CachedVal);
    if SepPos > 0 then begin
      ADomain := Copy(CachedVal, 1, SepPos - 1);
      AOwner  := Copy(CachedVal, SepPos + 1, MaxInt);
    end else
      AOwner := CachedVal;
    Exit(True);
  end;

  // 3. Resolve SID to account name (LookupAccountSidW)
  CchName   := 0;
  CchDomain := 0;
  PeUse     := SidTypeUser;

  LookupAccountSidW(nil, OwnerSID, nil, CchName, nil, CchDomain, PeUse);
  if (CchName > 0) then begin
    SetLength(BufName, CchName);
    SetLength(BufDomain, CchDomain);

    if LookupAccountSidW(nil, OwnerSID, PWideChar(BufName), CchName, PWideChar(BufDomain), CchDomain, PeUse) then begin
      AOwner  := PWideChar(BufName);
      ADomain := PWideChar(BufDomain);
      Result  := True;

      // Add result to cache if enabled
      if Assigned(GOwnerCache) then begin
        if ADomain <> '' then
          GOwnerCache.AddOrSetValue(SIDKey, ADomain + '\' + AOwner)
        else
          GOwnerCache.AddOrSetValue(SIDKey, AOwner);
      end;
    end;
  end;

  // Fallback: If name resolution fails (e.g. orphan SID), return SID as string
  if not Result then begin
    AOwner := SIDKey;
    Result := (AOwner <> '');
  end;
end;

function GetFileOwnerStr(const AFileName: UnicodeString): UnicodeString;
var
  Domain, Owner: String;
begin
  if GetFileOwner(AFileName, Domain, Owner) then begin
    if Domain <> '' then
      Result := Domain + '\' + Owner
    else
      Result := Owner;
  end else begin
    Result := 'Error: ' + UTF8Decode(SysErrorMessage(GetLastError));
  end;
end;

function GetFileOwnerStr(const AFileName: AnsiString): AnsiString;
begin
  // Safe casting for FPC environment
  Result := AnsiString(GetFileOwnerStr(UnicodeString(AFileName)));
end;

finalization
  // Ensure cache cleanup on unit destruction
  DisableOwnerCache;

end.
