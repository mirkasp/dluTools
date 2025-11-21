unit dluFileOwner;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$MODESWITCH UNICODESTRINGS+}
{$ENDIF}

interface

/// <summary>
/// Pobiera domenę i nazwę właściciela pliku.
/// Zwraca True jeśli się powiodło.
/// </summary>
function GetFileOwner(const AFileName: String; out ADomain, AOwner: String): Boolean;

/// <summary>
/// Zwraca sformatowany ciąg "DOMENA\Użytkownik" lub sam "Użytkownik".
/// W przypadku błędu zwraca sformatowany komunikat błędu.
/// </summary>
function GetFileOwnerStr(const AFileName: UnicodeString): UnicodeString; overload;
function GetFileOwnerStr(const AFileName: AnsiString): AnsiString; overload;

implementation

uses SysUtils
   , Windows
   ;


// Jawnie importujemy wersję W (Wide) funkcji WinAPI
function ConvertSidToStringSidW(Sid: PSID; out StringSid: LPWSTR): BOOL; stdcall; external 'advapi32.dll';

//<summary>
//Konwertuje SID na string
//</summary>
function SafeSIDToString(ASID: PSID): String;
  var StringSid: LPWSTR; //PChar;
begin
   Result := '';
   if Assigned( ASID ) then begin
      StringSid := nil;
      if ConvertSidToStringSidW( ASID, StringSid ) then begin
         try
            Result := StringSid;
         finally
            LocalFree( {%H-}HLOCAL(StringSid) );
         end;
      end;
   end;
end;

function GetFileOwner(const AFileName: String; out ADomain, AOwner: String): Boolean;
  var SecDesc      : TBytes; // Automatycznie zarządzany bufor
      SizeNeeded   : DWORD;
      OwnerSID     : PSID;
      OwnerDefault : BOOL;
      PeUse        : SID_NAME_USE;
      CchName,
      CchDomain    : DWORD;
      BufName,
      BufDomain    : array of WideChar; // Bufory dla API Unicode
begin
   Result     := False;
   ADomain    := '';
   AOwner     := '';
   SizeNeeded := 0;

   // 1. Pobierz rozmiar potrzebny na Security Descriptor
   GetFileSecurityW( PWideChar( AFileName ), OWNER_SECURITY_INFORMATION, nil, 0, @SizeNeeded);
  
   // Jeśli błąd jest inny niż "za mały bufor" i rozmiar to 0, to coś poszło nie tak (np. brak pliku)
   if (GetLastError() <> ERROR_INSUFFICIENT_BUFFER) and (SizeNeeded = 0) then Exit;

   // 2. Alokuj pamięć (SetLength robi to automatycznie i zwolni przy wyjściu z funkcji)
   SetLength( SecDesc, SizeNeeded );

   // 3. Pobierz faktyczne dane
   if not GetFileSecurityW( PWideChar( AFileName ), OWNER_SECURITY_INFORMATION, Windows.PSecurityDescriptor( SecDesc ), SizeNeeded, @SizeNeeded)
      then Exit;

   // 4. Wyciągnij SID właściciela z deskryptora
   OwnerSID     := nil;
   OwnerDefault := False;
   if not GetSecurityDescriptorOwner(PSecurityDescriptor(SecDesc), OwnerSID, @OwnerDefault)
      then Exit;

   if OwnerSID = nil
      then Exit;

   // 5. Pobierz nazwę użytkownika i domenę (LookupAccountSid)
   CchName   := 0;
   CchDomain := 0;
   PeUse     := SidTypeUser;

   // Pierwsze wywołanie tylko po rozmiary - oczekujemy False i błędu INSUFFICIENT_BUFFER
   if (not LookupAccountSidW(nil, OwnerSID, nil, CchName, nil, CchDomain, PeUse)) and (GetLastError() = ERROR_INSUFFICIENT_BUFFER) then begin

      SetLength( BufName,   CchName   );
      SetLength( BufDomain, CchDomain );

      if LookupAccountSidW(nil, OwnerSID, PWideChar(BufName), CchName, PWideChar(BufDomain), CchDomain, PeUse) then begin
         AOwner  := PWideChar( BufName   );
         ADomain := PWideChar( BufDomain );
         Result  := True;
      end;
   end;

   // Fallback: Jeśli nie udało się rozwiązać nazwy (np. brak dostępu do DC), zwróć SID
   if not Result then begin
      AOwner := SafeSidToString(OwnerSID);
      Result := (AOwner <> '');
   end;

end;

function GetFileOwnerStr(const AFileName: UnicodeString): UnicodeString;
  var Domain, Owner: String;
begin
   if GetFileOwner(AFileName, Domain, Owner) then begin
      if Domain <> ''
         then Result := Domain + '\' + Owner // Standardowy format Windows
         else Result := Owner;
   end else begin
      Result := UnicodeFormat('Error: %s', [SysErrorMessage(GetLastError)]);
  end;
end;

function GetFileOwnerStr(const AFileName: AnsiString): AnsiString;
begin
   Result := UTF8Encode( GetFileOwnerStr( UTF8Decode( AFileName ) ) );
end;

end.
