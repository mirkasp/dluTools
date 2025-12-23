unit dluRegistry.helper;

{$mode ObjFPC}{$H+}

interface

uses Registry, Windows, SysUtils, Classes;

type
  { Definicje callbacków - wersje dla metod obiektowych }
  TKeyMethod    = procedure(ARegistry: TRegistry; const AFulPath, AKeyName: string) of object;
  TValueMethod  = procedure(ARegistry: TRegistry; const AValueName: string; ADataType: Cardinal) of object;
  TPredicateKey = function(const AKeyName: string): Boolean of object;

  { TRegistryHelper }
  TRegistryHelper = class helper for TRegistry
  public
    { Iteracja po podkluczach (folderach) }
    procedure IterateKeys(const ASubKey: string; APredicate: TPredicateKey; AKeyMtd: TKeyMethod);

    { Iteracja po wartościach (wpisach/danych) wewnątrz klucza }
    procedure IterateValues(const ASubKey: string; AValueMtd: TValueMethod);
  end;

implementation

{ Pomocnicza funkcja do pobierania uchwytu i obsługi logiki otwierania }
function GetTargetKey(AReg: TRegistry; const ASubKey: string; out ACloseNeeded: Boolean): HKEY;
begin
   ACloseNeeded := False;
   Result := Default( HKEY );
   if ASubKey <> '' then begin
      // Używamy AReg.Access zamiast KEY_READ dla spójności z obiektem
      if RegOpenKeyExW(AReg.RootKey, PWideChar(UnicodeString(ASubKey)), 0, AReg.Access, Result) = ERROR_SUCCESS then
         ACloseNeeded := True
      else
         Result := 0;
   end else begin
      Result := AReg.CurrentKey;
      if Result = 0 then
         Result := AReg.RootKey;
  end;
end;

procedure TRegistryHelper.IterateKeys(const ASubKey: string; APredicate: TPredicateKey; AKeyMtd: TKeyMethod);
  var LIndex      : DWORD;
      LRetCode    : Longint;
      LTargetHKey : HKEY;
      LCloseNeeded: Boolean;
      LBuffer     : array[0..256] of WideChar; // Unicode version
      LBufLen     : DWORD;
      LKeyName    : string;
      LCurrentPath: string;
begin
   if not Assigned(AKeyMtd) then Exit;

   LTargetHKey := GetTargetKey(Self, ASubKey, LCloseNeeded);
   if LTargetHKey = 0 then Exit;

   // Przygotuj ścieżkę do przekazania w callbacku
   if ASubKey <> '' then
      LCurrentPath := ASubKey
   else
      LCurrentPath := String(Self.CurrentPath);

   try
      LIndex := 0;
      while True do begin
         LBufLen := SizeOf(LBuffer) div SizeOf(WideChar);
         LRetCode := RegEnumKeyExW(LTargetHKey, LIndex, @LBuffer[0], LBufLen, nil, nil, nil, nil);

         if LRetCode = ERROR_NO_MORE_ITEMS then Break;

         if LRetCode = ERROR_SUCCESS then begin
            LKeyName := UTF8Encode(WideString(LBuffer));
            if (not Assigned(APredicate)) or APredicate(LKeyName) then
               AKeyMtd(Self, LCurrentPath, LKeyName);
            Inc(LIndex);
         end else if LRetCode = ERROR_MORE_DATA then begin
            // Opcjonalnie: obsługa ekstremalnie długich nazw kluczy (rzadkość)
            Inc(LIndex);
         end else
            Break;
      end;
   finally
      if LCloseNeeded then RegCloseKey(LTargetHKey);
   end;
end;

procedure TRegistryHelper.IterateValues(const ASubKey: string; AValueMtd: TValueMethod);
  var LIndex      : DWORD;
      LRetCode    : Longint;
      LTargetHKey : HKEY;
      LCloseNeeded: Boolean;
      LBuffer     : array[0..16383] of WideChar; // Max długość nazwy wartości to 16,383 znaków
      LBufLen     : DWORD;
      LType       : DWORD;
begin
   if not Assigned(AValueMtd) then Exit;

   LTargetHKey := GetTargetKey(Self, ASubKey, LCloseNeeded);
   if LTargetHKey = 0 then Exit;

   try
      LIndex := 0;
      while True do begin
         LBufLen := SizeOf(LBuffer) div SizeOf(WideChar);
         LRetCode := RegEnumValueW(LTargetHKey, LIndex, @LBuffer[0], LBufLen, nil, @LType, nil, nil);

         if LRetCode = ERROR_NO_MORE_ITEMS then Break;

         if LRetCode = ERROR_SUCCESS then begin
            AValueMtd(Self, UTF8Encode(WideString(LBuffer)), LType);
            Inc(LIndex);
         end else
            Break;
      end;
   finally
      if LCloseNeeded then RegCloseKey(LTargetHKey);
   end;
end;

end.
