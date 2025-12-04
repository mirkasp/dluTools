unit dluRegistry.helper;

{$mode ObjFPC}{$H+}

interface

uses Registry
   ;
type TPredicatePChar = function( const ABufChar: PChar ): boolean of object;
type TKeyMethod = procedure( ARegistry: TRegistry; const ASubKey, AKeyName: string ) of object;

{ TRegistryHelper }

type TRegistryHelper = class helper for TRegistry
   public
      procedure IterateKeys( const ASubKey: string; APredicate: TPredicatePChar; AKeyMtd: TKeyMethod );
end;

(*************************
przykład użycia:
Reg.RootKey := HKEY_CLASSES_ROOT;
Reg.Iterate( '\', @Predicate_mtd, 'Content Type', @consumer_mtd );

**************************)

implementation

uses Windows
   , SysUtils
   ;

{ TRegistryHelper }

procedure TRegistryHelper.IterateKeys(const ASubKey: string; APredicate: TPredicatePChar; AKeyMtd: TKeyMethod);
  var KeyName   : string;
      NameLen   : DWORD;
      NameBuffer: array [0..MAX_PATH] of Char;
      dwIndex   : DWORD;
      retCode   : Longint;
      hKeyRoot  : HKEY = 0;
begin
   // Otwieramy klucz rejestru
   if RegOpenKeyEx(Self.RootKey, PChar(ASubKey), 0, KEY_READ, hKeyRoot) <> ERROR_SUCCESS
      then Exit;

   dwIndex := 0;
   try
      repeat
         // ustawiamy długość bufora na MAX_PATH
         NameLen := Length( NameBuffer );
         retCode := RegEnumKeyEx(hKeyRoot, dwIndex, PChar(@NameBuffer), NameLen, nil, nil, nil, nil);

         if retCode = ERROR_SUCCESS then begin
            // filtr przez predykat
            if (not Assigned(APredicate)) or APredicate(PChar(@NameBuffer)) then begin
               SetString(KeyName, PChar(@NameBuffer), NameLen);
               AKeyMtd( Self, ASubKey, KeyName );
            end;
            Inc(dwIndex);
         end else if retCode = ERROR_NO_MORE_ITEMS then
            Break
         else
            raise Exception.CreateFmt('%s (%d)', [ SysErrorMessage(retCode), retCode ] );
      until False;
   finally
      RegCloseKey(hKeyRoot);
   end;
end;

end .

