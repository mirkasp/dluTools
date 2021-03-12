unit dluLoadLibrary;

interface

function GetWindowsFunction( const ALibrary, AFuncName: string; var AHandle: THandle; var AFuncPtr: Pointer): boolean;

implementation

uses Windows;

function GetWindowsFunction( const ALibrary, AFuncName: string; var AHandle: THandle; var AFuncPtr: Pointer): boolean;
begin
   AHandle := LoadLibrary( PChar(ALibrary) );
   Result  := (AHandle > 0);
   if Result then begin
      AFuncPtr := GetProcAddress( AHandle, PChar(AFuncName) );
      Result   := Assigned( AFuncPtr );
   end;
end;



end.
