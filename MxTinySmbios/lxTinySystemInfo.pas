unit lxTinySystemInfo;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   {$modeswitch UNICODESTRINGS+}

   {$DEFINE dlu_Generics}
   {$DEFINE dlu_Unicode}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses lxTinySmbios;

{$IFDEF dlu_Unicode}
type dluString = UnicodeString;
{$ELSE}
type dluString = AnsiString;
{$ENDIF}

type

{ TuTinySystemInfo }

 TuTinySystemInfo = class( TuTinySmbios )
    strict private
    public
      constructor Create;
      function GetInfo(): dluString;
end;


function GetProductInfo() : dluString;


implementation

uses SysUtils;


function GetProductInfo(): dluString;
begin
   with TuTinySystemInfo.Create do begin
      Result := GetInfo();
      Free;
   end;
end;

const PROC_TABLE_CODE = $01;

type TxSystemInfo = packed record
    Header       : TSmBiosTableHeader;
    Manufacturer : Byte;                       // 2.0+     STRING index
    ProductName  : Byte;                       // 2.0+     STRING index
    Version      : Byte;                       // 2.0+     STRING index
    SerialNumber : Byte;                       // 2.0+     STRING index
    UUID         : array [0 .. 15] of Byte;    // 2.1+
    WakeUpType   : Byte;                       // 2.1+
    SKUNumber    : Byte;                       // 2.4+
    Family       : Byte;                       // 2.4+
end;

type PxSystemInfo = ^TxSystemInfo;

{ TuTinySystemInfo }

constructor TuTinySystemInfo.Create;
begin
   inherited Create( PROC_TABLE_CODE );
end;

function TuTinySystemInfo.GetInfo(): dluString;
  var p : PByte;
begin
   if fTablesList.Count > 0 then begin
      p := fTablesList[ 0 ];
      Result := GetSmbiosString( p, PxSystemInfo( p )^.Manufacturer );
      if Result <> '' then Result := ' (' + Result + ')';
      Result := GetSmbiosString( p, PxSystemInfo( p )^.ProductName ) + Result;
   end else begin
      Result := '';
   end;
end;

end.

