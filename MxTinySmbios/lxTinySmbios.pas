unit lxTinySmbios;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   {$modeswitch UNICODESTRINGS+}

   {$DEFINE dlu_Generics}
   {$DEFINE dlu_Unicode}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Classes;

{$IFDEF dlu_Unicode}
type dluString = UnicodeString;
{$ELSE}
type dluString = AnsiString;
{$ENDIF}

type TsmBiosTableType = Byte;

type TSmBiosTableHeader = packed record    {4 bytes}
   TableType           : TsmBiosTableType;
   Length              : Byte;
   Handle              : Word;
end;

type

{ TuTinySmbios }

 TuTinySmbios = class
    strict private
      fpMasterBuffer   : PByte;
      fpRawData        : PByte;           // pointer to formatted and unformatted data area - all tables
      fpGuardPtr       : PByte;
      fTableType       : TsmBiosTableType;
      fSearchNextPos   : integer;
      fVersionStr      : dluString;
      FVersion         : Cardinal;
      procedure DataInit;
      function GetCount: integer;
      function GetVersionStr: dluString;
    strict protected
      fTablesList      : TList;
      class function IsBit( const AValue: UInt64; const ABit: integer ): boolean; inline;
      function GetSmbiosString( const ATableStart: PByte; const AIndex: byte ): dluString;
      function GetVersionAsCardinal( const AMajorVer, AMinorVer: byte; const ARevision: byte = 0 ): Cardinal;
      function SMBIOS_GE( const AMajorVer, AMinorVer: byte; const ARevision: byte = 0 ): boolean;
    public
      constructor Create( const ATableType: TsmBiosTableType );
      destructor Destroy; override;
      //
      function GetFirst( var VTablePtr: PByte ): boolean;
      function GetNext( var VTablePtr: PByte ): boolean;
      //
      // number of items
      property Count: integer read GetCount;
      property VersionStr: dluString read GetVersionStr;
end;

implementation

uses SysUtils
   {$IFDEF FPC}
   , LCLIntf, LCLType
   {$ENDIF}
   ;

const ERR_MSG = 'Error reading System Firmware Table (%d)';

type DWORD    = Cardinal;

function GetSystemFirmwareTable( FirmwareTableProviderSignature: DWORD;
                                 FirmwareTableID               : DWORD;
                                 pFirmwareTableBuffer          : pointer;
                                 BufferSize: DWORD ): DWORD; stdcall; external 'kernel32';

function CompareFunc( Item1, Item2: Pointer): Integer;
begin
   Result := {%H-}DWORD(Item1) - {%H-}DWORD(Item2);
end;

{ TuTinySmbios }

constructor TuTinySmbios.Create(const ATableType: TsmBiosTableType);
   const FirmwareTableProviderSignature = $52534D42;  // 'RSMB'

   type TuSmbiosRawDataHeader = packed record
      Used20CallingMethod : Byte;
      SMBIOSMajorVersion  : Byte;
      SMBIOSMinorVersion  : Byte;
      DmiRevision         : Byte;
      DataLength          : DWORD;       // structure length without this header
   end;

   type PuSmbiosRawDataHeader = ^TuSmbiosRawDataHeader;

   var BufferSize : DWORD;
begin
   inherited Create;

   BufferSize := GetSystemFirmwareTable( FirmwareTableProviderSignature, 0, nil, 0);
   if BufferSize = 0
      then raise Exception.CreateFmt( ERR_MSG, [ 0 ]  );

   GetMem( fpMasterBuffer, BufferSize );
   if GetSystemFirmwareTable( FirmwareTableProviderSignature, 0, fpMasterBuffer, BufferSize ) <> BufferSize
      then raise Exception.CreateFmt( ERR_MSG, [ 1 ] );

   fpRawData       := fpMasterBuffer + SizeOf( TuSmbiosRawDataHeader );
   fpGuardPtr      := fpRawData + PuSmbiosRawDataHeader( fpMasterBuffer )^.DataLength - SizeOf( TSmBiosTableHeader );

   fTablesList     := TList.Create;
   fTableType      := ATableType;
   fSearchNextPos  := MaxInt;
   DataInit();

   with PuSmbiosRawDataHeader( fpMasterBuffer )^ do begin
      FVersion     := GetVersionAsCardinal( SMBIOSMajorVersion, SMBIOSMinorVersion, DmiRevision );
      fVersionStr  := UnicodeFormat( '%d.%d.%d', [ SMBIOSMajorVersion, SMBIOSMinorVersion, DmiRevision ] );
   end;
end;

destructor TuTinySmbios.Destroy;
begin
   fTablesList.Free;
   FreeMem( fpMasterBuffer );
   inherited Destroy;
end;

procedure TuTinySmbios.DataInit;
   type TSMBTableDescr = record
      TableHeader         : TSmBiosTableHeader;
      TableFullLength     : Cardinal;              // full table length (formatted and unformatted part)
      TableStartPtr       : PByte;                 // begin table index in RawData
   end;

   function PrepareNextTable( var tx: TSMBTableDescr ): boolean;
     var p : PByte;
   begin
      // it is possible to read next table ?
      p := tx.TableStartPtr;                        // previous table
      Inc( p, tx.TableFullLength );                 // add previous table full length
      Result := p < fpGuardPtr;
      if Result then begin
         tx.TableStartPtr := p;
         Move( p^, tx.TableHeader, SizeOf( tx.TableHeader ) );
         // get length of unformatted part
         Inc( p, tx.TableHeader.Length );
         repeat
             Inc( p, Length( PAnsiChar(p) )+1 );
         until p^ = 0;
         tx.TableFullLength := p - tx.TableStartPtr + 1;
      end;
   end;

   function PrepareFirstTable( var tx: TSMBTableDescr ): boolean;
   begin
      tx.TableFullLength := 0;       // fake length of previous table
      tx.TableStartPtr   := fpRawData;
      Result := PrepareNextTable( tx );
   end;

  var tx : TSMBTableDescr;
begin
   if PrepareFirstTable( tx{%H-} ) then begin
      repeat
         if tx.TableHeader.TableType = fTableType then fTablesList.Add( tx.TableStartPtr );
      until not PrepareNextTable( tx );
   end;

   fTablesList.Sort( @CompareFunc );

end;

function TuTinySmbios.GetCount: integer;
begin
   Result := fTablesList.Count;
end;

function TuTinySmbios.GetVersionStr: dluString;
begin
   Result := fVersionStr;
end;

class function TuTinySmbios.IsBit(const AValue: UInt64; const ABit: integer ): boolean;
begin
   Result := ((AValue shr ABit) and $1) = $1;
end;

function TuTinySmbios.GetSmbiosString(const ATableStart: PByte; const AIndex: byte): dluString;
  type PSmBiosTableHeader = ^TSmBiosTableHeader;
  var p : PByte;
      i : byte;
      s : AnsiString;
begin
   p := ATableStart;
   Inc( p, PSmBiosTableHeader( ATableStart )^.Length );
   i := 0;
   repeat
      Inc( i );
      s := PAnsiChar(p);
      Inc( p, Length( s )+1 );
   until (p^ = 0) or (i = AIndex ) or (i > 100);

   if i = AIndex
      then Result := dluString( s )
      else Result := 'ERROR';
end;

function TuTinySmbios.GetVersionAsCardinal(const AMajorVer, AMinorVer: byte; const ARevision: byte): Cardinal;
  const n1 : integer = BitSizeOf( byte );
begin
   Result := (AMajorVer shl (2*n1) ) or (AMinorVer shl n1) or ARevision;
end;

function TuTinySmbios.SMBIOS_GE(const AMajorVer, AMinorVer: byte; const ARevision: byte): boolean;
begin
   Result := (FVersion >= GetVersionAsCardinal( AMajorVer, AMinorVer, ARevision ) );
end;

function TuTinySmbios.GetFirst(var VTablePtr: PByte): boolean;
begin
   fSearchNextPos := 0;
   Result := GetNext( VTablePtr );
end;

function TuTinySmbios.GetNext(var VTablePtr: PByte): boolean;
begin
   Result := fSearchNextPos < fTablesList.Count;
   if Result then begin
      VTablePtr := fTablesList.Items[ fSearchNextPos ];
      Inc( fSearchNextPos );
   end;
end;

end.

