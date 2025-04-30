unit lxTinyProcessor;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   {$modeswitch UNICODESTRINGS+}
   {$modeswitch ADVANCEDRECORDS+}

   {$DEFINE dlu_Generics}
   {$DEFINE dlu_Unicode}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Classes, lxTinySmbios;

{$IFDEF dlu_Unicode}
type dluString = UnicodeString;
{$ELSE}
type dluString = AnsiString;
{$ENDIF}

{ TuTinyProcessorParam }
type TuTinyProcessorParam = record
    VersionStr   : UnicodeString;
    SocketStr    : UnicodeString;
    MaxSpeed     : Word;
    Voltage      : Byte;
    Cores        : Word;
    function MaxSpeedStr: UnicodeString;
    function VoltageStr: UnicodeString;
end;


{ TuTinyProcessor }
type TuTinyProcessor = class( TuTinySmbios )
    public
      constructor Create;
      function GetInfo( const AFullInfo: boolean = false; const AItemId: integer = 0 ): dluString;
      function GetInfoEx(out AInfo: TuTinyProcessorParam; const AItemId: integer = 0 ): boolean;
end;


function GetProcessorInfo( const AFullInfo: boolean = false; const AItemId: integer = 0 ): dluString;
function GetProcInfo(out AInfo: TuTinyProcessorParam; const AItemId: integer=0): boolean;

implementation

uses SysUtils
   //, dluCpuCount
   ;


function GetProcessorInfo( const AFullInfo: boolean = false; const AItemId: integer = 0 ): dluString;
begin
   with TuTinyProcessor.Create do begin
      Result := GetInfo( AFullInfo, AItemId );
      Free;
   end;
   if Result = ''
      then Result := dluString( SysUtils.GetEnvironmentVariable( 'PROCESSOR_IDENTIFIER' ) );

end;

function GetProcInfo( out AInfo: TuTinyProcessorParam; const AItemId: integer=0 ): boolean;
begin
   with TuTinyProcessor.Create() do begin
      Result := GetInfoEx( AInfo, AItemId );
      Free;
   end;
   if not Result
      then AInfo.VersionStr := SysUtils.GetEnvironmentVariable( 'PROCESSOR_IDENTIFIER' );
end;

const PROC_TABLE_CODE = $04;

type TxProcessor = packed record
   Header                  : TSmBiosTableHeader;
   SocketDesignation       : Byte;     // 2.0+  text index
   ProcessorType           : Byte;     // 2.0+  dict
   ProcessorFamily         : Byte;     // 2.0+  dict
   ProcessorManufacturer   : Byte;     // 2.0+  text index
   ProcessorID             : UInt64;   // 2.0+  QWORD; Raw processor identification data.
   ProcessorVersion        : Byte;     // 2.0+  text index
   Voltage                 : Byte;     // 2.0+  Two forms of information can be specified by the SMBIOS in this
   ExternalClock           : Word;     // 2.0+  External Clock Frequency, in MHz. If the value is unknown, the field
   MaxSpeed                : Word;     // 2.0+  Maximum processor speed (in MHz) supported by the system for this
   CurrentSpeed            : Word;     // 2.0+  Same format as Max Speed
   Status                  : Byte;     // 2.0+  Bit 7 Reserved, must be zero
   ProcessorUpgrade        : Byte;     // 2.0+  Processor Upgrade field
   //-----------------------------------------------------------------------------------------
   L1CacheHandle           : Word;     // 2.1+  The handle of a Cache Information structure that defines the
   {p.26}                              //       attributes of the primary (Level 1) cache for this processor. For
                                       //       version 2.1 and version 2.2 implementations, the value is 0FFFFh if
                                       //       the processor has no L1 cache. For version 2.3 and later
                                       //       implementations, the value is 0FFFFh if the Cache Information
                                       //       structure is not provided.
   L2CacheHandle           : Word;     // 2.1+  The handle of a Cache Information structure that defines the
   {p.28}                              //       attributes of the secondary (Level 2) cache for this processor. For
                                       //       version 2.1 and version 2.2 implementations, the value is 0FFFFh if
                                       //       the processor has no L2 cache. For version 2.3 and later
                                       //       implementations, the value is 0FFFFh if the Cache Information
                                       //       structure is not provided.
   L3CacheHandle           : Word;     // 2.1+  The handle of a Cache Information structure that defines the
   {p.30}                              //       attributes of the tertiary (Level 3) cache for this processor. For
                                       //       version 2.1 and version 2.2 implementations, the value is 0FFFFh if
                                       //       the processor has no L3 cache. For version 2.3 and later
                                       //       implementations, the value is 0FFFFh if the Cache Information
                                       //       structure is not provided.
   SerialNumber            : Byte;     // 2.3+  text index
   AssetTag                : Byte;     // 2.3+  text index
   PartNumber              : Byte;     // 2.3+  text index
   CoreCount               : Byte;     // 2.5+  Number of cores per processor socket. If the value is unknown, the
   {p.35}                              //       field is set to 0.
                                       //       Co Count is the number of cores detected by the BIOS for this
                                       //       processor socket. It does not necessarily indicate the full
                                       //       capability of the processor. For example, platform hardware may
                                       //       have the capability to limit the number of cores reported by the
                                       //       processor without BIOS intervention or knowledge. For a dual
                                       //       processor installed in a platform where the hardware is set to
                                       //       limit it to one core, the BIOS reports a value of 1 in Core Count.
                                       //       For a dual-core processor with multi-core support disabled by BIOS,
                                       //       the BIOS reports a value of 2 in Core Count.
   CoreEnabled             : Byte;     // 2.5+  Number of enabled cores per processor socket. If the value is
   {p.36}                              //       unknown, the field is set 0.
                                       //       Core Enabled is the number of cores that are enabled by the BIOS
                                       //       and available for Operating System use. For example, if the BIOS
                                       //       detects a dual-core processor, it would report a value of 2 if it
                                       //       leaves both cores enabled, and it would report a value of 1 if it
                                       //       disables multi-core support.
   ThreadCount             : Byte;     // 2.5+  Number of threads per processor socket. If the value is unknown,
   {p.37}                              //       the field is set to 0.
                                       //       Thread Count is the total number of threads detected by the BIOS
                                       //       for this processor socket. It is a processor-wide count, not a
                                       //       thread-per-core count. It does not necessarily indicate the full
                                       //       capability of the processor. For example, platform hardware may
                                       //       have the capability to limit the number of threads reported by the
                                       //       processor without BIOS intervention or knowledge. For a dual-thread
                                       //       processor installed in a platform where the hardware is set to
                                       //       limit it to one thread, the BIOS reports a value of 1 in Thread
                                       //       Count. For a dual-thread processor with multi-threading disabled by
                                       //       BIOS, the BIOS reports a value of 2 in Thread Count. For a
                                       //       dual-core, dual-thread-per-core processor, the BIOS reports a value
                                       //       of 4 in Thread Count.
   ProcessorCharacteristics: Word;     // 2.5+  Defines which functions the processor supports.
   {p.38}
   ProcessorFamily2        : Word;     // 2.6+
   {p.40 = 28h}
   CoreCount2              : Word;     // 3.0+ Varies
   {p.42 = 2Ah}                        //      Number of Cores per processor socket.
                                       //      Supports core counts >255. If this field is present, it holds the core
                                       //      count for the processor socket. Core Count will also hold the core count,
                                       //      except for core counts that are 256 or greater. In that case, Core Count
                                       //      shall be set to FFh and Core Count 2 will hold the count. See 7.5.6.
                                       //      Legal values:
                                       //           0000h = unknown
                                       //           0001h-00FFh = core counts 1 to 255. Matches Core Count value.
                                       //           0100h-FFFEh = Core counts 256 to 65534, respectively.
                                       //           FFFFh = reserved.
                                       //
   CoreEnabled2            : Word;     // 3.0+ Varies
   {p.44 = 2Ch}                        //      Number of enabled cores per processor socket.
                                       //      Supports core enabled counts >255. If this field is present,
                                       //      it holds the core enabled count the processor socket. Core
                                       //      Enabled will also hold the core enabled count, except for
                                       //      core counts that are 256 or greater. In that case, Core Enabled
                                       //      shall be set to FFh and Core Enabled 2 will hold the count. See 7.5.7.
                                       //      Legal values:
                                       //           0000h = unknown
                                       //           0001h-00FFh = core enabled counts 1 to 255. Matches Core Enabled value.
                                       //           0100h-FFFEh = core enabled counts 256 to 65534, respectively.
                                       //           FFFFh = reserved.
                                       //
   ThreadCount2            : Word;     // 3.0+ Varies
   {p.46 = 2Eh}                        //      Number of threads per processor socket.
                                       //      Supports thread counts >255. If this field is present,
                                       //      it holds the thread count for the processor socket.
                                       //      Thread Count will also hold the thread count, except for
                                       //      thread counts that are 256 or greater. In that case,
                                       //      Thread Count shall be set to FFh and Thread Count 2 will
                                       //      hold the count. See 7.5.8.
                                       //      Legal values:
                                       //           0000h = unknown
                                       //           0001h-00FFh = thread counts 1 to 255. Matches Thread Count value.
                                       //           0100h-FFFEh = thread counts 256 to 65534, respectively.
                                       //           FFFFh = reserved.
                                       //
   ThreadEnabled           : WORD;     // 3.6+  Varies
   {p.48 = 30h }                       //       Number of enabled threads per processor socket. See 7.5.10.
                                       //       Legal values:
                                       //           0000h = unknown
                                       //           0001h-FFFEh = thread enabled counts 1 to 65534, respectively
                                       //           FFFFh = reserved
                                       //
end;

type PxProcessor = ^TxProcessor;

function IIF( const AValue: boolean; const sTrue, sFalse: dluString ): dluString; inline;
begin
   if AValue then Result := sTrue else Result := sFalse;
end;

function IsBit(const AValue: UInt64; const AIndex: integer ): boolean; inline;
begin
   Result := AValue.TestBit( AIndex );
   if Result <> (((AValue shr AIndex) and $1) = $1) then raise Exception.Create('@#$');
end;

{ TuTinyProcessorParam }

function TuTinyProcessorParam.MaxSpeedStr: UnicodeString;
begin
   if self.MaxSpeed = 0 then Result := ''
   else Result := UnicodeFormat( '%d MHz', [ self.MaxSpeed ] );
end;

function TuTinyProcessorParam.VoltageStr: UnicodeString;
begin
   if IsBit( self.Voltage, 7 ) then begin
      // standard mode
      Result := UnicodeFormat( '%.1f V', [ 0.1 * (self.Voltage and $7F) ] );
   end else begin
      // legacy mode
      if IsBit( self.Voltage, 0 ) then Result := '5 V' else Result := '';
      if IsBit( self.Voltage, 1 ) then begin
         Result := IIF( Result <> '', ', ', '' ) + '3.3 V';
      end;
      if IsBit( self.Voltage, 2 ) then begin
         Result := IIF( Result <> '', ', ', '' ) + '2.9 V';
      end;
      if (self.Voltage and $07) <> 0 then Result := 'configurable: ' + Result;
   end;

end;

{ TuTinyProcessor }

constructor TuTinyProcessor.Create;
begin
   inherited Create( PROC_TABLE_CODE );
end;

function TuTinyProcessor.GetInfo(const AFullInfo: boolean; const AItemId: integer): dluString;
  var rx : TuTinyProcessorParam;
begin
   if GetInfoEx( rx, AItemId ) then begin
      Result := rx.VersionStr;
      if AFullInfo then
         Result := Result + dluString( Format( ', %s, %s, %s',
                                       [ rx.SocketStr,
                                         rx.MaxSpeedStr,
                                         rx.VoltageStr
                                       ]
                                     ) );
   end else
      Result := '';
end;

function TuTinyProcessor.GetInfoEx(out AInfo: TuTinyProcessorParam; const AItemId: integer): boolean;
   var p : PByte;
begin
   Result := (AItemId < fTablesList.Count) and (AItemId >= 0);
   if Result then begin
      p := fTablesList[ AItemId ];
      with PxProcessor( p )^ do begin
         AInfo.VersionStr := GetSmbiosString( p, ProcessorVersion  );
         AInfo.SocketStr  := GetSmbiosString( p, SocketDesignation );
         AInfo.MaxSpeed   := MaxSpeed;
         AInfo.Voltage    := Voltage;

         if SMBIOS_GE( 2, 5 ) then begin
            AInfo.Cores := CoreCount;
            if (AInfo.Cores = $FF) and SMBIOS_GE( 3, 0 )
               then AInfo.Cores := CoreCount2;
         end else
            AInfo.Cores := 1;
      end;
   end;
end;

end.

