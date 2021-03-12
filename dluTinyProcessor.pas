unit dluTinyProcessor;

{$I dluOptions.inc}

interface

uses dluTinySmbios;

{$IFDEF dlu_Unicode}
type dluString = UnicodeString;
{$ELSE}
type dluString = AnsiString;
{$ENDIF}

type

{ TuTinyProcessor }

 TuTinyProcessor = class( TuTinySmbios )
    strict private
      function GetSpeedValue(  const AValue: word  ): dluString;
      function GetProcessorVoltage( const AValue: byte ): dluString;
    public
      constructor Create;
      function GetInfo( const AFullInfo: boolean = false; const AItemId: integer = 0 ): dluString;
end;


function GetProcessorInfo( const AFullInfo: boolean = false; const AItemId: integer = 0 ): dluString;


implementation

uses SysUtils;


function GetProcessorInfo( const AFullInfo: boolean = false; const AItemId: integer = 0 ): dluString;
begin
   with TuTinyProcessor.Create do begin
      Result := GetInfo( AFullInfo, AItemId );
      if Result = '' then Result := dluString( SysUtils.GetEnvironmentVariable( 'PROCESSOR_IDENTIFIER' ) );
      Free;
   end;
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
end;

type PxProcessor = ^TxProcessor;

{ TuTinyProcessor }

function IIF( const AValue: boolean; const sTrue, sFalse: dluString ): dluString; inline;
begin
   if AValue then Result := sTrue else Result := sFalse;
end;

constructor TuTinyProcessor.Create;
begin
   inherited Create( PROC_TABLE_CODE );
end;

function TuTinyProcessor.GetSpeedValue(const AValue: word): dluString;
begin
   if AValue = 0 then Result := ''
   else Result := dluString( Format( '%d MHz', [ AValue ] ) );
end;

function TuTinyProcessor.GetProcessorVoltage(const AValue: byte): dluString;
begin
   if IsBit( AValue, 7 ) then begin
      // standard mode
      Result := dluString( Format( '%.1f V', [ 0.1 * (AValue and $7F) ] ) );
   end else begin
      // legacy mode
      if IsBit( AValue, 0 ) then Result := '5 V' else Result := '';
      if IsBit( AValue, 1 ) then begin
         Result := IIF( Result <> '', ', ', '' ) + '3.3 V';
      end;
      if IsBit( AValue, 2 ) then begin
         Result := IIF( Result <> '', ', ', '' ) + '2.9 V';
      end;
      if (AValue and $07) <> 0 then Result := 'configurable: ' + Result;
   end;
end;

function TuTinyProcessor.GetInfo(const AFullInfo: boolean; const AItemId: integer): dluString;
  var p : PByte;
begin

   if (AItemId < fTablesList.Count) and (AItemId >= 0) then begin

      p := fTablesList[ AItemId ];
      with PxProcessor( p )^ do begin
         Result := GetSmbiosString( p, ProcessorVersion );
         if AFullInfo then
            Result := Result + dluString( Format( ', %s, %s, %s',
                                       [ GetSmbiosString( p, SocketDesignation ),
                                         GetSpeedValue( MaxSpeed ),
                                         GetProcessorVoltage( Voltage )
                                       ]
                                     ) );
      end;
   end else
      Result := '';
end;

end.

