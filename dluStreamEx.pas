unit dluStreamEx;

{$I dluOptions.inc}

interface

uses Classes
   ;

const STREAMEX_BUFFER_SIZE = 128 * 1024;

type TCopyExProc     = procedure( const ABuffer; const ABufferSize: integer );
type TCopyExMethod   = procedure( const ABuffer; const ABufferSize: integer ) of object;
{$IFNDEF FPC}
type TCopyExDelegate = reference to procedure( const ABuffer; const ABufferSize: integer );
{$ENDIF}

// AA4B47D9EA5F7328D33FAADC4BC8296F
// AA4B47D9EA5F7328D33FAADC4BC8296F
// AA4B47D9EA5F7328D33FAADC4BC8296F
//
type TCustomCopyExMachine = class
   strict protected
      fSrc     : TFileStream;
      fDst     : TFileStream;
      pBuffer  : Pointer;
      fCnt     : integer;
   public
      constructor Create( const ASrcFileName, ADstFileName: string; const ABufferSize: integer = STREAMEX_BUFFER_SIZE);
      destructor Destroy; override;
end;

type TCopyExMachine = class(TCustomCopyExMachine)
   public
      function Execute( AProc    : TCopyExProc     ): boolean; overload;
      function Execute( AMethod  : TCopyExMethod   ): boolean; overload;
      {$IFNDEF FPC}
      function Execute( ADelegate: TCopyExDelegate ): boolean; overload;
      {$ENDIF}
end;


function CopyEx( const ASrcFileName, ADstFileName: string; AProc: TCopyExProc; const ABufferSize: integer = STREAMEX_BUFFER_SIZE ): boolean; overload;
function CopyEx( const ASrcFileName, ADstFileName: string; AMethod: TCopyExMethod; const ABufferSize: integer = STREAMEX_BUFFER_SIZE ): boolean; overload;
{$IFNDEF FPC}
function CopyEx( const ASrcFileName, ADstFileName: string; ADelegate: TCopyExDelegate; const ABufferSize: integer = STREAMEX_BUFFER_SIZE ): boolean; overload;
{$ENDIF}


implementation

uses SysUtils;

function CopyEx( const ASrcFileName, ADstFileName: string; AProc: TCopyExProc; const ABufferSize: integer = STREAMEX_BUFFER_SIZE ): boolean; overload;
begin
   with TCopyExMachine.Create( ASrcFileName, ADstFileName, ABufferSize ) do begin
       Result := Execute( AProc );
       Free;
   end;
end;

function CopyEx( const ASrcFileName, ADstFileName: string; AMethod: TCopyExMethod; const ABufferSize: integer = STREAMEX_BUFFER_SIZE ): boolean; overload;
begin
   with TCopyExMachine.Create( ASrcFileName, ADstFileName, ABufferSize ) do begin
       Result := Execute( AMethod );
       Free;
   end;
end;

{$IFNDEF FPC}
function CopyEx( const ASrcFileName, ADstFileName: string; ADelegate: TCopyExDelegate; const ABufferSize: integer = STREAMEX_BUFFER_SIZE ): boolean;
begin
   with TCopyExMachine.Create( ASrcFileName, ADstFileName, ABufferSize ) do begin
       Result := Execute( ADelegate );
       Free;
   end;
end;
{$ENDIF}


{ TAbstractWorkMachine }

constructor TCustomCopyExMachine.Create(const ASrcFileName, ADstFileName: string; const ABufferSize: integer);
begin
   inherited Create;
   fSrc := TFileStream.Create( ASrcFileName, fmOpenRead or fmShareDenyWrite );
   fDst := TFileStream.Create( ADstFileName, fmCreate or fmShareExclusive );
   GetMem( pBuffer, ABufferSize );

   // read first portion of data
   fSrc.Position := 0;
   fCnt := fSrc.Read( pBuffer^, ABufferSize );
end;

destructor TCustomCopyExMachine.Destroy;
begin
   FreeMem( pBuffer );
   fDst.Free;
   fSrc.Free;
   inherited;
end;

{ TWorkMachine }

function TCopyExMachine.Execute( AProc: TCopyExProc ): boolean;
begin
   while fCnt > 0 do begin
      AProc( pBuffer^, fCnt ) ;
      fDst.Write( pBuffer^, fCnt );
      //
      fCnt := fSrc.Read( pBuffer^, fCnt );
   end;
   Result := true;
end;

function TCopyExMachine.Execute(AMethod: TCopyExMethod): boolean;
begin
   while fCnt > 0 do begin
      AMethod( pBuffer^, fCnt ) ;
      fDst.Write( pBuffer^, fCnt );
      //
      fCnt := fSrc.Read( pBuffer^, fCnt );
   end;
   Result := true;
end;

{$IFNDEF FPC}
function TCopyExMachine.Execute(ADelegate: TCopyExDelegate): boolean;
begin
   while fCnt > 0 do begin
      ADelegate( pBuffer^, fCnt ) ;
      fDst.Write( pBuffer^, fCnt );
      //
      fCnt := fSrc.Read( pBuffer^, fCnt );
   end;
   Result := true;
end;
{$ENDIF}




end.
