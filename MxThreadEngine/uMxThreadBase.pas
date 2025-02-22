unit uMxThreadBase;

{$mode ObjFPC}{$H+}

interface

uses Classes;

{ TMxT_Base }
type TMxT_Base = class abstract (TThread)
   strict private
      FOnThreadStart    : TNotifyEvent;
      FOnThreadProgress : TNotifyEvent;
      FOnThreadStop     : TNotifyEvent;
      FParamsPtr        : Pointer;
      fToken            : integer;
   protected
      procedure DoOnThreadStart(); dynamic;
      procedure DoOnThreadProgress(); dynamic;
      procedure DoOnThreadStop(); dynamic;
   public
      constructor Create( const AToken: integer; const AParamsPtr: Pointer ); virtual;
      procedure Execute; override;
      //
      property OnThreadStart    : TNotifyEvent read FOnThreadStart    write FOnThreadStart;
      property OnThreadProgress : TNotifyEvent read FOnThreadProgress write FOnThreadProgress;
      property OnThreadStop     : TNotifyEvent read FOnThreadStop     write FOnThreadStop;
      //
      property ParamsPtr        : Pointer      read FParamsPtr;
      property Token            : integer      read fToken;
end;

implementation

uses SysUtils;

{ TMxT_Base }

constructor TMxT_Base.Create( const AToken: integer; const AParamsPtr: Pointer );
begin
   fToken       := AToken;
   FParamsPtr   := AParamsPtr;
   inherited Create( true );
end;

procedure TMxT_Base.Execute;
begin
   {------------}
   {} Synchronize( @DoOnThreadStart );
   {------------}
end;

procedure TMxT_Base.DoOnThreadStart();
begin
   if Assigned( FOnThreadStart )
      then FOnThreadStart( self );
end;

procedure TMxT_Base.DoOnThreadProgress();
begin
   if Assigned( FOnThreadProgress )
      then FOnThreadProgress( self );
end;

procedure TMxT_Base.DoOnThreadStop();
begin
   if Assigned( FOnThreadStop )
      then FOnThreadStop( self );
end;

end.

