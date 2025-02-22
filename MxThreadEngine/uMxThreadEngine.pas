unit uMxThreadEngine;
//******************************************************************************
// Universal Multithread Engine
//

// Usage:
//   1. You should create TMxT_Base descended class in your program.
//      [example]
//   2. In main module create specialized TMxEngine<A,B> object.
//
//******************************************************************************
{$mode ObjFPC}{$H+}

interface

uses Classes
   , dluGenQueue
   , uMxThreadBase
   , dluStopper
   ;


{ TMxEngine }
type generic TMxEngine<_T: TMxT_Base; _R: record> = class
   strict private
      type TMxSubjects        = specialize TuQueue<_R>;
           //
      var  FMaxThreads        : integer;
           FNumberOfThreads   : integer;
           FCurrentThread     : integer;
           FThreads           : array of _T;
           //
           FFinished          : boolean;
           FLastExecuteTime   : TMsClockCounter;
           FStopper           : TuStopper;
           //
           FMxSubjectsCount   : integer;
           FMxSubjects        : TMxSubjects;
           //
           FOnEngineStart     : TNotifyEvent;
           FOnEngineStop      : TNotifyEvent;
           //
           FOnThreadStart     : TNotifyEvent;
           FOnThreadProgress  : TNotifyEvent;
           FOnThreadStop      : TNotifyEvent;
           //
      procedure DoOnEngineStart(); dynamic;
      procedure DoOnEngineStop(); dynamic;
      //
      function TryServiceNextSubject(): boolean;
      procedure ThreadTerminate( Sender: TObject );
      //
   public
      constructor Create( const AMaxThreads: integer = 0 );
      destructor Destroy; override;
      //
      procedure AddSubjectParams( const APars: _R );
      procedure ClearSubjectParams;
      procedure Terminate;
      procedure Execute();
      //
      property OnEngineStart    : TNotifyEvent    read FOnEngineStart    write FOnEngineStart;
      property OnEngineStop     : TNotifyEvent    read FOnEngineStop     write FOnEngineStop;
      //
      property OnThreadStart    : TNotifyEvent    read FOnThreadStart    write FOnThreadStart;
      property OnThreadProgress : TNotifyEvent    read FOnThreadProgress write FOnThreadProgress;
      property OnThreadStop     : TNotifyEvent    read FOnThreadStop     write FOnThreadStop;
      //
      property MaxThreads       : integer         read fMaxThreads;
      property Finished         : boolean         read FFinished;
      property SubjectsCount    : integer         read FMxSubjectsCount;
      property LastExecuteTime  : TMsClockCounter read FLastExecuteTime;
end;

implementation

uses SysUtils
   , dluCpuCount
   ;

{ TMxEngine }
constructor TMxEngine.Create( const AMaxThreads : integer );
begin
   inherited Create;

   if AMaxThreads < 1 then begin
      FMaxThreads := GetLogicalCpuCount() - 1;
      if FMaxThreads < 1 then FMaxThreads := 1;
   end else begin
      FMaxThreads := AMaxThreads;
   end;

   FFinished    := true;
   FMxSubjects  := TMxSubjects.Create;
   FMxSubjectsCount := 0;

   Randomize;

end;

destructor TMxEngine.Destroy;
  var i: integer;
begin
   for i:=0 to High(FThreads) do
      if Assigned( FThreads[i] ) then FreeAndNil( FThreads[i] );
   FThreads := nil;
   FMxSubjects.Free;
   inherited Destroy;
end;

procedure TMxEngine.AddSubjectParams( const APars: _R );
begin
   FMxSubjects.Push( APars );
   Inc( FMxSubjectsCount );
end;

procedure TMxEngine.ClearSubjectParams;
begin
   with FMxSubjects do
      while not IsEmpty do Pop;
   FMxSubjectsCount := 0;

end;

procedure TMxEngine.Terminate;
  var i: integer;
begin
   for i:=0 to High(FThreads) do
      if Assigned( FThreads[i] ) then FThreads[i].Terminate;
end;

procedure TMxEngine.DoOnEngineStart();
   var i: integer;
begin
   if Assigned( FOnEngineStart ) then FOnEngineStart( self );

   FFinished := false;

   FThreads         := nil;
   SetLength( FThreads, FMaxThreads );
   for i:=0 to High( FThreads ) do FThreads[i] := nil;

   FNumberOfThreads := 0;   // total count of running threads
   FCurrentThread   := 0;   // index of next created thread
   FStopper.Start;
   FLastExecuteTime := 0;
end;

procedure TMxEngine.DoOnEngineStop();
begin
   FLastExecuteTime := FStopper.LapTime;
   if FNumberOfThreads <> 0 then raise Exception.Create( 'Fatal error in MxEngine' );
   FFinished := true;
   if Assigned( FOnEngineStop ) then FOnEngineStop( self );
end;

function TMxEngine.TryServiceNextSubject(): boolean;
  var subj : _R;
      thrd : _T;
begin
   subj   := Default( _R );
   Result := FMxSubjects.TryPop( subj );
   if Result then begin
      thrd := _T.Create( FCurrentThread, @subj );
      fThreads[ FCurrentThread ] := thrd;
      Inc( FCurrentThread );
      Inc( FNumberOfThreads );
      with thrd do begin

          OnTerminate      := @ThreadTerminate;
          FreeOnTerminate  := false;

          OnThreadStart    := FOnThreadStart;
          OnThreadProgress := FOnThreadProgress;
          OnThreadStop     := FOnThreadStop;

          Start;
      end;
   end;
end;

procedure TMxEngine.ThreadTerminate( Sender : TObject) ;
begin
   FCurrentThread := (Sender as _T).Token;
   FreeAndNil( FThreads[ FCurrentThread ] );
   Dec( FNumberOfThreads );

   if not FMxSubjects.IsEmpty then
       TryServiceNextSubject()
   else if FNumberOfThreads = 0 then
       DoOnEngineStop();

end;

procedure TMxEngine.Execute();
begin
   DoOnEngineStart();

   if not FMxSubjects.IsEmpty then begin
      while FNumberOfThreads < fMaxThreads do begin
         if not TryServiceNextSubject() then break;
      end;

   end else begin
      DoOnEngineStop();
   end;

end;

end.

