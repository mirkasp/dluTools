unit dluStopper;

{$I dluOptions.inc}
{$IFDEF FPC}
{$modeswitch ADVANCEDRECORDS+}
{$ENDIF}

interface

type TMsClockCounter = {$IFDEF FPC}QWord{$ELSE}Cardinal{$ENDIF} ;


type TuStopper = record
  strict private
    fIsRunning  : boolean;
    fElapsed    : TMsClockCounter;
    fLapTime    : TMsClockCounter;
  public
    procedure Start;
    function Stop: TMsClockCounter;
    function LapTime: TMsClockCounter;
end;

implementation

uses Windows;

{ TuStopper }

function TickCount(): TMsClockCounter; inline;
begin
   {$IFDEF FPC}
   Result := GetTickCount64();
   {$ELSE}
   Result := GetTickCount();
   {$ENDIF};
end;


function TuStopper.LapTime: TMsClockCounter;
begin
   Result := fLapTime;
end;

procedure TuStopper.Start;
begin
   fIsRunning := true;
   fLapTime   := 0;
   fElapsed   := TickCount();
end;

function TuStopper.Stop: TMsClockCounter;
begin
   if fIsRunning then begin
      fLapTime := TickCount() - fElapsed;
      Result   := fLapTime;
   end else
      Result := 0;
end;

end.
