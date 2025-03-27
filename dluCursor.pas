unit dluCursor;
// 2025.03.24 - "CurStack" late binding (created only before first use)
// 2020.05.15 - generic type TuStack<T>
// 2019.10.31 - first oficial release
//


{$I dluOptions.inc}

interface

uses Controls;

const crHourGlass = Controls.crHourGlass;

procedure PushCursor( const NewCursor: TCursor = crHourGlass );
procedure PopCursor();

implementation

uses Forms, dluGenStack
     ;

type TuCurStack = {$IFDEF FPC}specialize{$ENDIF} TuStack<TCursor>;

var CurStack : TuCurStack = nil;

function GetCurStack: TuCurStack;
begin
   if not Assigned( CurStack ) then CurStack := TuCurStack.Create(10);
   Result := CurStack;
end;

procedure PushCursor( const NewCursor: TCursor );
begin
   GetCurStack().Push( Screen.Cursor );
   Screen.Cursor := NewCursor;
end;

procedure PopCursor();
begin
   if GetCurStack().IsEmpty
      then Screen.Cursor := Controls.crDefault
      else Screen.Cursor := CurStack.Pop;
end;


finalization
   if Assigned( CurStack ) then CurStack.Free;

end.
