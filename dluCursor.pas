unit dluCursor;
// 2020.05.15 - generic type TuStack<T>
// 2019.10.31 - first oficial release
//


{$I dluOptions.inc}

//{$IFNDEF FPC}
//  {$IFDEF CONDITIONALEXPRESSIONS}
//     {$IF CompilerVersion >= 21.0}
//        {$DEFINE dlu_Generics}
//     {$IFEND}
//  {$ENDIF}
//{$ENDIF}

interface

uses Controls;

const crHourGlass = Controls.crHourGlass;

procedure PushCursor( const NewCursor: TCursor = crHourGlass );
procedure PopCursor();

implementation

uses Forms, dluGenStack
     ;

type TuCurStack = {$IFDEF FPC}specialize{$ENDIF} TuStack<TCursor>;

var CurStack : TuCurStack;

procedure PushCursor( const NewCursor: TCursor );
begin
   CurStack.Push( Screen.Cursor );
   Screen.Cursor := NewCursor;
end;

procedure PopCursor();
begin
   if CurStack.IsEmpty
      then Screen.Cursor := Controls.crDefault
      else Screen.Cursor := CurStack.Pop;
end;

initialization
  CurStack := TuCurStack.Create(10);

finalization
  CurStack.Free;

end.
