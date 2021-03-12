unit dluStringStack;

{$I dluOptions.inc}

interface

uses dluGenStack;

{$IFDEF dlu_Generics}
type TuStringStack = {$IFDEF FPC}specialize{$ENDIF} TuStack<string>;
{$ENDIF}

implementation


end.
