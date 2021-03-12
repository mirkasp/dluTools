
{$IFDEF FPC}

  {$mode objfpc}{$H+}
  {$modeswitch UNICODESTRINGS+}

  {$DEFINE dlu_Generics}
  {$DEFINE dlu_Unicode}

{$ELSE}

  {$IFDEF CONDITIONALEXPRESSIONS}

    {$IF CompilerVersion >= 20} //D2009 or higher
       {$DEFINE dlu_Anonymous}
       {$DEFINE dlu_Unicode}
    {$IFEND}

    {$IF CompilerVersion >= 21.0}
       {$DEFINE dlu_Generics}
    {$IFEND}

  {$ENDIF}

{$ENDIF}

