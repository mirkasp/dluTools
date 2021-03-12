unit dluMatchMaker;

{$IFDEF FPC}
  {$MODE delphiunicode}
{$ENDIF}

interface

uses dluSplitString;


{ matchmaker -  swat(ka) m (¿)
  https://pl.pons.com/t%C5%82umaczenie?q=matchmaker&l=enpl&in=ac_en&lf=en&qnac=match#dict
}

type TuMatchMaker = class
   strict private
      fPatternsTab : TStrDynArray; // array of string;
   public
      constructor Create( const APatterns: string = '' );
      function IsMatching( const AText: string ): boolean;
      procedure SetPatterns( const APatterns: string );
end;

implementation

uses Classes
    ;

{$IFDEF UNICODE}
//{$IF SizeOf(Char)>1}
//{$DEFINE UNICODE}
const cFuncName = 'PathMatchSpecW';
type PChar = ^WideChar;
{$ELSE}
const cFuncName = 'PathMatchSpecA';
type PChar = ^AnsiChar;
{$ENDIF}

function PathMatchSpec( pszFile, pszSpec: PChar ): LongBool; stdcall; external 'shlwapi.dll' name cFuncName;


{ TuMatchString }

constructor TuMatchMaker.Create(const APatterns: string);
begin
   inherited Create;
   SetPatterns( APatterns );
end;

procedure TuMatchMaker.SetPatterns(const APatterns: string);
begin
   fPatternsTab := SplitString( APatterns, ';' );
end;

function TuMatchMaker.IsMatching(const AText: string): boolean;
  var i : integer;
begin
   Result := false;
   i      := Low( fPatternsTab );
   while not Result and (i <= High(fPatternsTab)) do
      if PathMatchSpec( PChar( AText ), PChar( fPatternsTab[i] ) )
         then Result := true
         else Inc(i);
end;

end.
