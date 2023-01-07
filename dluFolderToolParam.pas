unit dluFolderToolParam;

{$I dluOptions.inc}

interface

uses Windows, Classes
{$IFDEF FPC}
   , fgl
{$ELSE}
   , Generics.Collections
{$ENDIF}
   , dluSplitString
//   , dluFileDate
   ;

{$IFDEF FPC}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
type TWin32FindData = TWin32FindDataW;
type PWin32FindData = PWin32FindDataW;
{$ENDIF}
type TFilterMethod  = function( const AFindData: TWin32FindData ): boolean of object;

type

{ TuFolderToolParam }

 TuFolderToolParam = class
   strict private
     type TIsMatchingFunc = function( const fn: PChar ): boolean of object;
     type TFuncList       = {$IFDEF FPC}specialize TFPGList<TFilterMethod>{$ELSE}TList<TFilterMethod>{$ENDIF} ;
     var
        fMtchFuncList      : TFuncList;
        fMasterSem         : boolean;    // czy jest zdefiniowany jakikolwiek filtr ?
        //
        fFileNameMask      : string;
        fMaskTabStr        : TUnicodeStringDynArray;
        fIsMatchingFunc    : TIsMatchingFunc;
        // technical
        fFreeonRelease     : boolean;
      procedure Initialize();
      procedure SetFileNameMask(const Value: string);
      function IsMatchingQ0( const fn: PChar ): boolean;
      function IsMatchingQ1( const fn: PChar ): boolean;
      function IsMatchingQ2( const fn: PChar ): boolean;
      function GetIsEmptyMask: boolean;
      //
    public
      class function IsWin7OrAbove: boolean;
      constructor Create( const AFileNameMask: string = '' );
      destructor Destroy; override;
      property FreeOnRelease   : boolean read fFreeonRelease write fFreeOnRelease;
      procedure AddFilterFunc( AFunc: TFilterMethod );
      procedure ClearFilterFuncs();
      //
      function MatchingWith( const AFindData: TWin32FindData ): boolean;
      //
      property FileNameMask : string  read fFileNameMask;
      property IsEmptyMask  : boolean read GetIsEmptyMask;
end;

implementation

uses SysUtils;

//const PMSF_NORMAL   = $00000000;
const PMSF_MULTIPLE = $00000001;

function PathMatchSpec( pszFile, pszSpec: PWideChar ): BOOL; stdcall; external 'shlwapi.dll' name 'PathMatchSpecW';
function PathMatchSpecEx( pszFile, pszSpec: PWideChar; dwFlags: DWORD ): HRESULT; stdcall; external 'shlwapi.dll' name 'PathMatchSpecExW';

{ TuFolderToolParam }

class function TuFolderToolParam.IsWin7OrAbove: boolean;
begin
   Result := ((Win32MajorVersion = 6) AND (Win32MinorVersion>= 1))  { Any version 6 equal and above Win7 }
              OR (Win32MajorVersion>= 7);                             { Any version above 6 }
end;

constructor TuFolderToolParam.Create(const AFileNameMask: string);
begin
   inherited Create;
   Initialize();
   SetFileNameMask( AFileNameMask );
end;

procedure TuFolderToolParam.Initialize;
begin
   fMtchFuncList   := TFuncList.Create;
   fFreeonRelease  := false;
   fMasterSem      := false;
   fFileNameMask   := '';
end;

destructor TuFolderToolParam.Destroy;
begin
   fMtchFuncList.Free;
   inherited;
end;

function TuFolderToolParam.GetIsEmptyMask: boolean;
begin
   Result := (fFileNameMask = '') or (fFileNameMask= '*') or (fFileNameMask='*.*')
end;

function TuFolderToolParam.MatchingWith( const AFindData: TWin32FindData): boolean;
   var i : integer;
begin

   if fMasterSem and Assigned( fIsMatchingFunc )
      then Result := fIsMatchingFunc( Addr( AFindData.cFileName ) )
      else Result := true;

   i := fMtchFuncList.Count - 1;
   while Result and (i >= 0 ) do begin
      Result := TFilterMethod( fMtchFuncList[i] )( AFindData );
      Dec(i);
   end;

end;

procedure TuFolderToolParam.AddFilterFunc(AFunc: TFilterMethod);
begin
   fMtchFuncList.Add( AFunc );
end;

procedure TuFolderToolParam.ClearFilterFuncs;
begin
   fMtchFuncList.Clear;
end;

procedure TuFolderToolParam.SetFileNameMask(const Value: string);
begin
   fFileNameMask := Value;
   if GetIsEmptyMask()
         then fMaskTabStr := nil
         else fMaskTabStr := dluSplitString.SplitString( Value, ';' );

   fMasterSem := Assigned( fMaskTabStr );

   if Length( fMaskTabStr ) <= 1 then begin

      fIsMatchingFunc := {$IFDEF FPC}@{$ENDIF}IsMatchingQ1;

   end else if IsWin7OrAbove() then begin

      fIsMatchingFunc := {$IFDEF FPC}@{$ENDIF}IsMatchingQ2;

   end else begin

      fIsMatchingFunc := {$IFDEF FPC}@{$ENDIF}IsMatchingQ0;

   end;

end;

function TuFolderToolParam.IsMatchingQ0(const fn: PChar): boolean;
  var i : integer;
begin
   Result := false;
   i      := Low( fMaskTabStr );
   while not Result and (i <= High(fMaskTabStr)) do
      if PathMatchSpec( fn, PChar( fMaskTabStr[i] ) )
         then Result := true
         else Inc(i);
end;

function TuFolderToolParam.IsMatchingQ1( const fn: PChar) : boolean;
begin
   Result := PathMatchSpec( fn, PChar( fFileNameMask ) );
end;

function TuFolderToolParam.IsMatchingQ2( const fn: PChar) : boolean;
begin
   Result := PathMatchSpecEx( fn, PChar( fFileNameMask ), PMSF_MULTIPLE ) = S_OK;
end;

end.
