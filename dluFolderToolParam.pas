unit dluFolderToolParam;

{$I dluOptions.inc}

interface

uses Windows, Classes
   , Generics.Collections
   , dluSplitString
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
     type TFuncList       = {$IFDEF FPC}specialize {$ENDIF}TList<TFilterMethod> ;
     var
        FMtchFuncList     : TFuncList;
        FMasterSem        : boolean;    // czy jest zdefiniowany jakikolwiek filtr ?
        //
        FFileNameMask     : string;
        FMaskTabStr       : TUnicodeStringDynArray;
        FIsMatchingFunc   : TIsMatchingFunc;
        // technical
        FFreeOnRelease    : boolean;
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
      property FreeOnRelease   : boolean read FFreeOnRelease write FFreeOnRelease;
      procedure AddFilterFunc( AFunc: TFilterMethod );
      procedure ClearFilterFuncs();
      //
      function MatchingWith( const AFindData: TWin32FindData ): boolean;
      //
      property FileNameMask : string  read FFileNameMask;
      property IsEmptyMask  : boolean read GetIsEmptyMask;
end;

implementation

uses SysUtils;

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
   FMtchFuncList   := TFuncList.Create;
   FFreeOnRelease  := false;
   FMasterSem      := false;
   FFileNameMask   := '';
end;

destructor TuFolderToolParam.Destroy;
begin
   FMtchFuncList.Free;
   inherited;
end;

function TuFolderToolParam.GetIsEmptyMask: boolean;
begin
   Result := (FFileNameMask = '') or (FFileNameMask= '*') or (FFileNameMask='*.*')
end;

//function TuFolderToolParam.MatchingWith( const AFindData: TWin32FindData): boolean;
//   var i : integer;
//begin
//   if FMasterSem and Assigned( FIsMatchingFunc )
//      then Result := FIsMatchingFunc( @AFindData.cFileName[0] )
//      else Result := true;
//   if not Result then Exit;
//
//   i := FMtchFuncList.Count - 1;
//   while Result and (i >= 0 ) do begin
//      Result := FMtchFuncList[i]( AFindData );
//      Dec(i);
//   end;
//end;

function TuFolderToolParam.MatchingWith(const AFindData: TWin32FindData): boolean;
   var fm: TFilterMethod;
begin
   // Dopasowanie maski
   if FMasterSem and Assigned(FIsMatchingFunc)
      then Result := FIsMatchingFunc(@AFindData.cFileName[0])
      else Result := true;

   if not Result then
      exit;

   // Wszystkie filtry muszą zwrócić TRUE
   for fm in FMtchFuncList do
      if not fm(AFindData) then
         Exit(FALSE);
end;



procedure TuFolderToolParam.AddFilterFunc(AFunc: TFilterMethod);
begin
   FMtchFuncList.Add( AFunc );
end;

procedure TuFolderToolParam.ClearFilterFuncs;
begin
   FMtchFuncList.Clear;
end;

procedure TuFolderToolParam.SetFileNameMask(const Value: string);
begin
   FFileNameMask := Value;
   if GetIsEmptyMask()
         then FMaskTabStr := nil
         else FMaskTabStr := dluSplitString.SplitString( Value, ';' );

   FMasterSem := Length(FMaskTabStr) > 0;

   if Length( FMaskTabStr ) <= 1 then begin

      FIsMatchingFunc := {$IFDEF FPC}@{$ENDIF}IsMatchingQ1;

   end else if IsWin7OrAbove() then begin

      FIsMatchingFunc := {$IFDEF FPC}@{$ENDIF}IsMatchingQ2;

   end else begin

      FIsMatchingFunc := {$IFDEF FPC}@{$ENDIF}IsMatchingQ0;

   end;

end;

function TuFolderToolParam.IsMatchingQ0(const fn: PChar): boolean;
  var i : integer;
begin
   Result := false;
   i      := Low( FMaskTabStr );
   while not Result and (i <= High(FMaskTabStr)) do
      if PathMatchSpec( fn, PChar( FMaskTabStr[i] ) )
         then Result := true
         else Inc(i);
end;

function TuFolderToolParam.IsMatchingQ1( const fn: PChar) : boolean;
begin
   Result := PathMatchSpec( fn, PChar( FFileNameMask ) );
end;

function TuFolderToolParam.IsMatchingQ2( const fn: PChar) : boolean;
begin
   Result := PathMatchSpecEx( fn, PChar( FFileNameMask ), PMSF_MULTIPLE ) = S_OK;
end;

end.
