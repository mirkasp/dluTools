unit dluFolderTool;

{$I dluOptions.inc}

interface

uses Windows
   , dluFolderToolParam
   , Generics.Collections
   ;

{$IFDEF FPC}
function SelectFolderDialog( var ARetFolder: AnsiString; AStartFolder: AnsiString = '' ): boolean;
{$ELSE}
function SelectFolderDialog( var ARetFolder: String; AStartFolder: String = '' ): boolean;
{$ENDIF}

type IuStrategy = interface
   procedure FileProc( const xFolder: string; const xFindData: TWin32FindData );
   procedure Finish();
end;

type TuMatchedFileMethod = procedure( const xFolder: String; const FindData: TWin32FindData ) of object;
type TuEachFileMethod    = procedure( const AIsMatching: boolean; const xFolder: string; const FindData: TWin32FindData; var ABreak: boolean ) of object;
type TuEachFolderMthd    = procedure( const AFolder: String; const FindData: TWin32FindData ) of object;

{$IFDEF dlu_Anonymous}
type TuMatchedFileProc = reference to procedure( const xFolder: string; const FindData: TWin32FindData );
type TuEachFileProc    = reference to procedure( const AIsMatching: boolean; const xFolder: string; const FindData: TWin32FindData; var ABreak: boolean );
{$ENDIF}

type TuScanParams = record
   Path           : String;
   EachFileMethod : TuEachFileMethod;
   EachFolderMthd : TuEachFolderMthd;
   Recursive      : boolean;
end;

{ TuFolderTool }
type TuFolderTool = class
   strict private
      FLargeFetch   : boolean;
      FInfoBasic    : boolean;
      FGetFirstFunc : function (const fn: PChar; const fFindData: PWin32FindData): THandle of object;
      FSearchParam  : TuFolderToolParam;
      FFolderStack  : specialize TStack<string>;
      //
      procedure Initialize();
      //
      procedure SetInfoBasic(const Value: boolean);
      procedure SetLargeFetch(const Value: boolean);
      //
      function GetFirstFileW7(const fn: PChar; const fFindData: PWin32FindData ): THandle;
      function GetFirstFileXP(const fn: PChar; const fFindData: PWin32FindData ): THandle;
      //
      function ValidFolderName(const xFolder: string): boolean; overload; register; inline;
   public
      constructor Create( const xMask: string = '' ); overload;
      constructor Create( const ASearchParam: TuFolderToolParam ); overload;
      destructor Destroy; override;
      //
      // simple file access
      function GetFirstFile( const xPath: string; var xHandle: THandle; const fFindData: PWin32FindData ): boolean;
      function GetNextFile( const xHandle: THandle; const fFindData: PWin32FindData ): boolean;
      //
      // process action for folders in selected path only
      procedure ProcessForFolders( const APath: string; AStrategy: IuStrategy ); overload;
      procedure ProcessForFolders( const APath: string; FileAction: TuMatchedFileMethod ); overload;
      {$IFDEF dlu_Anonymous}
      procedure ProcessForFolders( const APath: string; FileAction: TuMatchedFileProc ); overload;
      {$ENDIF}
      //
      // process action for files in selected path and subfolders [optional]
      procedure ProcessForFiles( const APath: string; AStrategy: IuStrategy; const ARecursive: boolean = false); overload;
      procedure ProcessForFiles( const APath: string; AMatchedFileMethod: TuMatchedFileMethod; const ARecursive: boolean = false ); overload;
      procedure ProcessForFiles( const APath: string; AEachFileMethod: TuEachFileMethod; const ARecursive: boolean = false ); overload;
      procedure ProcessForFiles( Const AParams: TuScanParams ); overload;

      {$IFDEF dlu_Anonymous}
      procedure ProcessForFiles( const APath: string; AMatchedFileAction: TuMatchedFileProc; const ARecursive: boolean = false ); overload;
      procedure ProcessForFiles( const APath: string; AEachFileAction: TuEachFileProc; const ARecursive: boolean = false ); overload;
      {$ENDIF}
      //
      property LargeFetch : boolean read FLargeFetch write SetLargeFetch;
      property InfoBasic  : boolean read FInfoBasic  write SetInfoBasic;
      //
end;

implementation

uses Classes
   , SysUtils
   , Dialogs
   {$IFNDEF FPC}
   {$WARN UNIT_PLATFORM OFF}
   , FileCtrl
   {$WARN UNIT_PLATFORM ON}
   {$ENDIF}
   ;

const FT_ALL_FILES = '*';

{ TuFolderTool }

constructor TuFolderTool.Create(const xMask: string);
begin
   inherited Create;

   FSearchParam := TuFolderToolParam.Create( xMask );
   FSearchParam.FreeOnRelease := true;

   Initialize();

end;

constructor TuFolderTool.Create(const ASearchParam: TuFolderToolParam);
begin
   inherited Create;

   FSearchParam := ASearchParam;
   Initialize();
end;

procedure TuFolderTool.Initialize;
begin
   FLargeFetch := true;
   FInfoBasic  := true;

   if FSearchParam.IsWin7OrAbove
      then FGetFirstFunc := {$IFDEF FPC}@{$ENDIF}GetFirstFileW7
      else FGetFirstFunc := {$IFDEF FPC}@{$ENDIF}GetFirstFileXP;

   FFolderStack := specialize TStack<String>.Create;

end;

destructor TuFolderTool.Destroy;
begin
   if Assigned( FSearchParam ) and FSearchParam.FreeOnRelease
      then FSearchParam.Free;
   if Assigned( FFolderStack )
      then FFolderStack.Free;

   inherited;
end;

function TuFolderTool.GetFirstFile(const xPath: string; var xHandle: THandle; const fFindData: PWin32FindData): boolean;
  var saf : string;
begin
   saf := xpath + FT_ALL_FILES;

   xHandle := FGetFirstFunc( PChar(saf), fFindData );
   Result  := xHandle <> INVALID_HANDLE_VALUE;
end;

function TuFolderTool.GetNextFile(const xHandle: THandle; const fFindData: PWin32FindData): boolean;
begin
   Result := FindNextFileW( xHandle, fFindData^ );
end;

{============================================================================================================}

procedure TuFolderTool.ProcessForFiles(const APath: string; AStrategy: IuStrategy; const ARecursive: boolean);
  var h   : THandle;
      fd  : TWin32FindData;
      s   : string;
begin
   FFolderStack.Clear;
   FFolderStack.Push( APath );
   h := Default( THandle );

   //while not FFolderStack.Count.IsEmpty  do begin
   while FFolderStack.Count > 0 do begin
       s := FFolderStack.Pop;
       if GetFirstFile( s, h, @fd ) then begin
          repeat
             if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                if FSearchParam.Matches( fd )  then AStrategy.FileProc( s, fd );
             end else
                if ARecursive and {%H-}ValidFolderName( fd.cFileName )
                  then FFolderStack.Push( s + fd.cFileName + PathDelim );
          until not GetNextFile( h, @fd );
          Windows.FindClose(h);
       end;
   end;
   AStrategy.Finish;

end;

procedure TuFolderTool.ProcessForFiles( const APath: string; AMatchedFileMethod: TuMatchedFileMethod; const ARecursive: boolean );
  var h  : THandle;
      fd : TWin32FindData;
      s  : string;
begin
   FFolderStack.Clear;
   FFolderStack.Push( APath );
   h := Default( THandle );

   while FFolderStack.Count > 0  do begin
       s := FFolderStack.Pop;
       if GetFirstFile( s, h, @fd ) then begin
          repeat
             if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                if FSearchParam.Matches( fd )  then AMatchedFileMethod( s, fd );
             end else
                if ARecursive and {%H-}ValidFolderName( fd.cFileName )
                  then FFolderStack.Push( s + fd.cFileName + PathDelim );
          until not GetNextFile( h, @fd );
          Windows.FindClose(h);
       end;
   end;
end;

procedure TuFolderTool.ProcessForFiles( const APath: string; AEachFileMethod: TuEachFileMethod; const ARecursive: boolean);
  var h  : THandle;
      fd : TWin32FindData;
      s  : string;
      isBreak: boolean;
begin
   isBreak := false;
   FFolderStack.Clear;
   FFolderStack.Push( APath );
   h := Default( THandle );

   while (FFolderStack.Count > 0) and not isBreak do begin
       s := FFolderStack.Pop;
       if GetFirstFile( s, h, @fd ) then begin
          repeat
             if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                AEachFileMethod( FSearchParam.Matches( fd ), s, fd, isBreak );

             end else
                if ARecursive and {%H-}ValidFolderName( fd.cFileName )
                  then FFolderStack.Push( s + fd.cFileName + PathDelim );
          until not GetNextFile( h, @fd ) or isBreak;
          Windows.FindClose(h);
       end;
   end;
end;

procedure TuFolderTool.ProcessForFiles(const AParams: TuScanParams);
  var h  : THandle;
      fd : TWin32FindData;
      s  : String;
      isBreak: boolean;
begin
   isBreak := false;
   FFolderStack.Clear;
   FFolderStack.Push( AParams.Path );
   h := Default( THandle );

   while (FFolderStack.Count > 0 ) and not isBreak do begin
       s := FFolderStack.Pop;
       if GetFirstFile( s, h, @fd ) then begin
          repeat

             if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                AParams.EachFileMethod( FSearchParam.Matches( fd ), s, fd, isBreak );

             end else
                if AParams.Recursive and {%H-}ValidFolderName( fd.cFileName ) then begin
                   FFolderStack.Push( s + fd.cFileName + PathDelim );
                   if Assigned( AParams.EachFolderMthd )
                      then AParams.EachFolderMthd( s, fd ) ;
                end;

          until not GetNextFile( h, @fd ) or isBreak;
          Windows.FindClose(h);
       end;
   end;

end;


{$IFDEF dlu_Anonymous}
procedure TuFolderTool.ProcessForFiles( const APath: string; AMatchedFileAction: TuMatchedFileProc; const ARecursive: boolean );
  var h  : THandle;
      fd : TWin32FindData;
      s  : string;
begin
   fFolderStack.Clear;
   fFolderStack.Push( APath );

   while not fFolderStack.IsEmpty  do begin
       s := fFolderStack.Pop;
       if GetFirstFile( s, h, @fd ) then begin
          repeat
             if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                if fSearchParam.Matches( fd )  then AMatchedFileAction( s, fd );
             end else
                if ARecursive and ValidFolderName( fd.cFileName )
                  then fFolderStack.Push( s + fd.cFileName + PathDelim );
          until not GetNextFile( h, @fd );
          Windows.FindClose(h);
       end;
   end;
end;

procedure TuFolderTool.ProcessForFiles( const APath: string; AEachFileAction: TuEachFileProc; const ARecursive: boolean);
  var h  : THandle;
      fd : TWin32FindData;
      s  : string;
      isBreak: boolean;
begin
   isBreak := false;
   fFolderStack.Clear;
   fFolderStack.Push( APath );

   while not (fFolderStack.IsEmpty or isBreak) do begin
       s := fFolderStack.Pop;
       if GetFirstFile( s, h, @fd ) then begin
          repeat
             if (fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                AEachFileAction( fSearchParam.MatchingWith( fd ), s, fd, isBreak );
             end else
                if ARecursive and ValidFolderName( fd.cFileName )
                  then fFolderStack.Push( s + fd.cFileName + PathDelim );
          until not GetNextFile( h, @fd ) or isBreak;
          Windows.FindClose(h);
       end;
   end;
end;
{$ENDIF}

procedure TuFolderTool.ProcessForFolders(const APath: string; AStrategy: IuStrategy);
  var h  : THandle;
      fd : TWin32FindData;
begin
   h := Default( THandle );
   if GetFirstFile( APath, h, @fd ) then begin
      repeat
         if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and {%H-}ValidFolderName( fd.cFileName) then begin
            if FSearchParam.Matches( fd ) then AStrategy.FileProc( APath, fd );
         end;
      until not GetNextFile( h, @fd );
      Windows.FindClose(h);
   end;
   AStrategy.Finish;
end;

procedure TuFolderTool.ProcessForFolders(const APath: string;  FileAction: TuMatchedFileMethod);
  var h  : THandle;
      fd : TWin32FindData;
begin
   h := Default( THandle );
   if GetFirstFile( APath, h, @fd ) then begin
      repeat
         if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and {%H-}ValidFolderName( fd.cFileName) then begin
            if FSearchParam.Matches( fd )  then FileAction( APath, fd );
         end;
      until not GetNextFile( h, @fd );
      Windows.FindClose(h);
   end;
end;

{$IFDEF dlu_Anonymous}
procedure TuFolderTool.ProcessForFolders(const APath: string; FileAction: TuMatchedFileProc);
  var h  : THandle;
      fd : TWin32FindData;
begin
   if GetFirstFile( APath, h, @fd ) then begin
      repeat
         if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and ValidFolderName( fd.cFileName) then begin
            if fSearchParam.Matches( fd )  then FileAction( APath, fd );
         end;
      until not GetNextFile( h, @fd );
      Windows.FindClose(h);
   end;
end;
{$ENDIF}

//
// private procedures and functions
//

function TuFolderTool.GetFirstFileW7(const fn: PChar; const fFindData: PWin32FindData): THandle;
  const FIND_FIRST_EX_LARGE_FETCH = 2;
  const FindExInfoBasic           = 1;
  const ExInfoFlags  : array[boolean] of integer  = ( 0, FindExInfoBasic );
  const ExLargeFlags : array[boolean] of Cardinal = ( 0, FIND_FIRST_EX_LARGE_FETCH );
begin
   Result := FindFirstFileExW( fn,
                              _FINDEX_INFO_LEVELS( ExInfoFlags[ FInfoBasic ] ),
                              fFindData,
                              FindExSearchNameMatch,
                              nil,
                              ExLargeFlags[ FLargeFetch ]
                            );
end;

function TuFolderTool.GetFirstFileXP(const fn: PChar; const fFindData: PWin32FindData): THandle;
begin
   Result := FindFirstFileW( fn, fFindData^ );
end;

procedure TuFolderTool.SetInfoBasic(const Value: boolean);
begin
   FInfoBasic := Value;
end;

procedure TuFolderTool.SetLargeFetch(const Value: boolean);
begin
   FLargeFetch := Value;
end;

function TuFolderTool.ValidFolderName(const xFolder: string): boolean; register;
  var p : PChar;
begin
   p := PChar( xFolder );
   Result := (p[0] <> #0) and
             ((p[0] <> '.') or ( (p[1] <> #0) and ((p[1] <> '.') or (p[2] <> #0) ) ) );
end;

const TitleBtn = 'Wybierz';
const TitleStr = TitleBtn + ' folder';

{$IFDEF FPC}
function SelectFolderDialog( var ARetFolder: AnsiString; AStartFolder: AnsiString = '' ): boolean;
begin
   Result := SelectDirectory( TitleStr, AStartFolder, ARetFolder );
end;
{$ELSE}
{$WARN SYMBOL_PLATFORM OFF}
function SelectFolderDialog( var ARetFolder: String; AStartFolder: String = '' ): boolean;
begin
   if AStartFolder = '' then AStartFolder := GetCurrentDir();

   if Win32MajorVersion >= 6 then
      with TFileOpenDialog.Create(nil) do
         try
            Title   := TitleStr;
            Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem]; // YMMV
            OkButtonLabel := TitleBtn;
            DefaultFolder := AStartFolder;
            FileName      := AStartFolder;
            Result        := Execute();
            if Result then ARetFolder := FileName;
         finally
            Free;
         end
   else begin
      Result := SelectDirectory( TitleStr, AStartFolder, ARetFolder, [sdNewUI, sdNewFolder] )
   end;
   if Result then ARetFolder := IncludeTrailingPathDelimiter( ARetFolder );
end;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}

end.
