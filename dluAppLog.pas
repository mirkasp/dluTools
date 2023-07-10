unit dluAppLog;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses StdCtrls;

type TLogLineType = ( llSingle, llDouble );

type

{ TAppLog }

 TAppLog = class
   strict private
     fMemLog     : TMemo;
     fSepLineLen : integer;
     fActive     : boolean;
     procedure SetActive(const Value: boolean);
     procedure SetMemLog(const Value: TMemo);
   public
     constructor Create( AMemLog: TMemo = nil );
     //
     procedure Clear;
     procedure BeginUpdate;
     procedure EndUpdate;
     procedure SkipToEnd;
     procedure LogLine( const ALineType: TLogLineType );
     //
     procedure Log( const AText: string ); overload;
     procedure Log( const AText: string; const AParam: array of const ); overload;
     //
     procedure SimpleLog( const AText: string ); overload;
     procedure SimpleLog( const AText: string; const AParam: array of const ); overload;
     //
     procedure SaveToFile( const AFileName: string );
     //
     property MemLog    : TMemo   read fMemLog     write SetMemLog;
     property SepLineLen: integer read fSepLineLen write fSepLineLen;
     property Active    : boolean read fActive     write SetActive;
     //
end;

implementation

uses Windows, Messages, SysUtils;

const cSepLineLen = 120;
const cErrorMsg   = 'AppLog error - output not assigned !';
const cALogLineChar: array[ TLogLineType ] of char = ( '-', '=' );

{ TAppLog }

constructor TAppLog.Create(AMemLog: TMemo);
begin
   inherited Create;
   fSepLineLen := cSepLineLen;
   SetMemLog( AMemLog );
end;

procedure TAppLog.BeginUpdate;
begin
   if fActive then begin
      if not Assigned( fMemLog ) then raise Exception.Create( cErrorMsg );
      fMemLog.Lines.BeginUpdate;
   end;
end;

procedure TAppLog.Clear;
begin
   if fActive then begin
      if not Assigned( fMemLog ) then raise Exception.Create( cErrorMsg );
      fMemLog.Clear;
   end;
end;

procedure TAppLog.EndUpdate;
begin
   if fActive then begin
      if not Assigned( fMemLog ) then raise Exception.Create( cErrorMsg );
      fMemLog.Lines.EndUpdate;
   end;
end;

procedure TAppLog.SimpleLog(const AText: string);
begin
   if fActive then begin
      if not Assigned( fMemLog ) then raise Exception.Create( cErrorMsg );
      fMemLog.Lines.Add( AText );     //    <// Accessviolation
   end;
end;

procedure TAppLog.SimpleLog(const AText: string; const AParam: array of const);
begin
  SimpleLog( Format( AText, AParam ) );
end;

procedure TAppLog.Log(const AText: string);
begin
  SimpleLog( DateTimeToStr( Now() ) + ': ' + AText );
end;

procedure TAppLog.Log(const AText: string; const AParam: array of const);
begin
   Log( Format( AText, AParam ) );
end;

procedure TAppLog.LogLine(const ALineType: TLogLineType);
begin
   Log( StringOfChar( cALogLineChar[ ALineType ], fSepLineLen ) );
end;

procedure TAppLog.SaveToFile(const AFileName: string);
begin
   if not Assigned( fMemLog ) then raise Exception.Create( cErrorMsg );
   fMemLog.Lines.SaveToFile( AFileName );
end;

procedure TAppLog.SetActive(const Value: boolean);
begin
   if Value <> fActive then begin
      fActive := Value and Assigned( fMemLog );
   end;
end;

procedure TAppLog.SetMemLog(const Value: TMemo);
begin
   fMemLog := Value;
   SetActive( Assigned( fMemLog ) );
end;

procedure TAppLog.SkipToEnd;
begin
   if fActive then begin
      if not Assigned( fMemLog ) then raise Exception.Create( cErrorMsg );
      SendMessage( fMemLog.Handle, EM_LINESCROLL, 0, fMemLog.Lines.Count);
   end;
end;

end.
