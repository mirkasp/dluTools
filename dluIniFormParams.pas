unit dluIniFormParams;

{$mode ObjFPC}{$H+}

interface

uses Classes, Forms, IniFiles;

{ TFormParamConfig }
type TFormParamConfig = class
  strict private
    const INIT_SIZE = 8;  // 8 znaków hex dla Integer (32-bit)
    const LEGACY_SIZE = 4; // Stary format (16-bit) - dla kompatybilności wstecznej
    const CLI_FORMS_DEFAULT_SECTION = 'FORMS';

    // Indeksy pól w zapisanym stringu
    const IDX_LEFT   = 0;
    const IDX_TOP    = 1;
    const IDX_WIDTH  = 2;
    const IDX_HEIGHT = 3;
    const IDX_STATE  = 4;
    const PARAM_COUNT = 5;

  strict private
    FForms: TStringList;
    CliFormsSection: AnsiString;

    function TryParseFormParams(const AData: AnsiString; out ALeft, ATop, AWidth, AHeight, AState: Integer): Boolean;
    function IsValidWindowState(AState: Integer): Boolean; inline;
    function IsValidDimension(AValue: Integer): Boolean; inline;

  public
    constructor Create(const AFormsSection: AnsiString = '' );
    destructor Destroy; override;

    procedure SaveFormParam(AForm: TForm; const AId: AnsiString);
    procedure RestoreFormParam(AForm: TForm; const AId: AnsiString);

    procedure SaveToIni(AIni: TIniFile);
    procedure RestoreFromIni(AIni: TIniFile);
  end;

implementation

uses SysUtils;

{ TFormParamConfig }

constructor TFormParamConfig.Create(const AFormsSection: AnsiString = '');
begin
   inherited Create;
   FForms := TStringList.Create;
   FForms.CaseSensitive := False;

   if AFormsSection <> '' then
      CliFormsSection := AFormsSection
   else
      CliFormsSection := CLI_FORMS_DEFAULT_SECTION;
end;

destructor TFormParamConfig.Destroy;
begin
   FForms.Free;
   inherited Destroy;
end;

function TFormParamConfig.IsValidWindowState(AState: Integer): Boolean; inline;
begin
   Result := (AState >= Ord(Low(TWindowState))) and (AState <= Ord(High(TWindowState)));
end;

function TFormParamConfig.IsValidDimension(AValue: Integer): Boolean; inline;
begin
   // Wymiary powinny być dodatnie i rozsądne (nie większe niż 32767)
   Result := (AValue > 0) and (AValue < 32767);
end;

function TFormParamConfig.TryParseFormParams(const AData: AnsiString; out ALeft, ATop, AWidth, AHeight, AState: Integer): Boolean;
  var fieldSize: Integer;
      expectedLength: Integer;

  function GetHexValue(AIndex: Integer; out AValue: Integer): Boolean;
    var hexStr: AnsiString;
  begin
     hexStr := Copy(AData, 1 + AIndex * fieldSize, fieldSize);
     Result := TryStrToInt('$' + hexStr, AValue);
  end;

begin
   Result := False;

   // Wykryj format: nowy (8 znaków) lub stary (4 znaki)
   expectedLength := PARAM_COUNT * INIT_SIZE;
   fieldSize      := INIT_SIZE;
   if Length(AData) <> expectedLength then begin
      expectedLength := PARAM_COUNT * LEGACY_SIZE;
      if Length(AData) = expectedLength then
         fieldSize := LEGACY_SIZE
      else
         Exit; // Nieprawidłowa długość
   end;

   // Parsuj wszystkie pola
   if not (GetHexValue(IDX_LEFT,   ALeft  ) and
           GetHexValue(IDX_TOP,    ATop   ) and
           GetHexValue(IDX_WIDTH,  AWidth ) and
           GetHexValue(IDX_HEIGHT, AHeight) and
           GetHexValue(IDX_STATE,  AState )) then
      Exit;

   // Walidacja
   if not (IsValidDimension(AWidth) and IsValidDimension(AHeight)) then
      Exit;

   if not IsValidWindowState(AState) then
      Exit;

   Result := True;
end;

procedure TFormParamConfig.SaveFormParam(AForm: TForm; const AId: AnsiString);
  var s  : AnsiString;
      idx: Integer;
begin
   if not Assigned(AForm) then
      Exit;

   if Trim(AId) = '' then
      Exit;

   // Koduj parametry formularza
   s := IntToHex(AForm.Left,   INIT_SIZE) +
        IntToHex(AForm.Top,    INIT_SIZE) +
        IntToHex(AForm.Width,  INIT_SIZE) +
        IntToHex(AForm.Height, INIT_SIZE) +
        IntToHex(Ord(AForm.WindowState), INIT_SIZE);

   // Dodaj lub zaktualizuj wpis
   idx := FForms.IndexOfName(AId);
   if idx < 0
      then FForms.AddPair(AId, s)
      else FForms.ValueFromIndex[idx] := s;
end;

procedure TFormParamConfig.RestoreFormParam(AForm: TForm; const AId: AnsiString);
  var _l, _t, _w, _h, _state: Integer;
      s: AnsiString;
begin
   if not Assigned(AForm) then
      Exit;

   // Sprawdź czy istnieje wpis dla tego formularza
   if FForms.IndexOfName(AId) < 0 then
      Exit; // Brak zapisanych parametrów - zostaw domyślne

   s := FForms.Values[AId];

   // Spróbuj sparsować parametry (obsługuje stary i nowy format)
   if TryParseFormParams(s, _l, _t, _w, _h, _state) then begin
      AForm.SetBounds(_l, _t, _w, _h);
      AForm.WindowState := TWindowState(_state);
   end;
   // Jeśli parsowanie się nie powiodło, pozostaw domyślne wartości
end;

procedure TFormParamConfig.SaveToIni(AIni: TIniFile);
var
  i: Integer;
begin
  if not Assigned(AIni) then
    Exit;

  for i := 0 to FForms.Count - 1 do
    AIni.WriteString(CliFormsSection, FForms.Names[i], FForms.ValueFromIndex[i]);
end;

procedure TFormParamConfig.RestoreFromIni(AIni: TIniFile);
begin
   if not Assigned(AIni) then
      Exit;

   FForms.Clear;
   AIni.ReadSectionRaw(CliFormsSection, FForms);
end;

end.
