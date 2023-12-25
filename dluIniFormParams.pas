unit dluIniFormParams;

{$mode ObjFPC}{$H+}

interface

uses Classes
   , Forms
   , IniFiles
   ;

type TFormParamConfig = class
   strict private
     const INIT_SIZE = 4;
     const CLI_FORMS_DEFAULT_SECTION  = 'FORMS';
       var fForms  : TStringList;
           CliFormsSection : string;
   public
     constructor Create( const AFormsSection: string = '' );
     destructor Destroy; override;
     //
     procedure SaveFormParam( AForm: TForm; const AId: string );
     procedure RestoreFormParam( AForm: TForm; const AId: string );
     //
     procedure SaveToIni( AIni: TIniFile );
     procedure RestoreFromIni( AIni: TIniFile );
end;


implementation

uses SysUtils
   ;

{ TFormParamConfig }

constructor TFormParamConfig.Create( const AFormsSection: string = '' );
begin
   inherited Create;
   fForms := TStringList.Create;
   if AFormsSection <> ''
      then CliFormsSection := AFormsSection
      else CliFormsSection := CLI_FORMS_DEFAULT_SECTION;

end;

destructor TFormParamConfig.Destroy;
begin
   fForms.Free;
   inherited Destroy;
end;

procedure TFormParamConfig.SaveFormParam( AForm: TForm; const AId: string);
  var s : string;
      n : integer;
begin

   s := IntToHex( AForm.Left,   INIT_SIZE ) +
        IntToHex( AForm.Top,    INIT_SIZE ) +
        IntToHex( AForm.Width,  INIT_SIZE ) +
        IntToHex( AForm.Height, INIT_SIZE );

   n := fForms.IndexOfName( Aid );
   if n < 0
      then fForms.AddPair( AId, s )
      else fForms.ValueFromIndex[ n ] := s;

end;

procedure TFormParamConfig.RestoreFormParam( AForm: TForm; const AId: string);
  var _l, _t, _w, _h : integer;
      s : string = '';
begin
   if fForms.IndexOfName( AId ) < 0
      then s := '*'
      else s := fForms.Values[ AId ];

   if not TryStrToInt( '$' + Copy( s, 1 + 0 * INIT_SIZE, INIT_SIZE ), _l ) or
      not TryStrToInt( '$' + Copy( s, 1 + 1 * INIT_SIZE, INIT_SIZE ), _t ) or
      not TryStrToInt( '$' + Copy( s, 1 + 2 * INIT_SIZE, INIT_SIZE ), _w ) or
      not TryStrToInt( '$' + Copy( s, 1 + 3 * INIT_SIZE, INIT_SIZE ), _h ) then begin

     _l := AForm.Left;
     _t := AForm.Top;
     _w := AForm.Width;
     _h := AForm.Height;

   end;
   AForm.SetBounds( _l, _t, _w, _h );

end;

procedure TFormParamConfig.SaveToIni( AIni: TIniFile );
  var i : integer;
begin
   for i:= 0 to fForms.Count-1 do
       AIni.WriteString( CliFormsSection, fForms.Names[i], fForms.ValueFromIndex[i] );
end;

procedure TFormParamConfig.RestoreFromIni(AIni: TIniFile);
begin
   AIni.ReadSectionRaw( CliFormsSection, fForms );
end;

end.

