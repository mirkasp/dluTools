unit dluOpSysComboBox;

{$mode ObjFPC}{$H+}

interface

uses EditBtn
   , dluMultiColsComboBox;

type

{ TOpSysComboBox }

 TOpSysComboBox = class( TMultiColsComboBox )
   public
     constructor Create( AEditButton: TEditButton );
end;

implementation

uses dluWinVer3;

{ TOpSysComboBox }

constructor TOpSysComboBox.Create(AEditButton: TEditButton);
  var dx : TDynStringArray = nil;
      i  : integer;
begin
   AEditButton.Text  := String(dluWinVer3.AppWinVer.Name) + ' [' + String(dluWinVer3.AppWinVer.CompilationInfo) + ']';

   SetLength( dx, dluWinVer3.AppWinVer.AllProperties.Count * 2 );
   with dluWinVer3.AppWinVer.AllProperties do
      for i := 0 to Count-1 do begin
         dx[ 2*i    ] := Names[ i ];
         dx[ 2*i +1 ] := ValueFromIndex[ i ];
      end;

  inherited Create( AEditButton, [ 0.3 ], dx );

end;

end.

