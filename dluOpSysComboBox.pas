unit dluOpSysComboBox;

{$mode ObjFPC}{$H+}

interface

uses EditBtn
   , dluMultiColsComboBox;


{ TOpSysComboBox }
type TOpSysComboBox = class( TMultiColsComboBox )
   public
     constructor Create( AEditButton: TEditButton );
end;

implementation

uses dluWinVer3;

{ TOpSysComboBox }

constructor TOpSysComboBox.Create(AEditButton: TEditButton);
  var dx : TDynStringArray = nil;
      i  : integer;
      wv : TWinVerSpec;
begin
   wv := dluWinVer3.GetAppWinVer();
   AEditButton.Text  := String( wv.Name) + ' [' + String( wv.CompilationInfo) + ']';

   SetLength( dx, wv.AllProperties.Count * 2 );
   with wv.AllProperties do
      for i := 0 to Count-1 do begin
         dx[ 2*i    ] := Names[ i ];
         dx[ 2*i +1 ] := ValueFromIndex[ i ];
      end;

  inherited Create( AEditButton, [ 0.3 ], dx );

end;

end.

