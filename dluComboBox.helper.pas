unit dluComboBox.helper;

{$I dluOptions.inc}

interface

uses StdCtrls;

type LxString = {$IFDEF UNICODE}UnicodeString{$ELSE}AnsiString{$ENDIF};

{ TComboBoxHelper }

 TComboBoxHelper = class helper for TComboBox
   strict private
      function GetSelectedItemIndex: integer;
      function GetSelectedItem: LxString;
      function GetSelectedItemObj: TObject;
   public
      property SelectedItemIndex: integer  read GetSelectedItemIndex;
      property SelectedItem     : LxString read GetSelectedItem;
      property SelectedItemObj  : TObject  read GetSelectedItemObj;
end;



implementation

{ TComboBoxHelper }

function TComboBoxHelper.GetSelectedItemIndex: integer;
begin
   Result := self.ItemIndex;
end;

function TComboBoxHelper.GetSelectedItem: LxString;
begin
   {$IFDEF UNICODE}
   Result := UnicodeString( self.Items[ GetSelectedItemIndex() ] );
   {$ELSE}
   Result := AnsiString( self.Items[ GetSelectedItemIndex() ] );
   {$ENDIF}
end;

function TComboBoxHelper.GetSelectedItemObj: TObject;
begin
   Result := self.Items.Objects[ GetSelectedItemIndex() ];
end;

end.
