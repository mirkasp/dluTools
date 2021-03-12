unit dluComboBox.helper;

{$I dluOptions.inc}

interface

uses StdCtrls;

type TComboBoxHelper = class helper for TComboBox
   strict private
      function GetSelectedItemIndex: integer;
      function GetSelectedItem: string;
      function GetSelectedItemObj: TObject;
   public
      property SelectedItemIndex: integer read GetSelectedItemIndex;
      property SelectedItem     : string  read GetSelectedItem;
      property SelectedItemObj  : TObject read GetSelectedItemObj;
end;



implementation

{ TComboBoxHelper }

function TComboBoxHelper.GetSelectedItemIndex: integer;
begin
   Result := self.ItemIndex;
end;

function TComboBoxHelper.GetSelectedItem: string;
begin
   Result := self.Items[ GetSelectedItemIndex() ];
end;

function TComboBoxHelper.GetSelectedItemObj: TObject;
begin
   Result := self.Items.Objects[ GetSelectedItemIndex() ];
end;

end.
