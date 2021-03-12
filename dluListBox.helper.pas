unit dluListBox.helper;

{$I dluOptions.inc}

interface

uses StdCtrls;

type

{ TListBoxHelper }

 TListBoxHelper = class helper for TListBox
   strict private
      function GetSelectedItemIndex: integer;
      function GetSelectedItem: string;
      function GetSelectedItemObj: TObject;
      procedure SetSelectedItems(const Value: string);
   public
      procedure IncItemIndex;
      procedure DecItemIndex;
      property SelectedItemIndex: integer read GetSelectedItemIndex;
      property SelectedItem     : string  read GetSelectedItem write SetSelectedItems;
      property SelectedItemObj  : TObject read GetSelectedItemObj;
end;



implementation

{ TListBoxHelper }

function TListBoxHelper.GetSelectedItemIndex: integer;
begin
   Result := self.ItemIndex;
end;

function TListBoxHelper.GetSelectedItem: string;
begin
   Result := self.Items[ GetSelectedItemIndex() ];
end;

function TListBoxHelper.GetSelectedItemObj: TObject;
begin
   Result := self.Items.Objects[ GetSelectedItemIndex() ];
end;

procedure TListBoxHelper.SetSelectedItems(const Value: string);
begin
   self.Items[ GetSelectedItemIndex() ] := Value;
end;

procedure TListBoxHelper.IncItemIndex;
  var n : integer;
begin
   n := GetSelectedItemIndex();
   if n < self.Count - 1 then self.ItemIndex := n + 1;
end;

procedure TListBoxHelper.DecItemIndex;
  var n : integer;
begin
   n := GetSelectedItemIndex();
   if n > 0 then self.ItemIndex := n - 1;
end;

end.
