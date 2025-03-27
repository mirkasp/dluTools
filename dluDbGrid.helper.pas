unit dluDbGrid.helper;

{$mode ObjFPC}{$H+}
{$modeswitch UNICODESTRINGS+}

interface

uses Controls
   , DbGrids
   ;

const MAXCOL = 50;
type TCols   = set of 0..MAXCOL-1;

{ TDBGridHelper }
type TDBGridHelper = class helper for TDBGrid
   strict private
      const CursorAtTitle: array[ boolean ] of TCursor = ( crDefault, crHandPoint );
      type TCrackDBGrid = class(TDBGrid)
          public
             property VisibleRowCount;
             property RowCount;
             property Row;
      end;

      function GetRow : integer;
      function GetVisbleRowCount : integer;
   public
      procedure AdjustCursor( const X, Y: Integer; const ACols: TCols );
      procedure RestoreRowTo( const NewRow: integer );
      //
      property VisibleRowCount: integer read GetVisbleRowCount;
      property Row            : integer read GetRow;
end;


(*************************************************************
based on:
https://stackoverflow.com/questions/25474102/dbgrid-stop-current-row-moving
*****************************************************************)

implementation

function TDBGridHelper.GetRow : integer;
  var crack: TCrackDBGrid absolute self;
begin
   Result := TCrackDBGrid(crack).Row;
end;

function TDBGridHelper.GetVisbleRowCount : integer;
   var crack: TCrackDBGrid absolute self;
begin
   Result := TCrackDBGrid(crack).VisibleRowCount;
end;

procedure TDBGridHelper.AdjustCursor( const X, Y : Integer; const ACols : TCols );
  //var gc : TGridCoord;
begin
   with MouseCoord( X, Y ) do
      self.Cursor := CursorAtTitle[ (y = 0) xor (x in ACols) ];
end;

procedure TDBGridHelper.RestoreRowTo( const NewRow: integer );
  var MoveDataSetBy,
      MovedBy   : Integer;
begin

  if (NewRow = self.Row) or (not NewRow in [1..self.VisibleRowCount])
      then exit;

   try
      if NewRow < self.Row then begin
         MoveDataSetBy := self.VisibleRowCount - NewRow;
         MovedBy       := self.DataSource.DataSet.MoveBy( MoveDataSetBy );
         if MoveDataSetBy = MovedBy
            then MoveDataSetBy := - MoveDataSetBy
            else MoveDataSetBy :=   MovedBy;
      end else begin
         MoveDataSetBy := 1 - NewRow;
         MovedBy       := self.DataSource.DataSet.MoveBy( MoveDataSetBy );
         if MoveDataSetBy = MovedBy
            then MoveDataSetBy := - MoveDataSetBy
            else MoveDataSetBy := - MovedBy;
      end;
      self.DataSource.DataSet.MoveBy( MoveDataSetBy );
   finally
      self.Invalidate;
   end;

end;

end.

