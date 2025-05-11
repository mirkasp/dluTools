unit siNodeIntf;

{$IFDEF FPC}
  {$mode OBJFPC}{$H+}
  {$modeswitch UNICODESTRINGS+}
  {$modeswitch ADVANCEDRECORDS+}
{$ELSE}
  {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Controls
   , Graphics
   , VirtualTrees
   ;

type TsiNodeType = ( sinParam, sinSubParam, sinUrl );

type IsiParam = interface
   procedure SetFontTo( const nCol: integer; xFont: TFont );
   function GetDeltaHeight() : integer;
   function GetNormalText( const nCol: integer ): String;
   function GetMouseCursor( const nCol: integer ): TCursor;
   function GetNodeType(): TsiNodeType;
   function GetExtraParam( const n: integer ): string;
end;

const NODE_URL  = 0;
const NODE_HINT = 2;

{ TsiBaseObj }
type TsiBaseObj = class abstract ( TInterfacedObject, IsiParam )
   protected
     const MAX_COL = 2;
     type TNodeParams = record
             NormalText    : string;
             FontSizeDelta : Integer;
             FontColor     : TColor;
             FontStyles    : TFontStyles;
             Cursor        : TCursor;
             Hint          : string;
          end;
     var fNodeParams : array[ 0.. MAX_COL-1 ] of TNodeParams;
         fNodeType   : TsiNodeType;
      class function TryAddColon( const AText: string ): string;
   public
     constructor Create;
     //
     procedure SetFontTo( const nCol: integer; xFont: TFont );
     function GetDeltaHeight(): integer;
     function GetNormalText( const nCol: integer ): String; virtual;
     function GetMouseCursor( const nCol: integer ): TCursor; virtual;
     function GetNodeType(): TsiNodeType;
     function GetExtraParam( const n: integer ): string; virtual;
     //
end;

type
  PNodeData = ^TNodeData;
  TNodeData = record
      NodeObjIntf : IsiParam;
      procedure ReleaseData();
  end;

{ VstHelper }
type VstHelper = class helper for TBaseVirtualTree {TVirtualStringTree}
    strict private
       procedure PrepareNode( ANode: PVirtualNode; AObject: IsiParam );
    public
       function GetNodeObject( ANode: PVirtualNode ): IsiParam;
       function AddNodeFirst( AParentNode: PVirtualNode; AObject: IsiParam ): PVirtualNode;
       function AddNodeLast( AParentNode: PVirtualNode; AObject: IsiParam ): PVirtualNode;
       function IsHyperLink( const xHitInfo: THitInfo ): boolean; inline;

end;

implementation

class function TsiBaseObj.TryAddColon(const AText: string): string;
  const cColon = ':';
begin
 if (AText <> '') and (AText[Length(AText)]<>cColon)
    then Result := AText + cColon
    else Result := AText;
end;

constructor TsiBaseObj.Create;
  var i: integer;
begin
   inherited Create;
   for i:= 0 to MAX_COL - 1 do fNodeParams[i] := Default( TNodeParams );
end;

function TsiBaseObj.GetNodeType(): TsiNodeType;
begin
   Result := fNodeType;
end;

function TsiBaseObj.GetExtraParam( const n: integer ) : string;
begin
    case n of
       NODE_HINT: Result := fNodeParams[ 1 ].Hint;
       else       Result := '';
    end;
end;

function TsiBaseObj.GetDeltaHeight(): integer;
  var i: integer;
begin
   Result := -MaxInt;
   for i := 0 to MAX_COL-1 do
      with fNodeParams[ i ] do
         if FontSizeDelta > Result then Result := FontSizeDelta;
   Result := Result + 2;
end;

function TsiBaseObj.GetNormalText(const nCol: integer): String;
begin
   Result := fNodeParams[ nCol ].NormalText;
end;

function TsiBaseObj.GetMouseCursor( const nCol: integer) : TCursor;
begin
   Result := fNodeParams[ nCol ].Cursor;
end;

procedure TsiBaseObj.SetFontTo( const nCol: integer; xFont: TFont);
begin
   if (nCol >= 0) and (nCol<MAX_COL) then begin
      with fNodeParams[ nCol ] do begin
         xFont.Style := xFont.Style + FontStyles;
         if FontColor <> clDefault then xfont.Color := FontColor;
         xFont.Size  := xFont.Size + FontSizeDelta;
      end;
   end;
end;

procedure TNodeData.ReleaseData;
begin
   NodeObjIntf := nil;
end;

{ VstHelper }

procedure VstHelper.PrepareNode( ANode: PVirtualNode; AObject: IsiParam) ;
begin
   ValidateNode( ANode, False );
   with PNodeData( GetNodeData( ANode ))^ do begin
      NodeObjIntf:= AObject;
   end;
   NodeHeight[ ANode ] := Int64(NodeHeight[ ANode ]) + AObject.GetDeltaHeight();
end;

function VstHelper.GetNodeObject( ANode: PVirtualNode) : IsiParam;
begin
   Result := PNodeData( GetNodeData( ANode ) )^.NodeObjIntf;
end;

function VstHelper.AddNodeFirst( AParentNode: PVirtualNode; AObject: IsiParam ) : PVirtualNode;
begin
   Result := InsertNode( AParentNode, amAddChildFirst );
   PrepareNode( Result, AObject );
end;

function VstHelper.AddNodeLast( AParentNode: PVirtualNode; AObject: IsiParam ) : PVirtualNode;
begin
   Result := AddChild( AParentNode );
   PrepareNode( Result, AObject );
end;

function VstHelper.IsHyperLink(const xHitInfo: THitInfo): boolean;
begin
   with xHitInfo do
      Result := ((hiOnItemLabel in HitPositions) or (hiOnNormalIcon in HitPositions)) and
                 (self.GetNodeObject(HitNode).GetNodeType = sinUrl);
end;



end.

