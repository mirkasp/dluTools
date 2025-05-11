unit frmMxSysInfo;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   {$modeswitch UNICODESTRINGS+}

   {$DEFINE dlu_Generics}
   {$DEFINE dlu_Unicode}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Classes
   , Forms
   , Graphics
   , VirtualTrees
   ;

type TSysInfoLang = ( silEn, silPl );
type TSysInfoPosi = ( sipOS, sipProc, sipMem, sipUser, sipComp );
type TSysInfos = set of TSysInfoPosi;

{ TSysInfoFrame }
type TSysInfoFrame = class( TFrame)
    vst: TVirtualStringTree;
    procedure vstChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstDblClick( Sender: TObject) ;
    procedure VstFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode) ;
    procedure vstGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var {%H-}LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: AnsiString);
    procedure VstGetNodeDataSize( Sender: TBaseVirtualTree;
      var NodeDataSize: Integer) ;
    procedure VstGetText( Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: AnsiString) ;
    procedure vstMouseMove( Sender: TObject; {%H-}Shift: TShiftState; X,Y: Integer) ;
    procedure VstPaintText( Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType) ;
  strict private
    FLang : TSysInfoLang;
    //
    procedure PreInitVst();
    //
    procedure AddInfoOS();
    procedure AddInfoProcessor();
    procedure AddInfoMemory;
    procedure AddInfoUser();
    procedure AddInfoComp();
  //protected
  public
    //
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    //
    procedure InitVst();
    //
    procedure AddStandardInfo( const AInfos: TSysInfos = [] );
    procedure AddFileInfo( const AFileName: string; const AFileDesc: string = '' );
    function AddInfoEx(p: PVirtualNode; const AKey, AValue: UnicodeString; const AHint: UnicodeString = ''): PVirtualNode; overload;
    function AddInfoEx(p: PVirtualNode; const AKey, AValue: AnsiString; const AHint: AnsiString = ''): PVirtualNode; overload;
    //
    property Language: TSysInfoLang read FLang write FLang;
  end;

implementation

uses Controls
   , SysUtils
   , lclIntf
   , lxSystemInfo
   , lxTinyProcessor
   , lxTinySystemInfo
   , lxWinVer3
   , dluFileInfo
   , siNodeIntf
   , siNodeParam
   ;

{$R *.lfm}

const ALangStr: array[ TSysInfoPosi, TSysInfoLang ] of string = (
                        ( 'OS',        'System op.' ),
                        ( 'Processor', 'Procesor'   ),
                        ( 'Memory',    'RAM'        ),
                        ( 'User',      'Użytkownik' ),
                        ( 'Computer',  'Komputer'   )
                     );

{ TSysInfoFrame }

constructor TSysInfoFrame.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   if AOwner is TWinControl
      then self.Parent := AOwner as TWinControl
      else raise Exception.Create( 'Parent must be TWinControl type' );
   FLang := silEn;
end;

procedure TSysInfoFrame.CreateWnd;
begin
   inherited CreateWnd;
   InitVst();
end;

procedure TSysInfoFrame.PreInitVst();
begin
   with vst do begin
      Indent            := 10;
      DefaultNodeHeight := 15;
      CheckImageKind    := ckLightCheck;
      with TreeOptions do begin
          AutoOptions      := AutoOptions
                              - [ toAutoSpanColumns ]
                              - [ toAutoSort        ];
          MiscOptions      := MiscOptions
                              - [ toGridExtensions  ]
                              + [ toCheckSupport    ]
                              - [ toReadOnly        ];
          PaintOptions     := PaintOptions
                              - [ toShowHorzGridLines, toShowVertGridLines, toFullVertGridLines  ]
                              - [ toShowTreeLines, toShowRoot   ];
          SelectionOptions := SelectionOptions
                              + [ toFullRowSelect   ]
                              - [ toMultiSelect     ];
          StringOptions    := StringOptions
                              - [ toShowStaticText  ];
      end;
   end;
end;

procedure TSysInfoFrame.InitVst();
begin
   PreInitVst();
   with vst do begin
      Color             := Parent.Color;
      RootNodeCount     := 0;

      // ************************************************************************
      // From VirtualTrees.pas
      // ************************************************************************
      //
      // DefaultColumnOptions = [ coAllowClick,    coDraggable,
      //                          coEnabled,       coParentBidiMode,
      //                          coParentColor,   coResizable,
      //                          coShowDropmark,  coVisible,
      //                          coAllowFocus,    coEditable
      //                        ];
      //
      // TVTColumnOption = (
      //   coAllowClick,            // + Column can be clicked (must be enabled too).
      //   coDraggable,             // + Column can be dragged.
      //   coEnabled,               // + Column is enabled.
      //   coParentBidiMode,        // + Column uses the parent's bidi mode.
      //   coParentColor,           // + Column uses the parent's background color.
      //   coResizable,             // + Column can be resized.
      //   coShowDropMark,          // + Column shows the drop mark if it is
      //                            //   currently the drop target.
      //   coVisible,               // + Column is shown.
      //   coAutoSpring,            // - Column takes part in the auto spring
      //                            //   feature of the header (must be resizable too).
      //   coFixed,                 // - Column is fixed and can not be selected or scrolled etc.
      //   coSmartResize,           // - Column is resized to its largest entry
      //                            //   which is in view (instead of its largest
      //                            //   visible entry).
      //   coAllowFocus,            // + Column can be focused.
      //   coDisableAnimatedResize, // - Column resizing is not animated.
      //   coWrapCaption,           // - Caption could be wrapped across several
      //                            //   header lines to fit columns width.
      //   coUseCaptionAlignment,   // - Column's caption has its own aligment.
      //   coEditable               // + Column can be edited
      // );
      //
      // ***********************************************************************

      with Header do begin
         Columns.Clear;

         with Columns.Add do begin
            Text       := 'Parameter';
            MinWidth   := 50;
            Width      := 100;
            Alignment  := taRightJustify;
            Options    := DefaultColumnOptions + [ coSmartResize ];
         end;

         with Columns.Add do begin
            Text       := 'Value';
            MinWidth   := 150;
            Width      := 150;
            Options    := DefaultColumnOptions + [coAutoSpring];
         end;

         Options := Options + [ hoAutoResize ]
                            + [ hoAutoSpring ]
                            + [ hoShowHint   ]
                            - [ hoVisible    ];

         Columns[0].Width := 100;
      end;
      ReinitNode( nil, true );
      FullExpand( nil );
      Invalidate;
   end;
end;

procedure TSysInfoFrame.AddInfoMemory;
  const aLng: array[ TSysInfoLang ] of TuLocalization = ( ulEn, ulPl );
begin
   AddInfoEx( nil, ALangStr[ sipMem, FLang ], GetMemoryInfo( false, aLng[ FLang ] ) );
end;

(******************************************************************************)
procedure TSysInfoFrame.AddInfoOS();
  var node: PVirtualNode;
      s   : AnsiString;
begin
   with TWinVerSpec.Create do begin
      node := AddInfoEx( nil, ALangStr[ sipOS, FLang ], Name + ' [' + CompilationInfo + ']' );
      VST.CheckType[ Node ] := ctButton;
      for s in AllProperties do AddInfoEx( node, '', UnicodeString(s) );
      Free;
   end;
end;

(******************************************************************************)
procedure TSysInfoFrame.AddInfoProcessor;
  var node: PVirtualNode;
      rx  : TuTinyProcessorParam;
begin
   lxTinyProcessor.GetProcInfo( rx );
   node := AddInfoEx( nil, ALangStr[ sipProc, FLang ], rx.VersionStr );
   VST.CheckType[ node ] := ctButton;

   AddInfoEx( node, '', 'Cores: ' + UnicodeString(rx.Cores.ToString) );
   AddInfoEx( node, '', 'Socket: ' + rx.SocketStr   );
   AddInfoEx( node, '', 'Voltage: ' + rx.VoltageStr  );
   AddInfoEx( node, '', 'Max speed: ' + rx.MaxSpeedStr );
end;

(******************************************************************************)
procedure TSysInfoFrame.AddInfoUser();
  var uish : string;
begin
   uish := SysUtils.GetEnvironmentVariable('USERNAME') + '@' +
           SysUtils.GetEnvironmentVariable('COMPUTERNAME');

   AddInfoEx( nil, ALangStr[ sipUser, FLang ], uish );
end;

(******************************************************************************)
procedure TSysInfoFrame.AddInfoComp();
begin
   AddInfoEx( nil, ALangStr[ sipComp, FLang ], lxTinySystemInfo.GetProductInfo() );
end;

(******************************************************************************)
function TSysInfoFrame.AddInfoEx(p: PVirtualNode; const AKey, AValue: UnicodeString; const AHint: UnicodeString): PVirtualNode;
begin
   if not Assigned( p )
      then Result := VST.AddNodeLast( nil, TsiParamObj.Create( AKey, AValue, AHint ) )
      else Result := VST.AddNodeLast( p,   TsiSubParamObj.Create( AKey, AValue, AHint ) );
end;

function TSysInfoFrame.AddInfoEx(p: PVirtualNode; const AKey, AValue: AnsiString; const AHint: AnsiString): PVirtualNode;
begin
   Result := AddInfoEx( p, UTF8Decode(AKey), UTF8Decode(AValue), UTF8Decode(AHint) );
end;

procedure TSysInfoFrame.AddStandardInfo( const AInfos: TSysInfos );
  var infos: TSysInfos;
      si   : TSysInfoPosi;
begin
   infos := AInfos;
   if infos = []
      then for si in TSysInfoPosi do infos := infos + [si];

   for si in infos do
      case si of
        sipOS   : AddInfoOS();
        sipProc : AddInfoProcessor();
        sipMem  : AddInfoMemory();
        sipUser : AddInfoUser();
        sipComp : AddInfoComp();
        else      AddInfoEx( nil, 'UNKNOWN', 'UNKNOWN' );
      end;
end;

procedure TSysInfoFrame.AddFileInfo(const AFileName: string; const AFileDesc: string);
  const cNotFound = '???';
  var pdf, floc: UnicodeString;
begin
   pdf := dluFileInfo.LookForFile( AFileName, true, [ '.' ] );

   if pdf <> '' then begin
      floc := GetFileInfo( pdf ).FileVersion;
      if floc = '' then floc := cNotFound;
   end else floc := cNotFound;

   AddInfoEx( nil, AFileDesc,  floc, pdf );
end;

procedure TSysInfoFrame.VstPaintText( Sender: TBaseVirtualTree;
                                          const TargetCanvas: TCanvas;
                                          Node: PVirtualNode;
                                          Column: TColumnIndex;
                                          TextType: TVSTTextType) ;
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
     if Assigned( NodeObjIntf ) and(TextType = ttNormal) then begin
        NodeObjIntf.SetFontTo( Column, TargetCanvas.Font );
     end;
end;

procedure TSysInfoFrame.VstGetText( Sender: TBaseVirtualTree;
                                        Node: PVirtualNode;
                                        Column: TColumnIndex;
                                        TextType: TVSTTextType;
                                        var CellText: AnsiString) ;
begin
   with PNodeData(Sender.GetNodeData(Node))^ do
      if Assigned( NodeObjIntf ) and (TextType = ttNormal) then begin
         if (Column = 0) or (Column = 1 )
            then CellText := AnsiString( NodeObjIntf.GetNormalText( Column ) )
            else CellText := 'xyz';
      end;
end;

procedure TSysInfoFrame.vstMouseMove( Sender: TObject; Shift: TShiftState; X,Y: Integer) ;
  var HitInfo : THitInfo;
begin
   with Sender as TBaseVirtualTree do begin
      HitInfo := Default( THitInfo );
      GetHitTestInfoAt( X, Y, True, HitInfo);
      if Assigned( HitInfo.HitNode ) then begin
         if IsHyperLink( HitInfo ) then
             Cursor := GetNodeObject( HitInfo.HitNode ).GetMouseCursor( HitInfo.HitColumn )
         //else if GetNodeObject( HitInfo.HitNode ).GetMouseCursor( HitInfo.HitColumn ) then
         //    Cursor := crHelp
         //else
         //    Cursor := parent.Cursor;
         else
            Cursor := GetNodeObject( HitInfo.HitNode ).GetMouseCursor( HitInfo.HitColumn );
      end else
         Cursor := parent.Cursor;
   end;
end;

procedure TSysInfoFrame.VstFreeNode( Sender: TBaseVirtualTree; Node: PVirtualNode) ;
begin
   PNodeData(Sender.GetNodeData( Node ))^.ReleaseData;
end;

procedure TSysInfoFrame.vstGetHint( Sender: TBaseVirtualTree;
                                    Node: PVirtualNode; Column: TColumnIndex;
                                    var LineBreakStyle: TVTTooltipLineBreakStyle;
                                    var HintText: AnsiString);
begin
   with Sender as TBaseVirtualTree do begin
      if (Column=1) and Assigned( Node ) then
         HintText := AnsiString( GetNodeObject( Node ).GetExtraParam( NODE_HINT ) );
   end;
end;

procedure TSysInfoFrame.vstDblClick( Sender: TObject) ;
  var Node : PVirtualNode;
begin
   with Sender as TBaseVirtualTree do begin
      Node := GetNodeAt( ScreenToClient( Mouse.CursorPos ) );
      if Assigned( Node ) then
         with GetNodeObject( Node ) do
            if GetNodeType() = sinUrl then
               OpenUrl( UTF8Encode( GetExtraParam( NODE_URL ) ) );
   end;
end;

procedure TSysInfoFrame.vstChecked(Sender: TBaseVirtualTree; Node: PVirtualNode );
begin
   if Node^.CheckType = ctButton then
      with (Sender as TBaseVirtualTree) do begin
         Expanded[ Node ] := not Expanded[ Node ];
         Refresh;
      end;
end;

procedure TSysInfoFrame.VstGetNodeDataSize( Sender: TBaseVirtualTree; var NodeDataSize: Integer) ;
begin
   NodeDataSize := SizeOf( TNodeData );
end;

end.

