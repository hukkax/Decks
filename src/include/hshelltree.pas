unit hShellTree;

{$mode delphi}

interface

uses
	Classes, SysUtils, Controls, Graphics, Forms, ShellCtrls, ComCtrls, LMessages;

type
  TListScrollEvent = procedure(Kind, Pos: Integer) of Object;

  ThShellTree = class(TShellTreeView) //class helper for TShellTreeView
  private
	FOnScroll: TListScrollEvent;
	DefItemSpace: Integer;
	FHintWnd: THintWindow;
	function  GetScrollY: Integer;
	procedure SetScrollY(Y: Integer);
	procedure UpdateHotTrack(X, Y: Integer);
	procedure UpdateTooltip(X, Y: integer);
	procedure HintMouseLeave(Sender: TObject);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
                          MousePos: TPoint): Boolean; override;
	procedure DoPaintNode(Node: TTreeNode); override;

	procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
	procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
	procedure MouseLeave; override;
  public
	NodeUnderCursor: TTreeNode;

	constructor Create(AOwner: TComponent); override;
    function GetMaxScrollY: Integer;
    property ScrollY: Integer read GetScrollY write SetScrollY;
  published
    property HotTrackColor;
	property OnScroll: TListScrollEvent read FOnScroll write FOnScroll;
    property ScrolledLeft;
    property ScrolledTop;
  end;

  procedure Register;

implementation

uses
	Math, Themes, LCLType, LCLIntF, InterfaceBase, ImgList;

procedure Register;
begin
	RegisterComponents('Custom', [ThShellTree]);
end;

{ ThShellTree }

function ThShellTree.GetMaxScrollY: Integer;
begin
	Result := GetMaxScrollTop;
end;

function ThShellTree.GetScrollY: Integer;
begin
	Result := ScrolledTop;
end;

procedure ThShellTree.SetScrollY(Y: Integer);
begin
	if Y <> ScrolledTop then
	begin
		ScrolledTop := Y;
		if Assigned(FOnScroll) then FOnScroll(0, 0);
	end;
end;

procedure ThShellTree.WMVScroll(var Msg: TLMScroll);
begin
	inherited;
	if Assigned(FOnScroll) then FOnScroll(Msg.ScrollCode, Msg.Pos);
end;

procedure ThShellTree.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	if Assigned(OnMouseMove) then OnMouseMove(Self, Shift, X, Y);
  if (tvoAutoInsertMark in Options) then
    UpdateInsertMark(X,Y);
  UpdateTooltip(X, Y);
  UpdateHotTrack(X, Y);
end;

procedure ThShellTree.UpdateHotTrack(X, Y: Integer);
begin
  NodeUnderCursor := nil;
  if not (tvoHotTrack in Options) then Exit;
  NodeUnderCursor := GetNodeAt(X, Y);
  Invalidate;
end;

procedure ThShellTree.HintMouseLeave(Sender: TObject);
begin
  if FindLCLControl(Mouse.CursorPos)<>Self then
    FHintWnd.Hide;
end;

procedure ThShellTree.MouseLeave;
begin
	NodeUnderCursor := nil;
	HintMouseLeave(Self);
	inherited MouseLeave;
end;

procedure ThShellTree.UpdateTooltip(X, Y: integer);
var
  Node: TTreeNode;
  PHint, PLeft: TPoint;
  R, TextRect, IntRect: TRect;
  CurMonitor: TMonitor;
begin
  if not (tvoToolTips in Options) then exit;

  if FHintWnd=nil then
  begin
    FHintWnd:=THintWindow.Create(Self);
    FHintWnd.OnMouseLeave:=HintMouseLeave;
  end;

  Node := GetNodeAt(X, Y);
  if Node=nil then
  begin
    FHintWnd.Hide;
    exit;
  end;

  TextRect := Rect(Node.DisplayTextLeft, Node.Top, Node.DisplayTextRight, Node.Top + Node.Height);
  OffsetRect(TextRect, 0, -ScrolledTop);
  if not PtInRect(TextRect, Point(X, Y))
  or (IntersectRect(IntRect, TextRect, ClientRect) and EqualRect(IntRect, TextRect)) then
  begin
    FHintWnd.Hide;
    Exit;
  end;

  // Get max width for hint from monitor's work area.
  CurMonitor := GetParentForm(Self).Monitor;
  R := CurMonitor.WorkareaRect;
  R := FHintWnd.CalcHintRect(R.Right-R.Left, Node.Text, nil);
  FHintWnd.Color := Application.HintColor;

  if WidgetSet.GetLCLCapability(lcTransparentWindow) = LCL_CAPABILITY_YES then
  begin
    // Font is explicitly set for transparent hints, otherwise default font is used.
    if not FHintWnd.Visible then
    begin
      FHintWnd.Font.Assign(Self.Font);
      FHintWnd.Font.Color := Screen.HintFont.Color;
    end;
    // Put transparent hint exactly on the node.
    PHint := ClientToScreen(Point(TextRect.Left-1, TextRect.Top-3+BorderWidth));
  end
  else begin
    // By default put hint to the right side of node.
    PHint := ClientToScreen(Point(ClientWidth, TextRect.Top-3+BorderWidth));
    if PHint.X + R.Right > CurMonitor.BoundsRect.Right then
    begin                      // No space on the right? Put it to the left side.
      PLeft := ClientToScreen(Point(ClientRect.Left, ClientRect.Top));
      if PLeft.X >= R.Right then  // enough space on left?
        PHint.X := PLeft.X - R.Right;
    end;
  end;

  OffsetRect(R, PHint.X, PHint.Y);
  FHintWnd.ActivateHint(R, Node.Text)
end;

function ThShellTree.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
	Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
	if Assigned(FOnScroll) then
		FOnScroll(0, ScrolledTop);
end;

procedure ThShellTree.DoPaintNode(Node: TTreeNode);
var
  NodeRect: TRect;
  VertMid, VertDelta, RealExpandSignSize, RealIndent: integer;
  NodeSelected, NodeHot, HasExpandSign: boolean;

  function InvertColor(AColor: TColor): TColor;
  var Red, Green, Blue: integer;
  begin
    if AColor<>clHighlight then begin
      Result:=clWhite;
      Red:=(AColor shr 16) and $ff;
      Green:=(AColor shr 8) and $ff;
      Blue:=AColor and $ff;
      if Red+Green+Blue>$180 then
        Result:=clBlack;
      //DebugLn(['[TCustomTreeView.DoPaintNode.InvertColor] Result=',Result,' ',Red,',',Green,',',Blue]);
    end
    else
      Result := clHighlightText;
  end;

  procedure DrawVertLine(X, Y1, Y2: Integer);
  begin
    if Y1 > Y2 then
      Exit;
    if TreeLinePenStyle = psPattern then
    begin
      // TODO: implement psPattern support in the LCL
      Y1 := Y1 + VertDelta;
      while Y1 < Y2 do
      begin
        Canvas.Pixels[X, Y1] := TreeLineColor;
        inc(Y1, 2);
      end;
    end
    else
    begin
      Canvas.MoveTo(X, Y1);
      Canvas.LineTo(X, Y2);
    end;
  end;

  procedure DrawHorzLine(Y, X1, X2: Integer);
  begin
    if X1 > X2 then
      Exit;
    if TreeLinePenStyle = psPattern then
    begin
      // TODO: implement psPattern support in the LCL
      while X1 < X2 do
      begin
        Canvas.Pixels[X1, Y] := TreeLineColor;
        inc(X1, 2);
      end;
    end
    else
    begin
      Canvas.MoveTo(X1, Y);
      Canvas.LineTo(X2, Y);
    end;
  end;

  function DrawTreeLines(CurNode: TTreeNode): integer;
  // paints tree lines, returns indent
  var
    CurMid: integer;
  begin
    if (CurNode <> nil) and ((tvoShowRoot in Options) or (CurNode.Parent<>nil)) then
    begin
      Result := DrawTreeLines(CurNode.Parent);
      if ShowLines then
      begin
        CurMid := Result + (RealIndent shr 1);
        if CurNode = Node then
        begin
          // draw horizontal line
          if HasExpandSign then
            DrawHorzLine(VertMid, CurMid + RealExpandSignSize div 2, Result + RealIndent)
          else
            DrawHorzLine(VertMid, CurMid, Result + RealIndent);
        end;

        if (CurNode.GetNextVisibleSibling <> nil) then
        begin
          // draw vertical line to next brother
          if (CurNode = Node) and HasExpandSign then
          begin
            if (Node.Parent = nil) and (Node.GetPrevSibling = nil) then
              DrawVertLine(CurMid, VertMid + RealExpandSignSize div 2, NodeRect.Bottom)
            else
            begin
              DrawVertLine(CurMid, NodeRect.Top, VertMid);
              DrawVertLine(CurMid, VertMid + RealExpandSignSize div 2 + VertDelta, NodeRect.Bottom);
            end;
          end
          else
          if (Node.Parent = nil) and (Node.GetPrevSibling = nil) then
            DrawVertLine(CurMid, VertMid + VertDelta, NodeRect.Bottom)
          else
            DrawVertLine(CurMid, NodeRect.Top, NodeRect.Bottom);
        end else
        if (CurNode = Node) then
        begin
          // draw vertical line from top to horizontal line
          if HasExpandSign then
          begin
            if ((InsertMarkNode = Node) and (InsertMarkType = tvimAsNextSibling)) then
            begin
              DrawVertLine(CurMid, NodeRect.Top, VertMid);
              DrawVertLine(CurMid, VertMid + RealExpandSignSize div 2, NodeRect.Bottom - 1);
            end
            else
              DrawVertLine(CurMid, NodeRect.Top, VertMid);
          end
          else
          if ((InsertMarkNode = Node) and (InsertMarkType = tvimAsNextSibling)) then
            DrawVertLine(CurMid, NodeRect.Top, NodeRect.Bottom - 1)
          else
            DrawVertLine(CurMid, NodeRect.Top, VertMid);
        end;
      end;
      inc(Result, RealIndent);
    end else
    begin
      Result := BorderWidth - ScrolledLeft;
      if CurNode <> nil then // indent first level of tree with ShowRoot = false a bit
        inc(Result, RealIndent shr 2);
    end;
  end;

  procedure DrawExpandSign(MidX, MidY: integer; CollapseSign: boolean);
  const
    PlusMinusDetail: array[Boolean {Hot}, Boolean {Expanded}] of TThemedTreeview =
    (
      (ttGlyphClosed, ttGlyphOpened),
      (ttHotGlyphClosed, ttHotGlyphOpened)
    );
  var
    HalfSize, ALeft, ATop, ARight, ABottom, SmallIndent: integer;
    Points: array[0..2] of TPoint; // for triangle
    Details: TThemedElementDetails;
    R: TRect;
    PrevColor: TColor;
  const
    cShiftHorzArrow = 2; //paint horz arrow N pixels upper than MidY
  begin
    HalfSize := RealExpandSignSize div 2;
    //if not Odd(RealExpandSignSize) then
    //  Dec(HalfSize);
    ALeft := MidX - HalfSize;
    ARight := MidX + HalfSize;
    ATop := MidY - HalfSize;
    ABottom := MidY + HalfSize;

    if Assigned(OnCustomDrawArrow) then
    begin
      OnCustomDrawArrow(Self, Rect(ALeft, ATop, ARight, ABottom), not CollapseSign);
      Exit
    end;

    with Canvas do
    begin
      Pen.Color := ExpandSignColor;
      Pen.Style := psSolid;
      case ExpandSignType of
      tvestTheme:
        begin
          // draw a themed expand sign. Todo: track hot
          R := Rect(ALeft, ATop, ARight, ABottom);
          Details := ThemeServices.GetElementDetails(PlusMinusDetail[False, CollapseSign]);
          ThemeServices.DrawElement(Canvas.Handle, Details, R, nil);
        end;
      tvestPlusMinus:
        begin
          // draw a plus or a minus sign
          R := Rect(ALeft, ATop, ARight+1, ABottom+1); //+1 for centering of line in square
          Rectangle(R);
          SmallIndent := Scale96ToFont(2);
          MoveTo(R.Left + SmallIndent, MidY);
          LineTo(R.Right - SmallIndent, MidY);
          if not CollapseSign then
          begin
            MoveTo(MidX, R.Top + SmallIndent);
            LineTo(MidX, R.Bottom - SmallIndent);
          end;
        end;
      tvestArrow,
      tvestArrowFill:
        begin
          // draw an arrow. down for collapse and right for expand
          R := Rect(ALeft, ATop, ARight+1, ABottom+1); //+1 for simmetry of arrow
          if CollapseSign then
          begin
            // draw an arrow down
            Points[0] := Point(R.Left, MidY - cShiftHorzArrow);
            Points[1] := Point(R.Right - 1, MidY - cShiftHorzArrow);
            Points[2] := Point(MidX, R.Bottom - 1 - cShiftHorzArrow);
          end else
          begin
            // draw an arrow right
            Points[0] := Point(MidX - 1, ATop);
            Points[1] := Point(R.Right - 2, MidY);
            Points[2] := Point(MidX - 1, R.Bottom - 1);
          end;

          if ExpandSignType = tvestArrowFill then
          begin
            PrevColor := Brush.Color;
            Brush.Color := ExpandSignColor;
          end;
          Polygon(Points, 3, False);
          if ExpandSignType = tvestArrowFill then
          begin
            Brush.Color := PrevColor;
          end;
        end;
      end;
    end;
  end;

  procedure DrawInsertMark;
  var
    InsertMarkRect: TRect;
    x: Integer;
  begin
    case InsertMarkType of

    tvimAsFirstChild:
      if InsertMarkNode=Node then begin
        // draw insert mark for new first child
        with Canvas do begin
          // draw virtual tree line
          Pen.Color:=TreeLineColor;
          // Pen.Style:=TreeLinePenStyle; ToDo: not yet implemented in all widgetsets
          x:=Node.DisplayExpandSignRight+RealIndent div 2;
          MoveTo(x,NodeRect.Bottom-3);
          LineTo(x,NodeRect.Bottom-2);
          x:=Node.DisplayExpandSignRight+RealIndent;
          LineTo(x,NodeRect.Bottom-2);
          Pen.Style:=psSolid;

          // draw virtual rectangle
          Pen.Color := TreeLineColor;
          Brush.Color := SelectionColor;
          InsertMarkRect:=Rect(x,NodeRect.Bottom-3,
                               NodeRect.Right,NodeRect.Bottom-1);
          Rectangle(InsertMarkRect);
        end;
      end;

    tvimAsPrevSibling:
      if InsertMarkNode=Node then begin
        // draw insert mark for new previous sibling
        with Canvas do begin
          // draw virtual tree line
          Pen.Color:=TreeLineColor;
          //Pen.Style:=TreeLinePenStyle; ToDo: not yet implemented in all widgetsets
          x:=Node.DisplayExpandSignLeft+RealIndent div 2;
          MoveTo(x,NodeRect.Top+1);
          x:=Node.DisplayExpandSignRight;
          LineTo(x,NodeRect.Top+1);
          Pen.Style:=psSolid;

          // draw virtual rectangle
          Pen.Color:=TreeLineColor;
          Brush.Color:=SelectionColor;
          InsertMarkRect:=Rect(x,NodeRect.Top,
                               NodeRect.Right,NodeRect.Top+2);
          Rectangle(InsertMarkRect);
        end;
      end;

    tvimAsNextSibling:
      if InsertMarkNode=Node then begin
        // draw insert mark for new next sibling
        with Canvas do begin
          // draw virtual tree line
          Pen.Color:=TreeLineColor;
          //Pen.Style:=TreeLinePenStyle; ToDo: not yet implemented in all widgetsets
          x:=Node.DisplayExpandSignLeft+RealIndent div 2;
          MoveTo(x,NodeRect.Bottom-3);
          LineTo(x,NodeRect.Bottom-2);
          x:=Node.DisplayExpandSignRight;
          LineTo(x,NodeRect.Bottom-2);
          Pen.Style:=psSolid;

          // draw virtual rectangle
          Pen.Color:=TreeLineColor;
          Brush.Color:=SelectionColor;
          InsertMarkRect:=Rect(x,NodeRect.Bottom-3,
                               NodeRect.Right,NodeRect.Bottom-1);
          Rectangle(InsertMarkRect);
        end;
      end;

    end;
  end;

  procedure DrawBackground(IsSelected, IsHot: Boolean; ARect: TRect);
  var
    Details: TThemedElementDetails;
    CurBackgroundColor,bclr: TColor;
  begin
    bclr:=Canvas.Brush.Color;
    try
      if (tvoRowSelect in Options) and IsSelected then
        if tvoThemedDraw in Options then
        begin
          if tvoFocusedPainting in States then
            Details := ThemeServices.GetElementDetails(ttItemSelected)
          else
            Details := ThemeServices.GetElementDetails(ttItemSelectedNotFocus);
          if ThemeServices.HasTransparentParts(Details) then
          begin
            Canvas.Brush.Color := Color;
            Canvas.FillRect(ARect);
          end;
          ThemeServices.DrawElement(Canvas.Handle, Details, ARect, nil);
          Exit;
        end
        else
          CurBackgroundColor := SelectionColor
      else
	  begin
		if IsHot then
		  CurBackgroundColor := HotTrackColor
		else
          CurBackgroundColor := Color;
	  end;
	  if CurBackgroundColor <> clNone then
      begin
        Canvas.Brush.Color := CurBackgroundColor;
        Canvas.FillRect(ARect);
      end;
    finally
      Canvas.Brush.Color := bclr;
    end;
  end;

  procedure DrawNodeText(IsSelected, IsHot: Boolean; NodeRect: TRect; AText: String);
  var
    Details: TThemedElementDetails;
    //NeedUnderline: Boolean;
    //PrevFontStyle: TFontStyles;
    PrevFontColor: TColor;
  begin
    if IsSelected then
    begin
      if tvoFocusedPainting in States then
        Details := ThemeServices.GetElementDetails(ttItemSelected)
      else
        Details := ThemeServices.GetElementDetails(ttItemSelectedNotFocus);
      if not (tvoRowSelect in Options) then
        if (tvoThemedDraw in Options) then
          ThemeServices.DrawElement(Canvas.Handle, Details, NodeRect, nil)
        else
        begin
          Canvas.Brush.Color := SelectionColor;
          Canvas.Font.Color := IfThen(SelectionFontColorUsed, SelectionFontColor, InvertColor(SelectionColor));
          Canvas.FillRect(NodeRect);
        end
      else
      if not (tvoThemedDraw in Options) then
      begin
        Canvas.Brush.Color := SelectionColor;
        Canvas.Font.Color := IfThen(SelectionFontColorUsed, SelectionFontColor, InvertColor(SelectionColor));
        Canvas.FillRect(NodeRect);
      end;
    end
    else
      Details := ThemeServices.GetElementDetails(ttItemNormal);

    if IsHot then
    begin
		PrevFontColor := Canvas.Brush.Color;
        Canvas.Brush.Color := HotTrackColor;
    end;

    if (tvoThemedDraw in Options) then
      ThemeServices.DrawText(Canvas, Details, AText, NodeRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX, 0)
    else
      DrawText(Canvas.Handle, PChar(AText), -1, NodeRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);

    if IsHot then
		Canvas.Brush.Color := PrevFontColor;
  end;


var
  x, ImgIndex: integer;
  CurTextRect: TRect;
  DrawState: TCustomDrawState;
  PaintImages: boolean;
  OverlayIndex: Integer;
  ImageRes, StateImageRes: TScaledImageListResolution;
  //SelectedNode: TTreeNode;
begin
  if Assigned(Images) then
    ImageRes := Images.ResolutionForPPI[ImagesWidth, Font.PixelsPerInch, GetCanvasScaleFactor];
  if Assigned(StateImages) then
    StateImageRes := StateImages.ResolutionForPPI[StateImagesWidth, Font.PixelsPerInch, GetCanvasScaleFactor];
  RealExpandSignSize := ExpandSignSize;
  RealIndent := Indent;
  NodeRect := Node.DisplayRect(False);
  if (NodeRect.Bottom < 0) or (NodeRect.Top >= ClientHeight) then
    Exit;

//  SelectedNode := GetSelection;
  NodeSelected := (Node.Selected) or (Node.MultiSelected);
  NodeHot := (not NodeSelected) and (tvoHotTrack in Options) and (HotTrackColor<>clNone) and
	(Node = NodeUnderCursor);

  Canvas.Font.Color := Font.Color;
  Canvas.Brush.Color := Color;
  PaintImages := True;
  if IsCustomDrawn(dtItem, cdPrePaint) then
  begin
    DrawState := [];
    if NodeSelected then
      Include(DrawState, cdsSelected);
    if Node.Focused then
      Include(DrawState, cdsFocused);
    if Node.MultiSelected then
      Include(DrawState, cdsMarked);
    if not CustomDrawItem(Node, DrawState, cdPrePaint, PaintImages) then Exit;
  end;

  VertMid := NodeRect.Top + (NodeRect.Bottom - NodeRect.Top) div 2;
  HasExpandSign := ShowButtons and Node.HasChildren and ((tvoShowRoot in Options) or (Node.Parent <> nil));
  VertDelta := Ord(DefaultItemHeight and 3 = 2);
  //DebugLn(['[TCustomTreeView.DoPaintNode] Node=',DbgS(Node),' Node.Text=',Node.Text,' NodeRect=',NodeRect.Left,',',NodeRect.Top,',',NodeRect.Right,',',NodeRect.Bottom,' VertMid=',VertMid]);
  with Canvas do
  begin
    // draw background
    DrawBackground(NodeSelected, NodeHot, NodeRect);

    // draw tree lines
    Pen.Color := TreeLineColor;
    Pen.Style := TreeLinePenStyle;
    //if Pen.Style = psPattern then Pen.SetPattern(FTreeLinePenPattern);
    x := DrawTreeLines(Node);
    Pen.Style := psSolid;

    // draw expand sign
    if HasExpandSign then
      DrawExpandSign(x - RealIndent + (RealIndent shr 1), VertMid, Node.Expanded);

    // draw state icon
    if (StateImages <> nil) then
    begin
      if (Node.StateIndex >= 0) and (Node.StateIndex < StateImages.Count) then
      begin
        if PaintImages then
          StateImageRes.Draw(Canvas, x + 1, NodeRect.Top +(NodeRect.Bottom - NodeRect.Top - StateImageRes.Height) div 2,
            Node.StateIndex, True);
        Inc(x, StateImageRes.Width + DefItemSpace);
      end;
    end;

    // draw icon
    if (Images <> nil) then
    begin
      //if Node <> SelectedNode then
	  if NodeSelected then
      begin
      	GetImageIndex(Node);
        ImgIndex := Node.ImageIndex;
      end
      else
      begin
      	GetSelectedIndex(Node);
        ImgIndex := Node.SelectedIndex;
      end;
      if (ImgIndex >= 0) and (ImgIndex < Images.Count) then
      begin
        if PaintImages then
        begin
      	  if (Node.OverlayIndex >= 0) then begin
            OverlayIndex:=Node.OverlayIndex;
            if Images.HasOverlays then begin
              ImageRes.DrawOverlay(Canvas, x + 1, NodeRect.Top + (NodeRect.Bottom - NodeRect.Top - ImageRes.Height) div 2,
                 ImgIndex, OverlayIndex, Node.NodeEffect);
            end else begin
              // draw the Overlay using the image from the list
              // set an Overlay
              Images.OverLay(OverlayIndex,0);
              // draw overlay
              ImageRes.DrawOverlay(Canvas, x + 1, NodeRect.Top + (NodeRect.Bottom - NodeRect.Top - ImageRes.Height) div 2,
                 ImgIndex, 0, Node.NodeEffect);
              // reset the Overlay
              Images.OverLay(-1,0);
            end;
          end
          else begin
            ImageRes.Draw(Canvas, x + 1, NodeRect.Top + (NodeRect.Bottom - NodeRect.Top - ImageRes.Height) div 2,
               ImgIndex, Node.NodeEffect);
          end;
        end;
        Inc(x, ImageRes.Width + DefItemSpace);
      end;
    end;

    // draw text
    if Node.Text <> '' then
    begin
      CurTextRect := NodeRect;
      CurTextRect.Left := x;
      CurTextRect.Right := x + TextWidth(Node.Text) + RealIndent div 2;
      DrawNodeText(NodeSelected, NodeHot, CurTextRect, Node.Text);
    end;

    // draw separator
    if (tvoShowSeparators in Options) then
    begin
      Pen.Color:=SeparatorColor;
      MoveTo(NodeRect.Left,NodeRect.Bottom-1);
      LineTo(NodeRect.Right,NodeRect.Bottom-1);
    end;

    // draw insert mark
    DrawInsertMark;
  end;
  PaintImages := true;
  if IsCustomDrawn(dtItem, cdPostPaint) then
  begin
    DrawState:=[];
    if Node.Selected then
      Include(DrawState,cdsSelected);
    if Node.Focused then
      Include(DrawState,cdsFocused);
    if Node.MultiSelected then
      Include(DrawState,cdsMarked);
    if not CustomDrawItem(Node,DrawState,cdPostPaint,PaintImages) then exit;
  end;
end;


constructor ThShellTree.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FOnScroll := nil;
	DefItemSpace := ScaleY(2, 96);
end;


end.

