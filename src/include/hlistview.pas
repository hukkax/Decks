unit hListView;

{$mode delphi}

interface

uses
	Classes, SysUtils, Graphics, Controls, ExtCtrls, Forms, Menus,
	LMessages, LResources, FGL, hSlider;

{$WARN 5024 off : Parameter "$1" not used}

const
	ColumnSortText: array[Boolean] of String = ('▲', '▼');

type
	ThListView = class;

	ThListItem = class(TPersistent)
	public
		//ImageIndex: Integer; // TODO
		SortIndex:  Integer; // use negative value to keep at top after sorting
		Caption,
		Hint:       String;
		Color,
		Background: TColor;
		SubItems:   TStringList;
		Tag:        PtrInt;

	    constructor Create(AOwner: ThListView);
	    destructor  Destroy; override;
	end;

	ThListColumn = class
	public
		Width,
		Left,
		Percentage: Integer;
		Visible:    Boolean;
		Alignment:  TAlignment;
		DrawnRect:  TRect;
		Caption:    String;
		Tag:        PtrInt;
		AlwaysShow: Boolean;

		constructor Create(AOwner: ThListView; ACaption: String; AWidth: Integer; AAlignment: TAlignment);
		destructor  Destroy; override;
	end;

	TSelectItemProc = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; Item: ThListItem) of Object;

	(*
	TMyHintData = record
		FontSize:  Integer;
		FontColor: TColor;
		FontName:  String;
	end;

	TMyHintWindow = class(THintWindow)
	public
		function CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect; override;
	end;
	*)

	ThListView = class(TCustomControl)
	private
		FEnabled: Boolean;
		FScrollPos: Integer;
		FItemHeight: Integer;
		FItemIndex: Integer;
		FColor,
		FColorSelection,
		FColorSelectedText,
		FColorGrid,
		FColorHover: TColor;
		FScrollbar: ThRangebar;
		FMouseOver: Boolean;
		FFont: TFont;

		FSelectedItem: ThListItem;

		//FHintData: TMyHintData;
		FHoveredColumn,
		FHoveredItem,
		FPreviousHoveredItem: Integer;
		FClickedColumn: Integer;
		FHintTextWidth: Integer;
		FHintRect: TRect;

		FSortColumn: Integer;
		FSortReverse: Boolean;
		FHeaderHeight: Integer;
		FHeaderColor,
		FHeaderTextColor: TColor;

		FTextOffset: TPoint;
		FRowsVisible: Integer;
		FFirstVisibleIndex,
		FLastVisibleIndex: Integer;
		FTextStyle: TTextStyle;

		FPopupHeader,
		FPopupList: TPopupMenu;

		FOnClickItem,
		FOnSelectItem: TSelectItemProc;

		procedure SetColorGrid(AValue: TColor);
		procedure SetColorHover(AValue: TColor);
		procedure SetColorSelectedText(AValue: TColor);
		procedure SetColorSelection(AValue: TColor);
		procedure Resized;

		procedure WMSize(var Message: TLMSize); message LM_SIZE;
		procedure CMMouseEnter(var Message: TLMessage); message CM_MouseEnter;
		procedure CMMouseLeave(var Message: TLMessage); message CM_MouseLeave;
	    procedure CMHintShow(var Message: TCMHintShow); message CM_HintShow;
		procedure ShowColumnPopup(P: TPoint);
		procedure ToggleColumnVisibilityFromMenu(Sender: TObject);
		procedure SetTextStyle(AValue: TTextStyle);

	protected
		procedure SetEnabled(Value: Boolean); override;
		procedure SetItemHeight(H: Integer);
		procedure SetColor(C: TColor); override;
		procedure SetFont(F: TFont);
		procedure SetScrollPos(Y: Integer);
		procedure SetFirstVisibleIndex(I: Integer);
		procedure SetLastVisibleIndex(I: Integer);
		procedure SetItemIndex(I: Integer);
		procedure SetScrollbar(AScrollbar: ThRangebar);
		procedure SetSortColumn(I: Integer);
		procedure SetHeaderHeight(H: Integer);
		procedure SetHeaderColor(C: TColor);
		procedure SetHeaderTextColor(C: TColor);

		procedure Draw(FullRedraw: Boolean = True);
		procedure Paint; override;
		procedure UpdateScrollbar;
		procedure ChangeScale(Multiplier, Divider: Integer); override;

		procedure ScrollbarChange(Sender: TObject);
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
	    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

	public
		Columns:  TFPGObjectList<ThListColumn>;
		Items:    TFPGObjectList<ThListItem>;
		HintColumn: array of TRect;

		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

		function  AddItem(const Caption: String): ThListItem;
		function  AddColumn(const ACaption: String; AWidth: Integer;
		          AAlignment: TAlignment = taLeftJustify; AVisible: Boolean = True): ThListColumn;
		function  ColumnAt(X: Integer): Integer;
		function  GetSubItemFor(const Item: ThListItem; var Column: Integer): String;
		procedure SortItems;

		function  GetVisibleRows: Integer;
		procedure ScrollToView(const Item: ThListItem);
		procedure ScrollBy(DeltaX, DeltaY: Integer); override;
		procedure ScrollToTop;
		procedure ScrollToBottom;

		property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
		property ClickedColumn: Integer read FClickedColumn;
		property ScrollPos: Integer read FScrollPos write SetScrollPos default 0;
		property FirstVisibleIndex: Integer read FFirstVisibleIndex write SetFirstVisibleIndex;
		property LastVisibleIndex:  Integer read FLastVisibleIndex  write SetLastVisibleIndex;
		property ItemIndex: Integer read FItemIndex write SetItemIndex;
		property HoveredItem: Integer read FHoveredItem;
		property SelectedItem: ThListItem read FSelectedItem;

	published
		property Align;
		property Anchors;

		property Color:             TColor read FColor              write SetColor;
		property ColorSelection:    TColor read FColorSelection     write SetColorSelection;
		property ColorSelectedText: TColor read FColorSelectedText  write SetColorSelectedText;
		property ColorGrid:         TColor read FColorGrid          write SetColorGrid;
		property ColorHover:        TColor read FColorHover         write SetColorHover;

		property SortColumn:      Integer read FSortColumn      write SetSortColumn;
		property HeaderHeight:    Integer read FHeaderHeight    write SetHeaderHeight;
		property HeaderColor:     TColor  read FHeaderColor     write SetHeaderColor;
		property HeaderTextColor: TColor  read FHeaderTextColor write SetHeaderTextColor;

		property Enabled: Boolean read FEnabled write SetEnabled default True;
		property Font: TFont read FFont write SetFont;
		property ItemHeight: Integer read FItemHeight write SetItemHeight default 12;
		property Scrollbar: ThRangebar read FScrollbar write SetScrollbar;

		property ParentShowHint;
		//property PopupMenu;
		property PopupHeader: TPopupMenu read FPopupHeader write FPopupHeader;
		property PopupList:   TPopupMenu read FPopupList   write FPopupList;
		property ShowHint;
		property Visible;

		property OnSelectItem: TSelectItemProc read FOnSelectItem write FOnSelectItem;
		property OnClickItem:  TSelectItemProc read FOnClickItem  write FOnClickItem;
		property OnClick;
		property OnDblClick;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseUp;
		property OnMouseMove;
		property OnMouseWheel;
		property OnMouseEnter;
		property OnMouseLeave;
		property DoubleBuffered;
		property TabOrder;
		property TabStop;
	  end;


  procedure Register;


implementation

uses
	Math, LCLType;

procedure Register;
begin
	RegisterComponents('Custom', [ThListView]);
end;

{ TMyHintWindow }

(*function TMyHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String;
	AData: Pointer): TRect;
begin
	with TMyHintData(AData^) do
	begin
		Canvas.Font.Size  := FontSize;
		Canvas.Font.Color := FontColor;
		Canvas.Font.Name  := FontName;
	end;
	Result := inherited;
end;*)

// ================================================================================================
// ThListColumn
// ================================================================================================

constructor ThListColumn.Create(AOwner: ThListView; ACaption: String; AWidth: Integer;
	AAlignment: TAlignment);
begin
	inherited Create;

	if AWidth < 0 then
	begin
		Percentage := Abs(AWidth);
		Width := 0;
	end
	else
	begin
		Percentage := 0;
		Width := AWidth;
	end;

	Caption := ACaption;
	Alignment := AAlignment;
	Visible := True;
end;

destructor ThListColumn.Destroy;
begin
	inherited Destroy;
end;

// ================================================================================================
// ThListItem
// ================================================================================================

constructor ThListItem.Create(AOwner: ThListView);
begin
	inherited Create;

	//ImageIndex := -1;
	Caption := '';
	Hint := '';
	Color := clNone;
	Background := clNone;
	SubItems := TStringList.Create;
end;

destructor ThListItem.Destroy;
begin
	SubItems.Free;

	inherited Destroy;
end;

// ================================================================================================
// ThListView
// ================================================================================================

constructor ThListView.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	if AOwner is TWinControl then
		Parent := TWinControl(AOwner);
	ControlStyle := ControlStyle + [csOpaque, csDoubleClicks] - [csNoFocus];

	Columns := TFPGObjectList<ThListColumn>.Create;
	Items := TFPGObjectList<ThListItem>.Create;
	FScrollbar := nil;

	FScrollPos := 0;
	FHoveredItem := -1;
	FPreviousHoveredItem := -1;
	FItemIndex := 0;

	Width := 200;
	Height := 100;

	FColor := clWindow;
	FColorSelection := clHighlight;
	FColorSelectedText := clHighlightText;
	FColorHover := clWhite;
	FColorGrid := clSilver;

	FSortColumn := -1; // no sorting by default
	FSortReverse := False;
	FHeaderHeight := 22;
	FHeaderColor := clBtnFace;
	FHeaderTextColor := clBtnText;

	FFont  := TFont.Create;
	FFont.Color := clWindowText;

	FEnabled := True;
	Visible := True;

	FTextStyle := Canvas.TextStyle;
	with FTextStyle do
	begin
		Alignment := taLeftJustify;
		Layout    := tlCenter;
		SingleLine := True;
		Clipping := True;
		ExpandTabs := False;
		ShowPrefix := False;
		Wordbreak := False;
		EndEllipsis := True;
	end;
	Canvas.TextStyle := FTextStyle;

	SetItemHeight(FHeaderHeight);
end;

destructor ThListView.Destroy;
begin
	Items.Free;
	Columns.Free;
	FFont.Free;

	inherited Destroy;
end;

function ThListView.AddItem(const Caption: String): ThListItem;
begin
	Result := ThListItem.Create(Self);
	Result.Caption := Caption;
	Items.Add(Result);

	UpdateScrollbar;
end;

function ThListView.AddColumn(const ACaption: String; AWidth: Integer;
	AAlignment: TAlignment; AVisible: Boolean): ThListColumn;
begin
	Result := ThListColumn.Create(Self, ACaption, AWidth, AAlignment);
	Result.Visible := AVisible;
	Columns.Add(Result);
	Resized;
end;

function ThListView.GetSubItemFor(const Item: ThListItem; var Column: Integer): String;
var
	i: Integer;
	j: Integer = 0;
begin
	Result := '';
	if Item = nil then Exit;
	if Column < 0 then
		Column := FClickedColumn;

	for i := 1 to Columns.Count-1 do
	begin
		if Columns[i].Visible then
			Inc(j);
		if j >= Column then Break;
	end;

	if Column = 0 then
		Result := Item.Caption
	else
	if Column > 0 then
		Result := Item.SubItems[Column-1];
end;

procedure ThListView.ScrollToView(const Item: ThListItem);
var
	I: Integer;
begin
	I := Items.IndexOf(Item);
	if (I < FFirstVisibleIndex) then
		SetScrollPos(I)
	else
	if (I >= FLastVisibleIndex) then
		SetScrollPos(I - FRowsVisible + 1);
	UpdateScrollbar;
end;

procedure ThListView.ScrollBy(DeltaX, DeltaY: Integer);
begin
	SetFirstVisibleIndex(FFirstVisibleIndex - DeltaY);
end;

procedure ThListView.ScrollToTop;
begin
	SetFirstVisibleIndex(0);
end;

procedure ThListView.ScrollToBottom;
begin
	SetLastVisibleIndex(Items.Count-1);
end;

function ThListView.GetVisibleRows: Integer;
begin
	FRowsVisible := Trunc((ClientHeight - FHeaderHeight) / FItemHeight);
	Result := FRowsVisible;
end;

procedure ThListView.SetFirstVisibleIndex(I: Integer);
begin
	FFirstVisibleIndex := EnsureRange(I, 0, Items.Count - FRowsVisible);
	UpdateScrollbar;
	Invalidate;
end;

procedure ThListView.SetLastVisibleIndex(I: Integer);
begin
	SetFirstVisibleIndex(I - FRowsVisible);
end;

procedure ThListView.SetItemIndex(I: Integer);
begin
	if (I >= 0) and (I < Items.Count) then
	begin
		FItemIndex := I;
		ScrollToView(Items[I]);
	end
	else
		FItemIndex := -1;
	if Assigned(FOnSelectItem) then
	begin
		if FItemIndex >= 0 then
			FOnSelectItem(Self, mbLeft, [], Items[FItemIndex])
		else
			FOnSelectItem(Self, mbLeft, [], nil);
	end;
	Invalidate;
end;

procedure ThListView.SetScrollbar(AScrollbar: ThRangebar);
begin
	FScrollbar := AScrollbar;

	if Assigned(FScrollbar) then
	begin
		FScrollbar.Kind := sbVertical;
		FScrollbar.OnChange := ScrollbarChange;
	end;

	UpdateScrollbar;
end;

function CompareItem(A, B: ThListItem): Integer;
begin
	Result := A.SortIndex - B.SortIndex;
	if Result = 0 then
		Result := CompareText(A.Caption, B.Caption);
end;

procedure ThListView.SetSortColumn(I: Integer);
begin
	if I <> FSortColumn then
		FSortReverse := False
	else
		FSortReverse := not FSortReverse;
	FSortColumn := I;
	SortItems;
end;

procedure ThListView.SortItems;
var
	I: Integer;
	Sl: TStringList;
	Item: ThListItem;
begin
	I := FSortColumn;
	Sl := TStringList.Create;
	try
		(*for Item in Items do
			Item.SortIndex := 0;
		Items.Sort(@CompareItem);*)

		for Item in Items do
		begin
			if (I > 0) and (I <= Item.SubItems.Count) then
				Sl.AddObject(Item.SubItems[I-1], Item)
			else
				Sl.AddObject(Item.Caption, Item);
		end;

		Sl.Sort;

		for I := 0 to Sl.Count-1 do
		begin
			Item := ThListItem(Sl.Objects[I]);
			if Item.SortIndex < 0 then Continue;
			if FSortReverse then
				Item.SortIndex := Sl.Count-1-I
			else
				Item.SortIndex := I;
		end;

		Items.Sort(@CompareItem);
	finally
		Sl.Free;
	end;

	Invalidate;
end;

procedure ThListView.SetHeaderHeight(H: Integer);
begin
	FHeaderHeight := Max(H, 0);
	GetVisibleRows;
	Invalidate;
end;

procedure ThListView.SetHeaderColor(C: TColor);
begin
	FHeaderColor := C;
	Invalidate;
end;

procedure ThListView.SetHeaderTextColor(C: TColor);
begin
	FHeaderTextColor := C;
	Invalidate;
end;

procedure ThListView.SetEnabled(Value: Boolean);
begin
	inherited SetEnabled(Value);
	FEnabled := Value;
	Invalidate;
end;

procedure ThListView.SetItemHeight(H: Integer);
begin
	if H < 1 then
		H := FFont.GetTextHeight('Xgjq"!_');

	FItemHeight := H;
	GetVisibleRows;

	//TS := Buffer.TextExtent('Xjgl/!');
	FTextOffset.X := 5;
	FTextOffset.Y := 2; //Abs(FItemHeight - TS.cy) div 2;

	Invalidate;
end;

procedure ThListView.SetScrollPos(Y: Integer);
begin
	FScrollPos := Y;
	Scrollbar.Position := FScrollPos;
	Invalidate;
end;

procedure ThListView.SetColor(C: TColor);
begin
	inherited SetColor(C);
	FColor := C;
	Invalidate;
end;

procedure ThListView.SetColorGrid(AValue: TColor);
begin
	FColorGrid := AValue;
	Invalidate;
end;

procedure ThListView.SetColorHover(AValue: TColor);
begin
	FColorHover := AValue;
	Invalidate;
end;

procedure ThListView.SetColorSelectedText(AValue: TColor);
begin
	FColorSelectedText := AValue;
	Invalidate;
end;

procedure ThListView.SetColorSelection(AValue: TColor);
begin
	FColorSelection := AValue;
	Invalidate;
end;

procedure ThListView.SetFont(F: TFont);
begin
	FFont.Assign(F);
	Invalidate;
end;

procedure ThListView.ScrollbarChange(Sender: TObject);
begin
	FScrollPos := Round(Scrollbar.Position);
	FFirstVisibleIndex := FScrollPos;
	Invalidate;
end;

procedure ThListView.Resized;
var
	W, I: Integer;
	Col: ThListColumn;
begin
	W := ClientWidth;

	if Columns.Count = 1 then
		Columns.First.Width := W
	else
	if Columns.Count > 1 then
	begin
		for I := 1 to Columns.Count-1 do
		begin
			Col := Columns[i];
			if (Col.Visible) and (Col.Percentage = 0) then
				Dec(W, Col.Width);
		end;

		for Col in Columns do
			if (Col.Visible) and (Col.Percentage > 0) then
				Col.Width := Trunc(W / 100 * Col.Percentage);

		W := 0;
		for Col in Columns do
			if Col.Visible then
			begin
				Col.Left := W;
				Inc(W, Col.Width);
			end;

		for i := Columns.Count-1 downto 0 do
		begin
			Col := Columns[i];
			if not Col.Visible then Continue;
			if Col.Percentage > 0 then
				Col.Width := ClientWidth - 1 - Col.Left
			else
			begin
				W := ClientWidth - W;
				for Col in Columns do
				begin
					if Col = Columns.First then
						Col.Width := Col.Width + W
					else
						Col.Left := Col.Left + W;
				end;
			end;
			Break;
		end;
	end;

	SetBounds(Left, Top, Width, Height);
	UpdateScrollbar;
end;

procedure ThListView.WMSize(var Message: TLMSize);
begin
	inherited;
	Resized;
end;

function ThListView.ColumnAt(X: Integer): Integer;
var
	I: Integer;
	PT: TPoint;
begin
	PT := Point(X, 1);
	for I := 0 to Columns.Count-1 do
		if (Columns[I].Visible) and (Columns[I].DrawnRect.Contains(PT)) then
			Exit(I);
	Result := 0;
end;

procedure ThListView.KeyDown(var Key: Word; Shift: TShiftState);
var
	Handled: Boolean;
begin
	Handled := True;
	case Key of
		VK_UP:    SetItemIndex(Max(0, FItemIndex - 1));
		VK_DOWN:  SetItemIndex(Min(Items.Count-1, FItemIndex + 1));
		VK_PRIOR: SetItemIndex(Max(0, FItemIndex - FRowsVisible));
		VK_NEXT:  SetItemIndex(Min(Items.Count-1, FItemIndex + FRowsVisible));
		VK_HOME:  SetItemIndex(0);
		VK_END:   SetItemIndex(Items.Count-1);
		//VK_LEFT, VK_RIGHT: ;
	else
		Handled := False;
	end;

	if Handled then
	begin
		Invalidate;
		Key := 0;
	end
	else
		inherited KeyDown(Key, Shift);
end;

procedure ThListView.ToggleColumnVisibilityFromMenu(Sender: TObject);
var
	i: Integer;
begin
	if not (Sender is TMenuItem) then Exit;

	i := (Sender as TMenuItem).Tag;
	if (i >= 0) and (i < Columns.Count) then
	begin
		Columns[i].Visible := not Columns[i].Visible;
		Resized;
	end;
end;

procedure ThListView.SetTextStyle(AValue: TTextStyle);
begin
	FTextStyle := AValue;
	Canvas.TextStyle := FTextStyle;
	Invalidate;
end;

procedure ThListView.ShowColumnPopup(P: TPoint);
var
	Col: ThListColumn;
	Mi: TMenuItem;
	i: Integer = 0;
begin
	if FPopupHeader = nil then Exit;

	FPopupHeader.Items.Clear;

	for Col in Columns do
	begin
		if not Col.AlwaysShow then
		begin
			Mi := TMenuItem.Create(FPopupHeader);
			Mi.Caption := Trim(Col.Caption);
			Mi.Checked := Col.Visible;
			Mi.Tag := i;
			Mi.OnClick := ToggleColumnVisibilityFromMenu;
			FPopupHeader.Items.Add(Mi);
		end;
		Inc(i);
	end;

	FPopupHeader.PopUp(P.X, P.Y);
end;

procedure ThListView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	I: Integer;
	Item: ThListItem;
	P: TPoint;
begin
	SetFocus;
	I := FItemIndex;
	Y := Y - FHeaderHeight;

	if Button = mbRight then
		P := ClientToScreen(Point(X, Y));

	FClickedColumn := ColumnAt(X);

	if Y < 0 then // clicked on header
	begin
		case Button of
			mbLeft:  SetSortColumn(FClickedColumn);
			mbRight: ShowColumnPopup(P{%H-});
		end;
	end
	else
	begin
		SetItemIndex(Y div FItemHeight + FFirstVisibleIndex);
		if FItemIndex >= 0 then
		begin
			Item := Items[FItemIndex]; // else Item := nil;
			if FItemIndex <> I then
				if Assigned(FOnSelectItem) then
					FOnSelectItem(Self, Button, Shift, Item);
			if Assigned(FOnClickItem) then
				FOnClickItem(Self, Button, Shift, Item);
		end
		else
			SetItemIndex(I); // select previously selected

		if Button = mbRight then
			if FPopupList <> nil then
				FPopupList.PopUp(P.X, P.Y);
	end;

	inherited MouseDown(Button, Shift, X, Y);
	Invalidate;
end;

procedure ThListView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	inherited MouseUp(Button, Shift, X, Y - FHeaderHeight);
	Invalidate;
end;

procedure ThListView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	Old: Integer;
begin
	Old := FHoveredItem;
	Y := Y - FHeaderHeight;
	if Y < 0 then
	begin
		FHoveredItem := -1;
		FHoveredColumn := -1;
	end
	else
	begin
		FHoveredItem := Y div FItemHeight + FFirstVisibleIndex;
		if (FHoveredItem < 0) or (FHoveredItem >= Items.Count) then
			FHoveredItem := -1
		else
			FHoveredColumn := ColumnAt(X);
	end;
	if FHoveredItem <> Old then
	begin
		FPreviousHoveredItem := Old;
		Draw(False);
	end;

	inherited MouseMove(Shift, X, Y);
end;

function ThListView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
	MousePos: TPoint): Boolean;
var
	Delta: Integer;
begin
	Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
	if Result then Exit;

	Delta := WheelDelta div 120;
	if Delta > 0 then
	begin
		if FFirstVisibleIndex > 0 then
		begin
			FFirstVisibleIndex := Max(0, FFirstVisibleIndex - Delta);
			Invalidate;
		end;
	end
	else
	if Delta < 0 then
	begin
		for WheelDelta := 1 to Abs(Delta) do
			if (FFirstVisibleIndex + FRowsVisible) < Items.Count then
				Inc(FFirstVisibleIndex);
		Invalidate;
	end;
	UpdateScrollbar;
	Result := True;
end;

procedure ThListView.CMMouseEnter(var Message: TLMessage);
begin
	if csDesigning in ComponentState then Exit;
	FMouseOver := True;
	if Assigned(OnMouseEnter) then OnMouseEnter(Self);
	Invalidate;
end;

procedure ThListView.CMMouseLeave(var Message: TLMessage);
begin
	if csDesigning in ComponentState then Exit;
	FMouseOver := False;
	FHoveredItem := -1;
	if Assigned(OnMouseLeave) then OnMouseLeave(Self);
	Invalidate;
end;

procedure ThListView.CMHintShow(var Message: TCMHintShow);
begin
	inherited;
	with Message.HintInfo^ do
	begin
		(*
		FHintData.FontColor := FFont.Color;
		FHintData.FontSize  := FFont.Size;
		FHintData.FontName  := FFont.Name;

		HintWindowClass := TMyHintWindow;
		HintData := @FHintData;
		*)
		if FHintTextWidth > FHintRect.Width then
		begin
			HintStr := Hint;
			CursorRect := FHintRect;
			HintColor := Self.Color;
			HintPos := ClientToScreen(Point(FHintRect.Left+1, FHintRect.Top-2));
		end
		else
			HintStr := '';
	end;
end;

procedure ThListView.UpdateScrollbar;
var
	I: Integer;
begin
	GetVisibleRows;

	if Assigned(FScrollbar) then
	begin
		FScrollbar.OnChange := nil;

		FScrollbar.Increment := 1;
		FScrollbar.Range := Items.Count;
		FScrollbar.Window := FRowsVisible;

		I := FFirstVisibleIndex;
		if (I + FRowsVisible) >= Items.Count then
			I := Max(Items.Count - FRowsVisible, 0);
		FScrollbar.Position := I;

		FScrollbar.OnChange := ScrollbarChange;
		FScrollbar.Repaint;
	end;
end;

procedure ThListView.ChangeScale(Multiplier, Divider: Integer);
var
	H: ThListColumn;
begin
	if Multiplier <> Divider then
	begin
		FFont.Height  := MulDiv(GetFontData(FFont.Reference.Handle).Height, Multiplier, Divider);
		FItemHeight   := MulDiv(ItemHeight, Multiplier, Divider);
		FHeaderHeight := MulDiv(FHeaderHeight, Multiplier, Divider);
		for H in Columns do
			H.Width := MulDiv(H.Width, Multiplier, Divider);
	end;
	inherited ChangeScale(Multiplier, Divider);
end;

procedure ThListView.Paint;
begin
	Draw(True);
	inherited;
	if csDesigning in ComponentState then
	with inherited Canvas do
	begin
		Pen.Style := psDash;
		Brush.Style := bsClear;
		Rectangle(0, 0, Width, Height);
	end;
end;

procedure ThListView.Draw(FullRedraw: Boolean = True);

	procedure FillRect(R: TRect; C: TColor);
	begin
		Canvas.Brush.Color := C;
		Canvas.FillRect(R);
	end;

var
	W, X, Y, I, Col: Integer;
	C: TColor;
	Hovered: Boolean;
	CR, IR: TRect;
	Item: ThListItem;
	Column: ThListColumn;
begin
	C := FColor;

	Canvas.Lock;

	Canvas.Font.Assign(FFont);
	Canvas.Brush.Style := bsSolid;
	Canvas.Brush.Color := C;

	CR := Rect(0, 0, Width, ClientHeight-FHeaderHeight);

	if Columns.Count >= 1 then
		W := Columns.First.Width-1
	else
		W := CR.Width;
	Y := FHeaderHeight;

	if FullRedraw then
	begin
		Canvas.Clear;

		if Y > 0 then
		begin
			IR := Bounds(0, 0, ClientWidth-1, Y);
			FillRect(IR, FHeaderColor);
			Canvas.Font.Color := FHeaderTextColor;
			X := 0;
			if Columns.Count > 0 then
			for Col := 0 to Columns.Count-1 do
			begin
				Column := Columns[Col];
				if not (Column.Visible) then Continue;
				IR := Bounds(X, 0, Column.Width-1, Y);
				Column.DrawnRect := IR;
				FTextStyle.Alignment := Column.Alignment;
				Canvas.TextStyle := FTextStyle;
				if Col = FSortColumn then
				begin
					FillRect(IR, $333333); //Gray32(40)
					Canvas.TextRect(IR,
						IR.Left + FTextOffset.X, FTextOffset.Y,
						ColumnSortText[FSortReverse] + Column.Caption);
				end
				else
					Canvas.TextRect(IR,
						IR.Left + FTextOffset.X, FTextOffset.Y,
						Column.Caption);
				Inc(X, Column.Width);
			end;
			Canvas.Pen.Color := clBlack;
			Canvas.Line(0, Y-1, ClientWidth-1, Y-1);
		end;
	end;

	for I := Max(FFirstVisibleIndex, 0) to Items.Count-1 do
	begin
		if Y >= ClientHeight then Break;

		Item := Items[I];
		Hovered := (I = FHoveredItem);

		if not FullRedraw then
		begin
			if I = FPreviousHoveredItem then
				FillRect(Bounds(1, Y, Width-3, FItemHeight), C)
			else
			if not Hovered then
			begin
				Inc(Y, FItemHeight);
				FLastVisibleIndex := I;
				Continue;
			end;
		end;

		IR := Bounds(0, Y, W-1, FItemHeight);

		if I = FItemIndex then
		begin
			FillRect(Bounds(1, Y, Width-3, FItemHeight), FColorSelection);
			Canvas.Font.Color := FColorSelectedText;
		end
		else
		begin
			Canvas.Font.Color := IfThen(Item.Color <> clNone,
				Item.Color, FFont.Color);
			if Hovered then
			begin
				FillRect(Bounds(1, Y, Width-3, FItemHeight), FColorHover);
				SetLength(HintColumn, Item.SubItems.Count + 1);
				HintColumn[0] := IR;
			end
			else
			if Item.Background <> clNone then
				FillRect(Bounds(1, Y, Width-3, FItemHeight), Item.Background);
		end;

		FTextStyle.Alignment := Columns[0].Alignment;
		Canvas.TextStyle := FTextStyle;
		Canvas.TextRect(IR, IR.Left + FTextOffset.X, Y + FTextOffset.Y, Item.Caption);

		if (Hovered) and (FHoveredColumn = 0) then
		begin
			FHintTextWidth := Canvas.TextWidth(Item.Caption);
			FHintRect := IR;
			Hint := Item.Caption;
		end;

		if Item.SubItems.Count > 0 then
		begin
			X := W;
			if Columns.Count > 1 then
			for Col := 1 to Columns.Count-1 do
			begin
				Column := Columns[Col];
				if not (Column.Visible) then Continue;

				IR := Bounds(X, Y, Column.Width-1, FItemHeight);
				if Col <= Item.SubItems.Count then
				begin
					FTextStyle.Alignment := Column.Alignment;
					Canvas.TextStyle := FTextStyle;
					Canvas.TextRect(IR,
						IR.Left + FTextOffset.X, Y + FTextOffset.Y,
						Item.SubItems[Col-1]);
					if (Hovered) and (Col = FHoveredColumn) then
					begin
						FHintTextWidth := Canvas.TextWidth(Item.SubItems[Col-1]);
						FHintRect := IR;
						Hint := Item.SubItems[Col-1];
					end;
				end;
				Inc(X, Column.Width);
			end;
		end;

		if FColorGrid <> clNone then
		begin
			Canvas.Pen.Color := FColorGrid;
			Canvas.Line(0, IR.Bottom-1, Canvas.Width-1, IR.Bottom-1);
		end;

		FLastVisibleIndex := I;
		Inc(Y, FItemHeight);
	end;

	C := FColorGrid;
	if C = clNone then C := clBlack;
	begin
		X := 0;
		Canvas.Pen.Color := C;
		for Col := 0 to Columns.Count-1 do
			if Columns[Col].Visible then
			begin
				Inc(X, Columns[Col].Width);
				if X < ClientWidth-1 then
					Canvas.Line(X-1, 0, X-1, Canvas.Height-1);
			end;
	end;

	Canvas.Unlock;
end;


end.

