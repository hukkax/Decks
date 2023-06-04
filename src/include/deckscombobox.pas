// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit DecksComboBox;

{$mode delphi}

interface

uses
	Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics, Types,
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	BCButton, DecksButton, DecksBevel,
	StdCtrls, BCTypes, BCBaseCtrls, BGRABitmap, BGRABitmapTypes, LMessages, LCLType;

type
	TFormPopup = class(TForm)
		{$IFDEF WINDOWS}
		procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
		procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
		{$ENDIF}
	end;

	TDecksComboBox = class(TBCStyleCustomControl)
	private
		FButton: TDecksButton;
		FDropDownBorderSize: integer;
		FDropDownCount: integer;
		FDropDownColor: TColor;
		FDropDownFontColor: TColor;
		FDropDownFontHighlight: TColor;
		FDropDownHighlight: TColor;
		FFocusBorderColor: TColor;
		FFocusBorderOpacity: byte;
		FItems: TStringList;
		FItemIndex: integer;
		FForm: TFormPopup;
		FFormHideDate: TDateTime;
		FHoverItem: integer;
		FItemHeight: integer;
		FListBox: TListBox;
		FDropDownBorderColor: TColor;
		FOnDrawItem: TDrawItemEvent;
		FOnDrawSelectedItem: TOnAfterRenderBCButton;
		FOnChange: TNotifyEvent;
		FOnDropDown: TNotifyEvent;
		FDrawingDropDown: boolean;
		FTimerCheckFormHide: TTimer;
		FQueryFormHide: boolean;
		function GetArrowFlip: boolean;
		function GetComboCanvas: TCanvas;
		function GetArrowSize: integer;
		function GetArrowWidth: integer;
		function GetGlobalOpacity: byte;
		function GetItemText: string;
		function GetDropDownColor: TColor;
		function GetItemIndex: integer;
		function GetItems: TStrings;
		function GetOnDrawSelectedItem: TOnAfterRenderBCButton;
		function GetRounding: TBCRounding;
		function GetStateNormal: TBCButtonState;
		function GetStaticButton: boolean;
		function GetBevel: TBCBevel;
		procedure ButtonClick(Sender: TObject);
		procedure FormDeactivate(Sender: TObject);
		procedure FormHide(Sender: TObject);
		procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure ListBoxMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
		procedure ListBoxMouseLeave(Sender: TObject);
		procedure ListBoxMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
		procedure ListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
		procedure ListBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
		procedure OnAfterRenderButton(Sender: TObject; const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
		procedure OnTimerCheckFormHide(Sender: TObject);
		procedure SetArrowFlip(AValue: boolean);
		procedure SetArrowSize(AValue: integer);
		procedure SetArrowWidth(AValue: integer);
		procedure SetDropDownColor(AValue: TColor);
		procedure SetGlobalOpacity(AValue: byte);
		procedure SetItemIndex(AValue: integer);
		procedure SetItems(AValue: TStrings);
		procedure SetOnDrawSelectedItem(AValue: TOnAfterRenderBCButton);
		procedure SetRounding(AValue: TBCRounding);
		procedure SetStateNormal(AValue: TBCButtonState);
		procedure SetStaticButton(AValue: boolean);
		procedure SetBevel(AValue: TBCBevel);
	protected
		function  GetStyleExtension: String; override;
		function  GetListBox: TListBox;
		procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
		procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
		procedure UpdateFocus(AFocused: boolean);
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
		procedure CreateForm;
		procedure FreeForm;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		{ Assign the properties from Source to this instance }
		procedure Assign(Source: TPersistent); override;
		procedure Clear;
		property HoverItem: integer read FHoverItem;
		property Button: TDecksButton read FButton write FButton;
		property ListBox: TListBox read GetListBox;
		property Text: string read GetItemText;
	published
		property Anchors;
		property Canvas: TCanvas read GetComboCanvas;
		property Items: TStrings read GetItems write SetItems;
		property ItemIndex: integer read GetItemIndex write SetItemIndex;
		property ItemHeight: integer read FItemHeight write FItemHeight default 0;
		property ArrowSize: integer read GetArrowSize write SetArrowSize;
		property ArrowWidth: integer read GetArrowWidth write SetArrowWidth;
		property ArrowFlip: boolean read GetArrowFlip write SetArrowFlip default False;
		property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clBlack;
		property FocusBorderOpacity: byte read FFocusBorderOpacity write FFocusBorderOpacity default 255;
		property DropDownBorderColor: TColor read FDropDownBorderColor write FDropDownBorderColor default clWindowText;
		property DropDownBorderSize: integer read FDropDownBorderSize write FDropDownBorderSize default 1;
		property DropDownColor: TColor read GetDropDownColor write SetDropDownColor default clWindow;
		property DropDownFontColor: TColor read FDropDownFontColor write FDropDownFontColor default clWindowText;
		property DropDownCount: integer read FDropDownCount write FDropDownCount default 8;
		property DropDownHighlight: TColor read FDropDownHighlight write FDropDownHighlight default clHighlight;
		property DropDownFontHighlight: TColor read FDropDownFontHighlight write FDropDownFontHighlight default clHighlightText;
		property GlobalOpacity: byte read GetGlobalOpacity write SetGlobalOpacity;
		property Rounding: TBCRounding read GetRounding write SetRounding;
		property Bevel: TBCBevel read GetBevel write SetBevel;
		property StateNormal: TBCButtonState read GetStateNormal write SetStateNormal;
		property StaticButton: boolean read GetStaticButton write SetStaticButton;
		property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
		property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
		property OnDrawSelectedItem: TOnAfterRenderBCButton read GetOnDrawSelectedItem write SetOnDrawSelectedItem;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property TabStop;
		property TabOrder;
	end;

	procedure Register;

implementation

uses
	Math, PropEdits, LCLIntf, BGRAText;

procedure Register;
begin
	RegisterComponents('Decks', [TDecksComboBox]);
end;

{ TFormPopup }

{$IFDEF WINDOWS}
procedure TFormPopup.WMActivate(var Msg: TWMActivate);
begin
	SendMessage(Self.Handle, WM_NCACTIVATE, 1, 0);
	inherited;
	// if we are being activated, then give pretend activation state back to our owner
	if (Msg.Active <> WA_INACTIVE) then
		SendMessage(Self.PopupParent.Handle, WM_NCACTIVATE, WPARAM(True), -1);
end;

procedure TFormPopup.WMMouseActivate(var Message: TWMMouseActivate);
begin
	Message.Result := MA_NOACTIVATE;
end;
{$ENDIF}

{ TDecksComboBox }

procedure TDecksComboBox.ButtonClick(Sender: TObject);
const
	MinDelayReopen = 500 / (1000 * 60 * 60 * 24) / 2;
var
	p: TPoint;
	h: Integer;
	s: TSize;
	ParFrm, Frm: TCustomForm;
begin
	{$IFDEF DARWIN}
	if Assigned(FForm) and not FForm.Visible then FreeForm;
	{$ENDIF}

	CreateForm;

	if FForm.Visible then
		FForm.Visible := False
	else
	if Now > FFormHideDate + MinDelayReopen then
	begin
		p := ControlToScreen(Types.Point(FButton.Left, FButton.Top + FButton.Height));
		FForm.Left := p.X;
		FForm.Top := p.Y;
		FForm.Color := FDropDownBorderColor;
		FListBox.Font.Name := Button.StateNormal.FontEx.Name;
		FListBox.Font.Style := Button.StateNormal.FontEx.Style;
		FListBox.Font.Height := FontEmHeightSign * Button.StateNormal.FontEx.Height;
		Self.Canvas.Font.Assign(FListBox.Font);
		if Assigned(FOnDrawItem) and (FItemHeight <> 0) then
			h := FItemHeight
		else
			h := Self.Canvas.GetTextHeight('Hg');
		{$IFDEF WINDOWS}Inc(h, 6);{$ENDIF}
		FListBox.ItemHeight := h;
		{$IFDEF LINUX}Inc(h, 6);{$ENDIF}
		{$IFDEF DARWIN}Inc(h, 2);{$ENDIF}
		s := TSize.Create(FButton.Width,
			h * Min(Items.Count, FDropDownCount) + 2 * FDropDownBorderSize);
		FForm.ClientWidth := s.cx;
		FForm.ClientHeight := s.cy;
		FListBox.SetBounds(FDropDownBorderSize, FDropDownBorderSize,
			s.cx - 2 * FDropDownBorderSize,
			s.cy - 2 * FDropDownBorderSize);
		if FForm.Top + FForm.Height > Screen.WorkAreaTop + Screen.WorkAreaHeight then
			FForm.Top := FForm.Top - FForm.Height - Self.Height;

		// give pretend activation state back to our owner
		Frm := GetParentForm(Self);
		ShowWindow(FForm.Handle, SW_SHOWNOACTIVATE);
		FForm.Visible := True;
//		if Frm <> nil then // and (Msg.Active <> WA_INACTIVE) then
//			SendMessage(Frm.Handle, LM_NCACTIVATE, WPARAM(True), -1);

		if Assigned(FOnDropDown) then
			FOnDropDown(self);
{		if FListBox.CanSetFocus then
			FListBox.SetFocus;}
		FTimerCheckFormHide.Enabled := True;
		FQueryFormHide := False;
	end;
end;

procedure TDecksComboBox.FormDeactivate(Sender: TObject);
begin
	FQueryFormHide := True;
end;

procedure TDecksComboBox.FormHide(Sender: TObject);
begin
	FFormHideDate := Now;
end;

function TDecksComboBox.GetArrowFlip: boolean;
begin
	Result := Button.FlipArrow;
end;

function TDecksComboBox.GetComboCanvas: TCanvas;
begin
	if FDrawingDropDown then
		Result := ListBox.Canvas
	else
		Result := inherited Canvas;
end;

function TDecksComboBox.GetArrowSize: integer;
begin
	Result := Button.DropDownArrowSize;
end;

function TDecksComboBox.GetArrowWidth: integer;
begin
	Result := Button.DropDownWidth;
end;

function TDecksComboBox.GetGlobalOpacity: byte;
begin
	Result := Button.GlobalOpacity;
end;

function TDecksComboBox.GetItemText: string;
begin
	if ItemIndex <> -1 then
		Result := Items[ItemIndex]
	else
		Result := '';
end;

function TDecksComboBox.GetDropDownColor: TColor;
begin
	if Assigned(FListBox) then
		Result := FListBox.Color
	else
		Result := FDropDownColor;
end;

function TDecksComboBox.GetItemIndex: integer;
begin
	if Assigned(FListBox) then
		Result := FListBox.ItemIndex
	else
	begin
		if FItemIndex >= Items.Count then
			FItemIndex := -1;
		Result := FItemIndex;
	end;
end;

function TDecksComboBox.GetItems: TStrings;
begin
	if Assigned(FListBox) then
		Result := FListBox.Items
	else
		Result := FItems;
end;

function TDecksComboBox.GetOnDrawSelectedItem: TOnAfterRenderBCButton;
begin
	Result := FOnDrawSelectedItem;
end;

function TDecksComboBox.GetRounding: TBCRounding;
begin
	Result := Button.Rounding;
end;

function TDecksComboBox.GetStateNormal: TBCButtonState;
begin
	Result := Button.StateNormal;
end;

function TDecksComboBox.GetStaticButton: boolean;
begin
	Result := Button.StaticButton;
end;

procedure TDecksComboBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
	begin
		ButtonClick(nil);
		Key := 0;
	end;
end;

procedure TDecksComboBox.ListBoxMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	FQueryFormHide := True;
end;

procedure TDecksComboBox.ListBoxMouseLeave(Sender: TObject);
begin
	FHoverItem := -1;
	FListBox.Repaint;
end;

procedure TDecksComboBox.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	TempItem: Integer;
begin
	TempItem := FListBox.ItemAtPos(Types.Point(X, Y), True);
	if TempItem <> FHoverItem then
	begin
		FHoverItem := TempItem;
		if (FHoverItem <> -1) and ([ssLeft, ssRight] * Shift <> []) then
			FListBox.ItemIndex := FHoverItem;
		FListBox.Repaint;
	end;
end;

procedure TDecksComboBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
	Button.Caption := GetItemText;
	if User and Assigned(FOnChange) then FOnChange(self);
end;

procedure TDecksComboBox.ListBoxDrawItem(Control: TWinControl; Index: Integer;
	ARect: TRect; State: TOwnerDrawState);
var
	aCanvas: TCanvas;
begin
	if Assigned(FOnDrawItem) then
	begin
		FDrawingDropDown := True;
		Exclude(State, odSelected);
		if Index = HoverItem then Include(State, odSelected);
		if Index = ItemIndex then Include(State, odChecked);
		try
			FOnDrawItem(Control, Index, ARect, State);
		finally
			FDrawingDropDown := False;
		end;
		Exit;
	end;

	aCanvas := TListBox(Control).Canvas;
	if Index = HoverItem then
	begin
		aCanvas.Brush.Color := DropDownHighlight;
		aCanvas.Font.Color := DropDownFontHighlight;
	end
	else
	begin
		aCanvas.Brush.Color := DropDownColor;
		aCanvas.Font.Color := DropDownFontColor;
	end;
	aCanvas.Pen.Style := psClear;
	aCanvas.FillRect(ARect);
	aCanvas.TextRect(ARect, ARect.Left + 4,
		ARect.Top + (ARect.Height - aCanvas.GetTextHeight(Items[Index])) div 2,
		Items[Index]);
end;

procedure TDecksComboBox.OnAfterRenderButton(Sender: TObject;
	const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
var
	focusMargin: integer;
begin
	if Assigned(FOnDrawSelectedItem) then
		FOnDrawSelectedItem(self, ABGRA, AState, ARect);
	if Focused then
	begin
		focusMargin := 2; //round(2 * Button.CanvasScale);
		ABGRA.RectangleAntialias(ARect.Left + focusMargin, ARect.Top + focusMargin,
			ARect.Right - focusMargin - 1, ARect.Bottom - focusMargin - 1,
			ColorToBGRA(FocusBorderColor, FocusBorderOpacity),
			1 {Button.CanvasScale});
	end;
end;

procedure TDecksComboBox.OnTimerCheckFormHide(Sender: TObject);
begin
	if Assigned(FForm) and FForm.Visible and
		({$IFDEF DARWIN}not FForm.Active or{$ENDIF} FQueryFormHide) then
	begin
		FForm.Visible := False;
		FQueryFormHide := False;
		FTimerCheckFormHide.Enabled := False;
	end;
end;

procedure TDecksComboBox.SetArrowFlip(AValue: boolean);
begin
	Button.FlipArrow := AValue;
end;

procedure TDecksComboBox.SetArrowSize(AValue: integer);
begin
	Button.DropDownArrowSize := AValue;
end;

procedure TDecksComboBox.SetArrowWidth(AValue: integer);
begin
	Button.DropDownWidth := AValue;
end;

procedure TDecksComboBox.SetDropDownColor(AValue: TColor);
begin
	if Assigned(FListBox) then
		FListBox.Color := AValue
	else
		FDropDownColor := AValue;
end;

procedure TDecksComboBox.SetGlobalOpacity(AValue: byte);
begin
	Button.GlobalOpacity := AValue;
end;

procedure TDecksComboBox.SetItemIndex(AValue: integer);
begin
	if Assigned(FListBox) then
		FListBox.ItemIndex := AValue
	else
	begin
		if AValue <> FItemIndex then
		begin
			FItemIndex := AValue;
			Button.Caption := GetItemText;
		end;
	end;
end;

procedure TDecksComboBox.SetItems(AValue: TStrings);
begin
	if Assigned(FListBox) then
		FListBox.Items.Assign(AValue)
	else
		FItems.Assign(AValue);
end;

procedure TDecksComboBox.SetOnDrawSelectedItem(AValue: TOnAfterRenderBCButton);
begin
	if @FOnDrawSelectedItem = @AValue then Exit;
	FOnDrawSelectedItem := AValue;
	FButton.ShowCaption := not Assigned(AValue);
end;

procedure TDecksComboBox.SetRounding(AValue: TBCRounding);
begin
	Button.Rounding := AValue;
end;

procedure TDecksComboBox.SetStateNormal(AValue: TBCButtonState);
begin
	Button.StateNormal := AValue;
end;

procedure TDecksComboBox.SetStaticButton(AValue: boolean);
begin
	Button.StaticButton := AValue;
end;

function TDecksComboBox.GetBevel: TBCBevel;
begin
	Result := Button.Bevel;
end;

procedure TDecksComboBox.SetBevel(AValue: TBCBevel);
begin
	Button.Bevel := AValue;
end;

function TDecksComboBox.GetStyleExtension: String;
begin
	Result := 'bccombo';
end;

procedure TDecksComboBox.WMSetFocus(var Message: TLMSetFocus);
begin
	UpdateFocus(True);
end;

procedure TDecksComboBox.WMKillFocus(var Message: TLMKillFocus);
begin
	if Message.FocusedWnd <> Handle then
		UpdateFocus(False);
end;

procedure TDecksComboBox.UpdateFocus(AFocused: boolean);
var
	lForm: TCustomForm;
	oldCaption: string;
begin
	lForm := GetParentForm(Self);
	if lForm = nil then Exit;

	if AFocused then
	begin
		SendMessage(lForm.Handle, LM_NCACTIVATE, WPARAM(True), -1);
		ActiveDefaultControlChanged(lForm.ActiveControl);
	end
	else
		ActiveDefaultControlChanged(nil);

	oldCaption := FButton.Caption;
	FButton.Caption := FButton.Caption + '1';
	FButton.Caption := oldCaption;

	Invalidate;
end;

procedure TDecksComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_RETURN:
		begin
			ButtonClick(nil);
		end;
		VK_DOWN:
		begin
			if ItemIndex + 1 < Items.Count then
			begin
				ItemIndex := ItemIndex + 1;
				Button.Caption := GetItemText;
				if Assigned(FOnChange) then
					FOnChange(Self);
			end;
		end;
		VK_UP:
		begin
			if ItemIndex - 1 >= 0 then
			begin
				ItemIndex := ItemIndex - 1;
				Button.Caption := GetItemText;
				if Assigned(FOnChange) then
					FOnChange(Self);
			end;
		end;
	else
		Exit;
	end;
	Key := 0;
end;

procedure TDecksComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
	i: integer;
begin
	for i := 0 to Items.Count - 1 do
	begin
		if (Items[i] <> '') and Items[i].ToLower.StartsWith(LowerCase(UTF8Key)) then
		begin
			if ItemIndex <> i then
			begin
				ItemIndex := i;
				Button.Caption := GetItemText;
				if Assigned(FOnChange) then
					FOnChange(Self);
				Break;
			end;
		end;
	end;
end;

procedure TDecksComboBox.CreateForm;
begin
	if FForm = nil then
	begin
		FForm := TFormPopup.CreateNew(Self);
		FForm.Visible := False;
		FForm.ShowInTaskBar := stNever;
		FForm.BorderStyle := bsNone;
		FForm.OnDeactivate := FormDeactivate;
		FForm.OnHide := FormHide;
		FForm.FormStyle := fsStayOnTop;
		FForm.Parent := nil;
		FForm.PopupParent := GetParentForm(Self);
	end;

	if FListBox = nil then
	begin
		FListBox := TListBox.Create(self);
		FListBox.Parent := FForm;
		FListBox.BorderStyle := bsNone;
		FListBox.OnSelectionChange := ListBoxSelectionChange;
		FListBox.OnMouseLeave := ListBoxMouseLeave;
		FListBox.OnMouseMove := ListBoxMouseMove;
		FListBox.OnMouseUp := ListBoxMouseUp;
		FListBox.Style := lbOwnerDrawFixed;
		FListBox.OnDrawItem := ListBoxDrawItem;
		FListBox.Options := []; // do not draw focus rect
		FListBox.OnKeyDown := ListBoxKeyDown;
		if Assigned(FItems) then
		begin
			FListBox.Items.Assign(FItems);
			FreeAndNil(FItems);
		end;
		FListBox.ItemIndex := FItemIndex;
		FListBox.Color := FDropDownColor;
	end;
end;

procedure TDecksComboBox.FreeForm;
begin
	if Assigned(FListBox) then
	begin
		if FListBox.LCLRefCount > 0 then Exit;
		if FItems = nil then
			FItems := TStringList.Create;
		FItems.Assign(FListBox.Items);
		FItemIndex := FListBox.ItemIndex;
		FDropDownColor := FListBox.Color;
		FreeAndNil(FListBox);
	end;
	FreeAndNil(FForm);
end;

function TDecksComboBox.GetListBox: TListBox;
begin
	CreateForm;
	Result := FListBox;
end;

constructor TDecksComboBox.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	with GetControlClassDefaultSize do
		SetInitialBounds(0, 0, CX, CY);

	FButton := TDecksButton.Create(Self);
	FButton.Align := alClient;
	FButton.Parent := Self;
	FButton.OnClick := ButtonClick;
	FButton.DropDownArrow := True;
	FButton.OnAfterRenderBCButton := OnAfterRenderButton;

	FButton.Bevel.ColorDark  := $666666;
	FButton.Bevel.ColorLight := $FFFFFF;
	FButton.Bevel.InnerBevel := bcbNone;
	FButton.Bevel.OuterBevel := bcbBottom;
	FButton.Bevel.Sides      := [bcsBottom];
	FButton.StateNormal.Background.Color := $444444;

	FItems := TStringList.Create;
	FHoverItem := -1;
	FItemIndex := -1;

	ParentBackground := True;
	DropDownBorderSize := 1;
	DropDownColor := FButton.StateNormal.Background.Color;
	DropDownBorderColor := $222222;
	DropDownCount := 8;
	DropDownFontColor := $CCCCCC;
	DropDownHighlight := $BA8744;
	DropDownFontHighlight := $FFFFFF;
	FocusBorderOpacity := 100;

	FTimerCheckFormHide := TTimer.Create(self);
	FTimerCheckFormHide.Interval := 30;
	FTimerCheckFormHide.OnTimer := OnTimerCheckFormHide;
end;

destructor TDecksComboBox.Destroy;
begin
	FreeAndNil(FItems);
	inherited Destroy;
end;

procedure TDecksComboBox.Assign(Source: TPersistent);
var
	src: TDecksComboBox;
begin
	if Source is TDecksComboBox then
	begin
		src := TDecksComboBox(Source);
		Button.Assign(src.Button);
		Items.Assign(src.Items);
		ItemIndex := src.ItemIndex;
		DropDownBorderColor := src.DropDownBorderColor;
		DropDownBorderSize := src.DropDownBorderSize;
		DropDownColor := src.DropDownColor;
		DropDownFontColor := src.DropDownFontColor;
		DropDownCount := src.DropDownCount;
		DropDownHighlight := src.DropDownHighlight;
		DropDownFontHighlight := src.DropDownFontHighlight;
	end
	else
		inherited Assign(Source);
end;

procedure TDecksComboBox.Clear;
begin
	Items.Clear;
end;

end.
