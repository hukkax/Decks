// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
{ Customizable component which using BGRABitmap for drawing. Control mostly rendered
  using framework.

  Functionality:
  - Gradients
  - Double gradients
  - Rounding
  - Drop down list
  - Glyph
  - States (normal, hover, clicked)
  - Caption with shadow
  - Full alpha and antialias support

  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

- Joel Toivonen (hukka)
  Modified for use in Decks 3 (2020-10-13)

***************************** END CONTRIBUTOR(S) *****************************}

unit DecksButton;

{$MODE Delphi}

interface

uses
	Classes, SysUtils, Graphics, Controls, ExtCtrls, Menus, ImgList,
	LCLType, LResources,
	BGRABitmap, BGRABitmapTypes, BCTypes, BCBasectrls, BCButton;

type
	TBCBevel = class(TBCProperty)
	private
		FOpacity: Byte;
		FVerticalOnly: Boolean;
		procedure SetOpacity(Value: Byte);
		procedure SetVerticalOnly(Value: Boolean);
	public
		constructor Create(AControl: TControl); override;
		procedure   Assign(Source: TPersistent); override;
	published
		property Opacity: Byte read FOpacity write SetOpacity;
		property VerticalOnly: Boolean read FVerticalOnly write SetVerticalOnly;
	end;

	TDecksButton = class(TBCStyleGraphicControl)
	private
		FDropDownArrowSize: integer;
		FDropDownWidth: integer;
		FFlipArrow: boolean;
		FActiveButt: TBCButtonStyle;
		FBGRANormal, FBGRAHover, FBGRAClick: TBGRABitmapEx;
		FGlyphAlignment: TBCAlignment;
		FGlyphOldPlacement: boolean;
		FInnerMargin: single;
		FPreserveGlyphOnAssign: Boolean;
		FRounding: TBCRounding;
		FRoundingDropDown: TBCRounding;
		FStateClicked,
		FStateHover,
		FStateNormal: TBCButtonState;
		FDown: boolean;
		FGlyph: TBitmap;
		FGlyphMargin: integer;
		FButtonState: TBCMouseState;
		FDownButtonState: TBCMouseState;
		FPrevButtonState: TBCMouseState;
		FOnAfterRenderBCButton: TOnAfterRenderBCButton;
		FOnButtonClick: TNotifyEvent;
		FStaticButton: Boolean;
		FStyle: TBCButtonStyle;
		FGlobalOpacity: byte;
		FTextApplyGlobalOpacity: boolean;
		AutoSizeExtraY: integer;
		AutoSizeExtraX: integer;
		FLastBorderWidth: integer;
		FBevel: TBCBevel;
		// MORA
		FClickOffset: boolean;
		FDropDownArrow: boolean;
		FDropDownMenu: TPopupMenu;
		FDropDownMenuVisible: boolean;
		FDropDownClosingTime: TDateTime;
		FDropDownPosition: TBCButtonDropDownPosition;
		FDropDownStyle: TBCButtonDropDownStyle;
		FImageChangeLink: TChangeLink;
		FImageIndex: integer;
		FImages: TCustomImageList;
		FSaveDropDownClosed: TNotifyEvent;
		FShowCaption: boolean;

		function  GetButtonRect: TRect;
		function  GetDropDownWidth(AFull: boolean = True): integer;
		function  GetDropDownRect(AFull: boolean = True): TRect;
		function  GetGlyph: TBitmap;
		procedure AssignDefaultStyle;
		procedure CalculateGlyphSize(out NeededWidth, NeededHeight: integer);
		procedure DropDownClosed(Sender: TObject);
		procedure RenderAll(ANow: boolean = False);
		procedure SetBCButtonStateClicked(const AValue: TBCButtonState);
		procedure SetBCButtonStateHover(const AValue: TBCButtonState);
		procedure SetBCButtonStateNormal(const AValue: TBCButtonState);
		procedure SetClickOffset(AValue: boolean);
		procedure SetDown(AValue: boolean);
		procedure SetDropDownArrow(AValue: boolean);
		procedure SetDropDownArrowSize(AValue: integer);
		procedure SetDropDownPosition(AValue: TBCButtonDropDownPosition);
		procedure SetDropDownWidth(AValue: integer);
		procedure SetFlipArrow(AValue: boolean);
		procedure SetGlyph(const AValue: TBitmap);
		procedure SetGlyphAlignment(AValue: TBCAlignment);
		procedure SetGlyphMargin(const AValue: integer);
		procedure SetGlyphOldPlacement(AValue: boolean);
		procedure SetImageIndex(AValue: integer);
		procedure SetImages(AValue: TCustomImageList);
		procedure SetInnerMargin(AValue: single);
		procedure SetRounding(AValue: TBCRounding);
		procedure SetRoundingDropDown(AValue: TBCRounding);
		procedure SetShowCaption(AValue: boolean);
		procedure SetStaticButton(const AValue: boolean);
		procedure SetStyle(const AValue: TBCButtonStyle);
		procedure SetGlobalOpacity(const AValue: byte);
		procedure SetTextApplyGlobalOpacity(const AValue: boolean);
		procedure UpdateSize;
		procedure OnChangeGlyph({%H-}Sender: TObject);
		procedure OnChangeState({%H-}Sender: TObject; AData: PtrInt);
		procedure ImageListChange(ASender: TObject);
		procedure SetBevel(Value: TBCBevel);
	protected
		class function GetControlClassDefaultSize: TSize; override;
		procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
			{%H-}WithThemeSpace: boolean); override;
		procedure Click; override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
		procedure MouseEnter;  override;
		procedure MouseLeave;  override;
		procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
		procedure SetEnabled(Value: boolean); override;
		procedure TextChanged;  override;

		procedure ActionChange(Sender: TObject; CheckDefaults: boolean); override;
		function  GetActionLinkClass: TControlActionLinkClass; override;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
		procedure Render(ABGRA: TBGRABitmapEx; AState: TBCButtonState); virtual;
		procedure RenderState(ABGRA: TBGRABitmapEx; AState: TBCButtonState;
			const ARect: TRect; ARounding: TBCRounding); virtual;

		function  GetStyleExtension: string; override;
		procedure DrawControl; override;
		procedure RenderControl; override;

		property AutoSizeExtraVertical: integer read AutoSizeExtraY;
		property AutoSizeExtraHorizontal: integer read AutoSizeExtraX;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		procedure Assign(Source: TPersistent); override;
		{ Set dropdown size and autosize extra padding }
		procedure SetSizeVariables(newDropDownWidth, newDropDownArrowSize,
			newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
		{ Called by EndUpdate }
		procedure UpdateControl; override;
		{ Load and assign all published settings from file }
		procedure LoadFromFile(AFileName: string); override;
		{ Save all published settings to file }
		procedure SaveToFile(AFileName: string); override;
		{ Assign the properties from AFileName to this instance }
		procedure AssignFromFile(AFileName: string); override;
		{ Used by SaveToFile/LoadFromFile }
		procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
			var ComponentClass: TComponentClass);

		procedure SetMouseDown(Button: TMouseButton; ButtonDown: Boolean; InvokeCallback: Boolean = True);
	published
		property Action;
		property Align;
		property Anchors;
		{ Click to edit the style. Available when editing only. If you want to stream the style from a file at runtime please use LoadFromFile and SaveToFile methods. }
		property AssignStyle;
		property AutoSize;
		property StateNormal: TBCButtonState read FStateNormal write SeTBCButtonStateNormal;
//		property StateHover: TBCButtonState read FStateHover write SetBCButtonStateHover;
//		property StateClicked: TBCButtonState read FStateClicked write SetBCButtonStateClicked;
		property BorderSpacing;
		property Caption;
		property Color;
		property Constraints;
		{ Set to True to change the button to always show a StateClicked style that will not change when button is clicked or hovered. }
		property Down: boolean read FDown write SetDown default False;
		{ The width of the dropdown arrow area. }
		property DropDownWidth: integer read FDropDownWidth write SetDropDownWidth;
		{ The size of the dropdown arrow. }
		property DropDownArrowSize: integer read FDropDownArrowSize write SetDropDownArrowSize;
		property Enabled;
		{ Changes the direction of the arrow. Default: False. }
		property FlipArrow: boolean read FFlipArrow write SetFlipArrow default False;
		{ Set the opacity that will be applied to the whole button. Default: 255. }
		property GlobalOpacity: Byte read FGlobalOpacity write SetGlobalOpacity;
		{ The glyph icon. }
		property Glyph: TBitmap read GetGlyph write SetGlyph;
		property GlyphAlignment: TBCAlignment read FGlyphAlignment write SetGlyphAlignment default bcaCenter;
		property GlyphOldPlacement: boolean read FGlyphOldPlacement write SetGlyphOldPlacement default true;
		property PreserveGlyphOnAssign: boolean read FPreserveGlyphOnAssign write FPreserveGlyphOnAssign default True;
		{ The margin of the glyph icon. }
		property GlyphMargin: integer read FGlyphMargin write SetGlyphMargin default 5;
		property Hint;
		property InnerMargin: single read FInnerMargin write SetInnerMargin;
		{ Called when the button finish the render. Use it to add your own drawings to the button. }
		property OnAfterRenderBCButton: TOnAfterRenderBCButton read FOnAfterRenderBCButton write FOnAfterRenderBCButton;
		{ Called when the button part is clicked, not the dropdown. }
		property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
		property OnClick;
		property OnDblClick;
		property OnMouseDown;
		property OnMouseEnter;
		property OnMouseLeave;
		property OnMouseMove;
		property OnMouseUp;
		property OnMouseWheel;
		property OnMouseWheelDown;
		property OnMouseWheelUp;
		property ParentColor;
		property PopupMenu;
		{ Change the style of the rounded corners of the button. }
		property Rounding: TBCRounding read FRounding write SetRounding;
		{ Change the style of the rounded corners of the dropdown part of the button. }
		property RoundingDropDown: TBCRounding read FRoundingDropDown write SetRoundingDropDown;
		{ Set to True to change the button to always show a StateNormal style that will not change when button is clicked or hovered. }
		property StaticButton: Boolean read FStaticButton write SetStaticButton default False;
		property ShowHint;
		{ The style of button that will be used. bbtButton or bbtDropDown. }
		property Style: TBCButtonStyle read FStyle write SetStyle default bbtButton;
		{ Opacity of extra highlight/shadow lines at top/bottom }
		property Bevel: TBCBevel read FBevel write SetBevel;
		{ Apply the global opacity to rendered text. Default: False. }
		property TextApplyGlobalOpacity: Boolean read FTextApplyGlobalOpacity write SetTextApplyGlobalOpacity;
		property Visible;
		{ -ToDo: Unused property? }
		property ClickOffset: boolean read FClickOffset write SetClickOffset default False;
		{ Show the dropdown arrow. }
		property DropDownArrow: Boolean read FDropDownArrow write SetDropDownArrow default False;
		{ The dropdown menu that will be displayed when the button is pressed. }
		property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
		{ The kind of dropdown that will be used. bdsSeparate will show the dropdown down the dropdown arrow side. bdsCommon will show the dropdown down the whole button. }
		property DropDownStyle: TBCButtonDropDownStyle read FDropDownStyle write FDropDownStyle default bdsSeparate;
		{ The position of the dropdown arrow. }
		property DropDownPosition: TBCButtonDropDownPosition read FDropDownPosition write SetDropDownPosition default bdpLeft;
		{ The image list that holds an image to be used with the button ImageIndex property. }
		property Images: TCustomImageList read FImages write SetImages;
		{ The index of the image that will be used for the button as glyph icon if glyph property is not set. }
		property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
		{ Show caption or hides it. Default: True. }
		property ShowCaption: boolean read FShowCaption write SetShowCaption default True;
		{ The unique name of the control in the form. }
		property Name;
	end;

	procedure Register;

implementation

uses
	Math,
	ActnList,
	BCTools;

const
	DropDownReopenDelay = 0.2/(24*60*60);
	HoverLight = 1500;


function Lighten(C: TColor; Light: Integer): TColor;
var
	P: THSLAPixel;
begin
	P := C;
	P.lightness := Min(65535, P.lightness + Light);
	Result := P.ToColor;
end;

// stupid, but TBCFont.Assign is missing these assignments and I wasn't
// able to override the Assign method
procedure AssignFont(Dest, Source: TBCFont);
begin
	Dest.PaddingTop := Source.PaddingTop;
	Dest.PaddingLeft := Source.PaddingLeft;
	Dest.PaddingBottom := Source.PaddingBottom;
	Dest.PaddingRight := Source.PaddingRight;
end;

{ TDecksButton }

procedure Register;
begin
	RegisterComponents('Custom', [TDecksButton]);
end;

{ TBCBevel }

procedure TBCBevel.SetOpacity(Value: Byte);
begin
	if FOpacity = Value then Exit;
	FOpacity := Value;
	Change;
end;

procedure TBCBevel.SetVerticalOnly(Value: Boolean);
begin
	if FVerticalOnly = Value then Exit;
	FVerticalOnly := Value;
	Change;
end;

constructor TBCBevel.Create(AControl: TControl);
begin
	inherited Create(AControl);
	FOpacity := 50;
	FVerticalOnly := True;
end;

procedure TBCBevel.Assign(Source: TPersistent);
begin
	if Source is TBCBevel then
	begin
		FOpacity := TBCBevel(Source).FOpacity;
		FVerticalOnly := TBCBevel(Source).FVerticalOnly;
	end
	else
		inherited Assign(Source);
end;

procedure TDecksButton.AssignDefaultStyle;
begin
	FRounding.RoundX := 0;
	FRounding.RoundY := 0;
	FBevel.Opacity := 50;
	FBevel.VerticalOnly := True;
	with FStateNormal do
	begin
		FontEx.Color := 11775917;
		FontEx.FontQuality := fqSystemClearType;
		//FontEx.Height := 32;
		FontEx.Shadow := True;
		FontEx.ShadowColorOpacity := 160;
		FontEx.ShadowRadius := 2;
		FontEx.ShadowOffsetX := 1;
		FontEx.ShadowOffsetY := 1;
		FontEx.Style := [fsBold];
		FontEx.PaddingLeft := 0;
		FontEx.PaddingTop  := 0;

		Border.Color := 2302497;
		Border.LightColor := 3529055;
		Border.Style := bboSolid;

		Background.Color := 6249562;
		Background.Gradient1EndPercent := 100; // custom drawing mode
		Background.Style := bbsGradient;

		with Background.Gradient1 do
		begin
			StartColor := 6249562;
			EndColor := 4210494;
			ColorCorrection := False;
			GradientType := gtLinear;
			Point1XPercent := 0;
			Point1YPercent := 0;
			Point2XPercent := 0;
			Point2YPercent := 100;
		end;
		// Gradient2
		with Background.Gradient2 do
		begin
			StartColor := $808080;
			EndColor := $202020;
			ColorCorrection := False;
			GradientType := gtLinear;
			Point1XPercent := 0;
			Point1YPercent := 0;
			Point2XPercent := 0;
			Point2YPercent := 100;
		end;
	end;
	SetBCButtonStateHover(FStateNormal);
	SetBCButtonStateClicked(FStateNormal);
end;

procedure TDecksButton.CalculateGlyphSize(out NeededWidth, NeededHeight: integer);
begin
  if Assigned(FGlyph) and not FGlyph.Empty then
  begin
    NeededWidth := FGlyph.Width;
    NeededHeight := FGlyph.Height;
  end
  else
  if Assigned(FImages) then
  begin
    NeededWidth := FImages.Width;
    NeededHeight := FImages.Height;
  end
  else
  begin
    NeededHeight := 0;
    NeededWidth := 0;
  end;
end;

procedure TDecksButton.RenderAll(ANow: boolean);
begin
  if (csCreating in ControlState) or IsUpdating or (FBGRANormal = nil) then Exit;

  if ANow then
  begin
    Render(FBGRANormal, FStateNormal);
    Render(FBGRAHover,  FStateHover);
    Render(FBGRAClick,  FStateClicked);
  end
  else
  begin
    FBGRANormal.NeedRender := True;
    FBGRAHover.NeedRender  := True;
    FBGRAClick.NeedRender  := True;
  end;
end;

function TDecksButton.GetButtonRect: TRect;
begin
  Result := GetClientRect;
  if FStyle = bbtDropDown then
    case FDropDownPosition of
      bdpBottom:
        Dec(Result.Bottom, GetDropDownWidth(False));
      else
        // bdpLeft:
        Dec(Result.Right, GetDropDownWidth(False));
    end;
end;

function TDecksButton.GetDropDownWidth(AFull: boolean): integer;
begin
  Result := FDropDownWidth + (ifthen(AFull, 2, 1) * FStateNormal.Border.Width);
end;

function TDecksButton.GetGlyph: TBitmap;
begin
  Result := FGlyph as TBitmap;
end;

procedure TDecksButton.SetBevel(Value: TBCBevel);
begin
	if FBevel = Value then Exit;
	FBevel.Assign(Value);
	RenderControl;
	Invalidate;
end;

function TDecksButton.GetDropDownRect(AFull: boolean): TRect;
begin
  Result := GetClientRect;
  case FDropDownPosition of
    bdpBottom:
      Result.Top := Result.Bottom - GetDropDownWidth(AFull);
    else
      // bdpLeft:
      Result.Left := Result.Right - GetDropDownWidth(AFull);
  end;
end;

procedure TDecksButton.Render(ABGRA: TBGRABitmapEx; AState: TBCButtonState);

  function GetActualGlyph: TBitmap;
  begin
    if Assigned(FGlyph) and not FGlyph.Empty then result := FGlyph else
    if Assigned(FImages) and (FImageIndex > -1) and (FImageIndex < FImages.Count) then
    begin
      result := TBitmap.Create;
      {$IFDEF FPC}
      FImages.GetBitmap(FImageIndex, result);
      {$ELSE}
      FImages.GetBitmapRaw(FImageIndex, result);
      {$ENDIF}
    end else exit(nil);
  end;

  procedure RenderGlyph(ARect: TRect; AGlyph: TBitmap);
  begin
    if ARect.IsEmpty or (AGlyph = nil) then exit;
    ABGRA.PutImage(ARect.Left, ARect.Top, AGlyph, dmLinearBlend);
  end;

var
  r, r_a, r_g: TRect;
  g: TBitmap;
  actualCaption: TCaption;

begin
  if (csCreating in ControlState) or IsUpdating then
    Exit;

  ABGRA.NeedRender := False;

  { Refreshing size }
  ABGRA.SetSize(Width, Height);

  { Clearing previous paint }
  ABGRA.Fill(BGRAPixelTransparent);

  { Basic body }
  r := GetButtonRect;

  if (FClickOffset) and (FButtonState = msClicked) then
    r.Top := r.Top + 1;

  RenderState(ABGRA, AState, r, FRounding);

  if not GlyphOldPlacement then
    r.Inflate(-round(InnerMargin),-round(InnerMargin));

  { Calculating rect }
  CalculateBorderRect(AState.Border, r);

  if FStyle = bbtDropDown then
  begin
    r_a := GetDropDownRect;
    RenderState(ABGRA, AState, r_a, FRoundingDropDown);
    CalculateBorderRect(AState.Border, r_a);

    // Click offset for arrow
    if (FClickOffset) and (FButtonState = msClicked) then //(AState = FStateClicked) then
      r_a.Offset(1,1);

    if FFlipArrow then
      RenderArrow(TBGRABitmap(ABGRA), r_a, FDropDownArrowSize, badUp,
        AState.FontEx.Color)
    else
      RenderArrow(TBGRABitmap(ABGRA), r_a, FDropDownArrowSize, badDown,
        AState.FontEx.Color);
  end;

  // Click offset for text and glyph
//  if (FClickOffset) and (FButtonState = msClicked) then //(AState = FStateClicked) then
//    r.Offset(0, 1);

  // DropDown arrow
  if FDropDownArrow and (FStyle <> bbtDropDown) then
  begin
    r_a := r;
    r_a.Left := r_a.Right - FDropDownWidth;
    if FFlipArrow then
      RenderArrow(TBGRABitmap(ABGRA), r_a, FDropDownArrowSize, badUp,
        AState.FontEx.Color)
    else
      RenderArrow(TBGRABitmap(ABGRA), r_a, FDropDownArrowSize, badDown,
        AState.FontEx.Color);
    Dec(R.Right, FDropDownWidth);
  end;

  g := GetActualGlyph;
  if FShowCaption then actualCaption := self.Caption else actualCaption := '';
  r_g := ComputeGlyphPosition(r, g, GlyphAlignment, GlyphMargin, actualCaption, AState.FontEx, GlyphOldPlacement);
  if FTextApplyGlobalOpacity then
  begin
    { Drawing text }
    RenderText(r, AState.FontEx, actualCaption, ABGRA);
    RenderGlyph(r_g, g);
    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
  end
  else
  begin
    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
    { Drawing text }
    RenderText(r, AState.FontEx, actualCaption, ABGRA);
    RenderGlyph(r_g, g);
  end;
  if g <> FGlyph then g.Free;

  { Convert to gray if not enabled }
  if not Enabled then ABGRA.InplaceGrayscale;

  if Assigned(FOnAfterRenderBCButton) then
    FOnAfterRenderBCButton(Self, ABGRA, AState, r);

  {$IFDEF INDEBUG}
  FRenderCount := FRenderCount +1;
  {$ENDIF}
end;

procedure TDecksButton.RenderState(ABGRA: TBGRABitmapEx;
  AState: TBCButtonState; const ARect: TRect; ARounding: TBCRounding);
var
	PaddingX, PaddingY: Integer;
	w: Single;
	BG: TBCBackground;
	Bor: TBCBorder;
	ColBri, ColDark: TBGRAPixel;
begin
	BG := AState.Background;
	Bor := AState.Border;

	if Bor.Style = bboNone then
	begin
		w := FInnerMargin - 0.5;
		RenderBackgroundF(ARect.Left+w, ARect.Top+w, ARect.Right-1-w,
			ARect.Bottom-1-w, BG, ABGRA, ARounding);
	end
	else
	begin
		w := (Bor.Width-1) / 2 + FInnerMargin;
		RenderBackgroundF(ARect.Left+w, ARect.Top+w, ARect.Right-1-w, ARect.Bottom-1-w,
			BG, ABGRA, ARounding);
	end;

	if FBevel.Opacity > 0 then
	begin
		PaddingY := Bor.Width + Bor.LightWidth;
		PaddingX := Trunc(PaddingY + (Rounding.RoundX * 0.5));

		// bright bevel
		ColBri := BGRA(255, 255, 255, FBevel.Opacity);
		// top
		ABGRA.FastBlendHorizLine(
			ARect.Left+PaddingX, ARect.Top+PaddingY, ARect.Right-1-PaddingX, ColBri);

		// dark bevel
		ColDark := BGRA(0, 0, 0, FBevel.Opacity);
		// bottom
		ABGRA.FastBlendHorizLine(
			ARect.Left+PaddingX, ARect.Bottom-1-PaddingY, ARect.Right-1-PaddingX, ColDark);

		if not Bevel.VerticalOnly then
		begin
			PaddingX := PaddingY;
			PaddingY := Trunc(PaddingX + (Rounding.RoundY * 0.5));
			// left: bright
			ABGRA.FastBlendVertLine(
				ARect.Left+PaddingX, ARect.Top+PaddingY, ARect.Bottom-1-PaddingY, ColBri);
			// right: dark
			ABGRA.FastBlendVertLine(
				ARect.Right-1-PaddingX, ARect.Top+PaddingY, ARect.Bottom-1-PaddingY, ColDark);
		end;
	end;

	if Bor.Style <> bboNone then
	begin
		RenderBorderF(ARect.Left+w, ARect.Top+w, ARect.Right-1-w, ARect.Bottom-1-w,
			Bor, ABGRA, ARounding);
	end;
end;

procedure TDecksButton.OnChangeGlyph(Sender: TObject);
begin
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.OnChangeState(Sender: TObject; AData: PtrInt);
begin
  RenderControl;
  if (TBCButtonPropertyData(AData) = pdUpdateSize) or
    (FStateNormal.Border.Width <> FLastBorderWidth) then
    UpdateSize;
  Invalidate;
end;

procedure TDecksButton.ImageListChange(ASender: TObject);
begin
  if ASender = Images then
  begin
    RenderControl;
    Invalidate;
  end;
end;

procedure TDecksButton.SetBCButtonStateClicked(const AValue: TBCButtonState);
begin
	FStateClicked.Assign(AValue);
	AssignFont(FStateClicked.FontEx, AValue.FontEx);
end;

procedure TDecksButton.SetBCButtonStateHover(const AValue: TBCButtonState);
begin
	FStateHover.Assign(AValue);
	AssignFont(FStateHover.FontEx, AValue.FontEx);

	with FStateHover.Background do
	begin
		Color := Lighten(Color, HoverLight);
	end;
	with FStateHover.Background.Gradient1 do
	begin
		Color := Lighten(Color, HoverLight);
		StartColor := Lighten(StartColor, HoverLight);
		EndColor := Lighten(EndColor, HoverLight);
	end;
	with FStateHover.Background.Gradient2 do
	begin
		Color := Lighten(Color, HoverLight);
		StartColor := Lighten(StartColor, HoverLight);
		EndColor := Lighten(EndColor, HoverLight);
	end;
end;

procedure TDecksButton.SetBCButtonStateNormal(const AValue: TBCButtonState);
begin
	if FStateNormal = AValue then Exit;
	FStateNormal.Assign(AValue);
	SetBCButtonStateHover(AValue);
	SetBCButtonStateClicked(AValue);
	RenderControl;
	Invalidate;
end;

procedure TDecksButton.SetClickOffset(AValue: boolean);
begin
  if FClickOffset = AValue then
    Exit;
  FClickOffset := AValue;
  RenderControl;
end;

procedure TDecksButton.SetDown(AValue: boolean);
begin
  if FDown = AValue then
    exit;
  FDown := AValue;
  if FDown then
    FButtonState := msClicked
  else
    FButtonState := msNone;
  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetDropDownArrow(AValue: boolean);
begin
  if FDropDownArrow = AValue then
    Exit;
  FDropDownArrow := AValue;
  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetDropDownArrowSize(AValue: integer);
begin
  if FDropDownArrowSize = AValue then
    Exit;
  FDropDownArrowSize := AValue;

  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetDropDownPosition(AValue: TBCButtonDropDownPosition);
begin
  if FDropDownPosition = AValue then
    Exit;
  FDropDownPosition := AValue;

  if FStyle <> bbtDropDown then
    Exit;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetDropDownWidth(AValue: integer);
begin
  if FDropDownWidth = AValue then
    Exit;
  FDropDownWidth := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetFlipArrow(AValue: boolean);
begin
  if FFlipArrow = AValue then
    Exit;
  FFlipArrow := AValue;

  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetGlyph(const AValue: TBitmap);
begin
  if (FGlyph <> nil) and (FGlyph = AValue) then
    exit;

  FGlyph.Assign(AValue);

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetGlyphAlignment(AValue: TBCAlignment);
begin
  if FGlyphAlignment=AValue then Exit;
  FGlyphAlignment:=AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetGlyphMargin(const AValue: integer);
begin
  if FGlyphMargin = AValue then
    exit;
  FGlyphMargin := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetGlyphOldPlacement(AValue: boolean);
begin
  if FGlyphOldPlacement=AValue then Exit;
  FGlyphOldPlacement:=AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetImageIndex(AValue: integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  FImages := AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetInnerMargin(AValue: single);
begin
  if FInnerMargin=AValue then Exit;
  FInnerMargin:=AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then
    Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetRoundingDropDown(AValue: TBCRounding);
begin
  if FRoundingDropDown = AValue then
    Exit;
  FRoundingDropDown.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetShowCaption(AValue: boolean);
begin
  if FShowCaption = AValue then
    Exit;
  FShowCaption := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.SetStaticButton(const AValue: boolean);
begin
  if FStaticButton = AValue then
    exit;
  FStaticButton := AValue;

  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetStyle(const AValue: TBCButtonStyle);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.UpdateSize;
begin
	InvalidatePreferredSize;
	AdjustSize;
	SetBCButtonStateHover(FStateNormal);
	SetBCButtonStateClicked(FStateNormal);
end;

procedure TDecksButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
//  AWidth: integer;
  gh,gw: integer;
  actualCaption: TCaption;
  horizAlign, relHorizAlign: TAlignment;
  vertAlign, relVertAlign: TTextLayout;
  glyphHorzMargin, glyphVertMargin: integer;
  tw, th, availW: integer;
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then
    Exit;

  FLastBorderWidth := FStateNormal.Border.Width;
  CalculateGlyphSize(gw, gh);

  if GlyphOldPlacement then
  begin
    {  if WidthIsAnchored then
        AWidth := Width
      else
        AWidth := 10000;}

    PreferredWidth := 0;
    PreferredHeight := 0;
    if FShowCaption then
      CalculateTextSize(Caption, FStateNormal.FontEx, PreferredWidth, PreferredHeight);

    // Extra pixels for DropDown
    if Style = bbtDropDown then
      if FDropDownPosition in [bdpBottom] then
        Inc(PreferredHeight, GetDropDownWidth)
      else
        Inc(PreferredWidth, GetDropDownWidth);

    if (Style = bbtButton) and FDropDownArrow then
      Inc(PreferredWidth, FDropDownArrowSize);// GetDropDownWidth);


    //if (FGlyph <> nil) and (not FGlyph.Empty) then
    if (gw > 0) and (gh > 0) then
    begin
      //if Caption = '' then
      if PreferredWidth = 0 then
      begin
        Inc(PreferredWidth, gw{ - AutoSizeExtraY * 2});
        Inc(PreferredHeight, gh);
      end
      else
      begin
        Inc(PreferredWidth, gw + FGlyphMargin);
        if gh > PreferredHeight then
          PreferredHeight := gh;
      end;
    end;

    // Extra pixels for AutoSize
    Inc(PreferredWidth, AutoSizeExtraX);
    Inc(PreferredHeight, AutoSizeExtraY);
  end else
  begin
    if ShowCaption then actualCaption := Caption else actualCaption := '';
    PreferredWidth := round(InnerMargin);
    PreferredHeight := round(InnerMargin);
    case FStyle of
    bbtDropDown:
      case FDropDownPosition of
        bdpBottom: inc(PreferredHeight, GetDropDownWidth(False));
        else{bdpLeft} inc(PreferredWidth, GetDropDownWidth(False));
      end;
    else{bbtButton} if FDropDownArrow then
      inc(PreferredWidth, FDropDownWidth);
    end;
    inc(PreferredWidth, FStateNormal.Border.Width);
    inc(PreferredHeight, FStateNormal.Border.Width);

    if actualCaption='' then
    begin
      inc(PreferredWidth,gw);
      inc(PreferredHeight,gh);
      if gw>0 then inc(PreferredWidth, GlyphMargin*2);
      if gh>0 then inc(PreferredHeight, GlyphMargin*2);
    end else
    begin
      GetGlyphActualLayout(actualCaption, FStateNormal.FontEx, GlyphAlignment, GlyphMargin,
        horizAlign, vertAlign, relHorizAlign, relVertAlign, glyphHorzMargin, glyphVertMargin);
      availW := 65535;
      if (Align in [alTop,alBottom]) and (Parent <> nil) then
        availW := Parent.ClientWidth - PreferredWidth;
      CalculateTextSizeEx(actualCaption, FStateNormal.FontEx, tw, th, availW);
      if (tw<>0) and FStateNormal.FontEx.WordBreak then inc(tw);
      if vertAlign<>relVertAlign then
      begin
        inc(PreferredWidth,  max(gw+2*GlyphMargin,tw));
        inc(PreferredHeight, GlyphMargin+gh+th);
      end
      else
      begin
        inc(PreferredWidth,  GlyphMargin+gw+tw);
        inc(PreferredHeight, max(gh+2*GlyphMargin,th));
      end;
    end;
  end;
end;

class function TDecksButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 123;
  Result.CY := 33;
end;

procedure TDecksButton.Click;
begin
  if Assigned(FOnButtonClick) and ((FActiveButt = bbtDropDown) or (FStyle = bbtButton)) then
  begin
    FOnButtonClick(Self);
    Exit;
  end;
  inherited Click;
end;

procedure TDecksButton.DropDownClosed(Sender: TObject);
begin
  if Assigned(FSaveDropDownClosed) then
    FSaveDropDownClosed(Sender);
  {$IFDEF FPC}//#
  if Assigned(FDropDownMenu) then
    FDropDownMenu.OnClose := FSaveDropDownClosed;
  {$ENDIF}

  FDropDownMenuVisible := False;
  FDropDownClosingTime := Now;
end;

procedure TDecksButton.SetMouseDown(Button: TMouseButton; ButtonDown: Boolean; InvokeCallback: Boolean = True);
begin
	if not Enabled then Exit;

	if ButtonDown then
	begin
		if FButtonState = msClicked then Exit;
		if (InvokeCallback) and (Assigned(OnMouseDown)) then
			OnMouseDown(Self, Button, [], 0, 0);

		FPrevButtonState := FButtonState;
		FButtonState := msClicked;
		if FDropDownStyle = bdsCommon then
			FDownButtonState := msClicked
		else
			FDownButtonState := msNone;
	end
	else
	begin
		if FButtonState <> msClicked then Exit;
		if (InvokeCallback) and (Assigned(OnMouseUp)) then
			OnMouseUp(Self, Button, [], 0, 0);

		FButtonState := FPrevButtonState;
		if FDropDownStyle = bdsCommon then
			FDownButtonState := msHover
		else
			FDownButtonState := msNone;
	end;

	Invalidate;
end;

procedure TDecksButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
	ClientToScreenPoint : TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then Exit;

  if {(Button = mbLeft) and} Enabled {and (not (FButtonState = msClicked)) } then
  begin
    case FActiveButt of
      bbtButton:
		SetMouseDown(Button, True, False);
      bbtDropDown:
        if not (FDownButtonState = msClicked) then
        begin
          if FDropDownStyle = bdsCommon then
            FButtonState := msClicked
          else
            FButtonState := msNone;
          FDownButtonState := msClicked;
          Invalidate;
        end;
    end;
    // Old
    {FButtonState := msClicked;
    Invalidate;}

    // MORA: Show DropDown menu
    if FDropDownMenuVisible or (Now < FDropDownClosingTime+DropDownReopenDelay) then
      FDropDownMenuVisible := False // Prevent redropping
    else
    if ((FActiveButt = bbtDropDown) or (FStyle = bbtButton)) and
      (FDropDownMenu <> nil) and Enabled then
    begin
      ClientToScreenPoint := ClientToScreen(Point(0, Height));
      with ClientToScreenPoint do
      begin
        // normal button
        if FStyle = bbtButton then
        begin
          x := x + Width * integer(FDropDownMenu.Alignment = paRight);
          if FFlipArrow then
            y := y -Height;
        end
        else
          // dropdown button
        begin
          if FDropDownPosition = bdpBottom then
          begin
            x := x + Width * integer(FDropDownMenu.Alignment = paRight);
            if FFlipArrow then
              y := y -(FDropDownWidth + (FStateNormal.Border.Width * 2));
          end
          else
          begin
            if FFlipArrow then
              y := y -Height;
            if FDropDownStyle = bdsSeparate then
              x := x + Width - (FDropDownWidth + (FStateNormal.Border.Width * 2)) *
                integer(FDropDownMenu.Alignment <> paRight)
            else
              x := x + Width * integer(FDropDownMenu.Alignment = paRight);
          end;
        end;

        FDropDownMenuVisible := True;
        {$IFDEF FPC}//#
        FSaveDropDownClosed := FDropDownMenu.OnClose;
        FDropDownMenu.OnClose := DropDownClosed;
        {$ENDIF}
        FDropDownMenu.PopUp(x, y);
      end;
    end;
  end;
end;

procedure TDecksButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
{var
  p: TPoint;}
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then Exit;

  if {(Button = mbLeft) and} Enabled {and (FButtonState = msClicked)} then
  begin
    case FActiveButt of
      bbtButton:
		SetMouseDown(Button, False, False);
      bbtDropDown:
        if FDownButtonState = msClicked then
        begin
          FDownButtonState := msHover;
          if FDropDownStyle = bdsCommon then
            FButtonState := msHover
          else
            FButtonState := msNone;
          Invalidate;
        end;
    end;
    // Old
    {FButtonState := msHover;
    Invalidate;}
  end;

  //if (FActiveButt = bbtDropDown) and (PopupMenu <> nil) and Enabled then
  //begin
  //  if FFlipArrow then
  //    p := ClientToScreen(Point(Width - FDropDownWidth - (FStateNormal.FBorder.Width * 2),
  //      {PopupMenu.Height} -1))
  //  else
  //    p := ClientToScreen(Point(Width - FDropDownWidth - (FStateNormal.FBorder.Width * 2), Height + 1));

  //  PopupMenu.PopUp(p.x, p.y);
  //end;
end;

procedure TDecksButton.MouseEnter;
begin
  if csDesigning in ComponentState then
    exit;
  case FActiveButt of
    bbtButton:
    begin
      if FDown then
        FButtonState := msClicked
      else
        FButtonState := msHover;

      if FDropDownStyle = bdsSeparate then
        FDownButtonState := msNone
      else
        FDownButtonState := msHover;
    end;
    bbtDropDown:
    begin
      if FDown then
        FButtonState := msClicked
      else
      if FDropDownStyle = bdsSeparate then
        FButtonState := msNone
      else
        FButtonState := msHover;
      FDownButtonState := msHover;
    end;
  end;
  Invalidate;
  // Old
  {FButtonState := msHover;
  Invalidate;}
  inherited MouseEnter;
end;

procedure TDecksButton.MouseLeave;
begin
  if csDesigning in ComponentState then
    exit;
  if FDown then
  begin
    FButtonState := msClicked;
    FActiveButt := bbtButton;
  end
  else
    FButtonState := msNone;
  FDownButtonState := msNone;
  Invalidate;
  inherited MouseLeave;
end;

procedure TDecksButton.MouseMove(Shift: TShiftState; X, Y: integer);

  function IsOverDropDown: boolean;
  begin
    with GetButtonRect do
      case FDropDownPosition of
        bdpBottom:
          Result := Y > Bottom;
        else
          Result := X > GetButtonRect.Right;
      end;
  end;

begin
  inherited MouseMove(Shift, X, Y);

  if FStyle = bbtButton then
    FActiveButt := bbtButton
  else
  begin
    // Calling invalidate only when active button changed. Otherwise, we leave
    // this for LCL. This reduce paint call
    if (FActiveButt = bbtButton) and IsOverDropDown then
    begin
      FActiveButt := bbtDropDown;
      if FDropDownStyle <> bdsCommon then // Don't need invalidating
      begin
        FDownButtonState := msHover;
        if FDown then
          FButtonState := msClicked
        else
          FButtonState := msNone;
        Invalidate;
      end;
    end
    else
    if (FActiveButt = bbtDropDown) and not IsOverDropDown then
    begin
      FActiveButt := bbtButton;
      if FDropDownStyle <> bdsCommon then // Don't need invalidating
      begin
        if FDown then
          FButtonState := msClicked
        else
          FButtonState := msHover;
        FDownButtonState := msNone;
        Invalidate;
      end;
    end;
  end;
end;

procedure TDecksButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);

  RenderControl;
  Invalidate;
end;

procedure TDecksButton.TextChanged;
begin
  inherited TextChanged;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksButton.ActionChange(Sender: TObject; CheckDefaults: boolean);
var
  NewAction: TCustomAction;
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    NewAction := TCustomAction(Sender);
    if (not CheckDefaults) or (not Down) then
      Down := NewAction.Checked;
    if (not CheckDefaults) or (ImageIndex < 0) then
      ImageIndex := NewAction.ImageIndex;
  end;
end;

function TDecksButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TBCButtonActionLink;
end;

procedure TDecksButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FImages) and (Operation = opRemove) then
    Images := nil;
end;

procedure TDecksButton.UpdateControl;
begin
  RenderControl;
  inherited UpdateControl; // indalidate
end;
{$IFDEF FPC}//#
procedure TDecksButton.SaveToFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    WriteComponentAsTextToStream(AStream, Self);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure TDecksButton.LoadFromFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(Self), OnFindClass);
  finally
    AStream.Free;
  end;
end;

procedure TDecksButton.AssignFromFile(AFileName: string);
var
  AStream: TMemoryStream;
  AButton: TBCButton;
begin
  AButton := TBCButton.Create(nil);
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(AButton), OnFindClass);
    Assign(AButton);
  finally
    AStream.Free;
    AButton.Free;
  end;
end;
{$ENDIF}

procedure TDecksButton.OnFindClass(Reader: TReader; const AClassName: string;
	var ComponentClass: TComponentClass);
begin
	if CompareText(AClassName, 'TDecksButton') = 0 then ComponentClass := TDecksButton;
end;

procedure TDecksButton.DrawControl;
var
  bgra: TBGRABitmapEx;
begin
  // If style is without dropdown button or state of each button
  // is the same (possible only for msNone) or static button then
  // we can draw whole BGRABitmap
  if (FStyle = bbtButton) or (FButtonState = FDownButtonState) or FStaticButton then
  begin
    // Main button
    if FStaticButton then
      bgra := FBGRANormal
    else
    if FDown then
      bgra := FBGRAClick
    else
      case FButtonState of
        msNone: bgra := FBGRANormal;
        msHover: bgra := FBGRAHover;
        msClicked: bgra := FBGRAClick;
      end;
    if {%H-}bgra.NeedRender then
      Render(bgra, TBCButtonState(bgra.CustomData));
    bgra.Draw(Self.Canvas, 0, 0, False);
  end
  // Otherwise we must draw part of state for each button
  else
  begin
    // The active button must be draw as last because right edge of button and
    // left edge of dropdown are overlapping each other, so we must draw edge
    // for current state of active button
    case FActiveButt of
      bbtButton:
      begin
        // Drop down button
        case FDownButtonState of
          msNone: bgra := FBGRANormal;
          msHover: bgra := FBGRAHover;
          msClicked: bgra := FBGRAClick;
        end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonState(bgra.CustomData));
        bgra.DrawPart(GetDropDownRect, Self.Canvas, GetDropDownRect.Left,
          GetDropDownRect.Top, False);
        // Main button
        if FDown then
          bgra := FBGRAClick
        else
          case FButtonState of
            msNone: bgra := FBGRANormal;
            msHover: bgra := FBGRAHover;
            msClicked: bgra := FBGRAClick;
          end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonState(bgra.CustomData));
        bgra.DrawPart(GetButtonRect, Self.Canvas, 0, 0, False);
      end;
      bbtDropDown:
      begin
        // Main button
        if FDown then
          bgra := FBGRAClick
        else
          case FButtonState of
            msNone: bgra := FBGRANormal;
            msHover: bgra := FBGRAHover;
            msClicked: bgra := FBGRAClick;
          end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonState(bgra.CustomData));
        bgra.DrawPart(GetButtonRect, Self.Canvas, 0, 0, False);
        // Drop down button
        case FDownButtonState of
          msNone: bgra := FBGRANormal;
          msHover: bgra := FBGRAHover;
          msClicked: bgra := FBGRAClick;
        end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonState(bgra.CustomData));
        bgra.DrawPart(GetDropDownRect, Self.Canvas, GetDropDownRect.Left,
          GetDropDownRect.Top, False);
      end;
    end;
  end;
end;

procedure TDecksButton.RenderControl;
begin
  inherited RenderControl;
  RenderAll;
end;

procedure TDecksButton.SetGlobalOpacity(const AValue: byte);
begin
  if FGlobalOpacity = AValue then
    exit;
  FGlobalOpacity := AValue;

  RenderControl;
  Invalidate;
end;

procedure TDecksButton.SetTextApplyGlobalOpacity(const AValue: boolean);
begin
  if FTextApplyGlobalOpacity = AValue then
    exit;
  FTextApplyGlobalOpacity := AValue;

  RenderControl;
  Invalidate;
end;

constructor TDecksButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF FPC}
  DisableAutoSizing;
  Include(FControlState, csCreating);
  {$ELSE} //#

  {$ENDIF}
  //{$IFDEF WINDOWS}
  // default sizes under different dpi settings
  //SetSizeVariables(ScaleX(8,96), ScaleX(16,96), ScaleY(8,96), ScaleX(24,96));
  //{$ELSE}
  // default sizes
  SetSizeVariables(16, 8, 8, 24);
  //{$ENDIF}
  BeginUpdate;
  try
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
    ControlStyle := ControlStyle + [csAcceptsControls];
    FBGRANormal := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);
    FBGRAHover := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);
    FBGRAClick := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);

    ParentColor := False;
    Color := clNone;

    FStateNormal  := TBCButtonState.Create(Self);
    FStateHover   := TBCButtonState.Create(Self);
    FStateClicked := TBCButtonState.Create(Self);
    FStateNormal.OnChange := OnChangeState;

    FRounding := TBCRounding.Create(Self);
    FRounding.OnChange := OnChangeState;

	FBevel := TBCBevel.Create(Self);
    FBevel.OnChange := OnChangeState;

    FRoundingDropDown := TBCRounding.Create(Self);
    FRoundingDropDown.OnChange := OnChangeState;

    { Connecting bitmaps with states property to easy call and access }
    FBGRANormal.CustomData := PtrInt(FStateNormal);
    FBGRAHover.CustomData := PtrInt(FStateHover);
    FBGRAClick.CustomData := PtrInt(FStateClicked);

    FButtonState := msNone;
    FDownButtonState := msNone;
    FFlipArrow := False;
	FClickOffset := True;
    FGlyph := TBitmap.Create;
    FGlyph.OnChange := OnChangeGlyph;
    FGlyphMargin := 5;
    FGlyphAlignment:= bcaCenter;
    FGlyphOldPlacement:= true;
    FStyle := bbtButton;
    FStaticButton := False;
    FActiveButt := bbtButton;
    FGlobalOpacity := 255;
    FTextApplyGlobalOpacity := False;
    //FStates := [];
    FDown := False;

    { Default style }
    AssignDefaultStyle;

    FImageChangeLink := TChangeLink.Create;
    FImageChangeLink.OnChange := ImageListChange;
    FImageIndex := -1;

    FShowCaption := True;
    FPreserveGlyphOnAssign := True;
  finally
    {$IFDEF FPC}
    Exclude(FControlState, csCreating);
    EnableAutoSizing;
    {$ELSE} //#
    {$ENDIF}
    EndUpdate;
  end;
end;

procedure FreeThenNil(var obj);
begin
  if Pointer(obj) <> nil then
  begin
    TObject(obj).Free;
    Pointer(obj) := nil;
  end;
end;

destructor TDecksButton.Destroy;
begin
  FImageChangeLink.Free;
  FStateNormal.Free;
  FStateHover.Free;
  FStateClicked.Free;
  FBGRANormal.Free;
  FBGRAHover.Free;
  FBGRAClick.Free;
  {$IFDEF FPC}FreeThenNil(FGlyph);{$ELSE}FreeAndNil(FGlyph);{$ENDIF}
  FRounding.Free;
  FRoundingDropDown.Free;
  FBevel.Free;
  inherited Destroy;
end;

procedure TDecksButton.Assign(Source: TPersistent);
begin
  if Source is TDecksButton then
  begin
    if not PreserveGlyphOnAssign then
      Glyph := TDecksButton(Source).Glyph;
    FGlyphMargin := TDecksButton(Source).FGlyphMargin;
    FStyle := TDecksButton(Source).FStyle;
    FFlipArrow := TDecksButton(Source).FFlipArrow;
    FStaticButton := TDecksButton(Source).FStaticButton;
    FGlobalOpacity := TDecksButton(Source).FGlobalOpacity;
    FTextApplyGlobalOpacity := TDecksButton(Source).FTextApplyGlobalOpacity;
    FStateNormal.Assign(TDecksButton(Source).FStateNormal);
    FStateHover.Assign(TDecksButton(Source).FStateNormal);
    FStateClicked.Assign(TDecksButton(Source).FStateNormal);
    FDropDownArrowSize := TDecksButton(Source).FDropDownArrowSize;
    FDropDownWidth := TDecksButton(Source).FDropDownWidth;
    AutoSizeExtraX := TDecksButton(Source).AutoSizeExtraX;
    AutoSizeExtraY := TDecksButton(Source).AutoSizeExtraY;
    FDown := TDecksButton(Source).FDown;
    FRounding.Assign(TDecksButton(Source).FRounding);
    FRoundingDropDown.Assign(TDecksButton(Source).FRoundingDropDown);
    FBevel.Assign(TDecksButton(Source).FBevel);

    RenderControl;
    Invalidate;
    UpdateSize;
  end
  else
    inherited Assign(Source);
end;

procedure TDecksButton.SetSizeVariables(newDropDownWidth,
  newDropDownArrowSize, newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
begin
  FDropDownArrowSize := newDropDownArrowSize;
  FDropDownWidth := newDropDownWidth;
  AutoSizeExtraY := newAutoSizeExtraVertical;
  AutoSizeExtraX := newAutoSizeExtraHorizontal;

  if csCreating in ControlState then
    Exit;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

function TDecksButton.GetStyleExtension: string;
begin
	Result := 'decksbtn';
end;


end.

