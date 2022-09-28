// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
}

unit DecksValueLabel;

{$MODE Delphi}

interface

uses
  Classes, SysUtils,{$IFDEF FPC}LResources,{$ENDIF}
  Types, Forms, Controls, Graphics, Dialogs,
  BCBasectrls, BGRABitmap, BGRABitmapTypes, BCTypes,
  DecksBevel;

const
	dvlZoneNone  = -1;
	dvlZoneLeft  = -2;
	dvlZoneRight = -3;

type
	TDecksValueLabel = class(TBCStyleGraphicControl)
	private
		Drag: record
			Zone:     Integer;
			Value:    Integer;
			Length:   Integer;
			Origin:   TPoint;
			Min, Max: Integer;
		end;
		FBackground: TBCBackground;
		FBGRA: TBGRABitmapEx;
		FBevel:  TBCBevel;
		FBorder: TBCBorder;
		FFontEx: TBCFont;
		FInnerMargin: Single;
		FRounding: TBCRounding;
		FValue: Double;
		FDecimalSeparator: Char;
		FIntegerDigits: Byte;
		FFractionDigits: Byte;
		FFormatSettings: TFormatSettings;
		FHoverColor: TColor;
		FHoverAlpha: Byte;
		HoveredZone: Integer;
		FMin, FMax:  Cardinal;
		DigitZones: array[0..20] of TRect;
		FDragHeight: Word;
		FInvertDragDir,
		FInvertWheelDir: Boolean;
		FFullSideSelect: Boolean;
		FOnChange: TNotifyEvent;
		procedure Render;
		procedure Redraw;
		procedure SetInnerMargin(AValue: single);
		procedure SetRounding(AValue: TBCRounding);
		procedure UpdateSize;
		procedure SetBackground(AValue: TBCBackground);
		procedure SetBorder(AValue: TBCBorder);
		procedure SetFontEx(AValue: TBCFont);
		procedure OnChangeProperty(Sender: TObject; {%H-}Data: BGRAPtrInt);
		procedure SetBevel(AValue: TBCBevel);
		procedure SetValue(AValue: Double);
		procedure SetIntegerDigits(AValue: Byte);
		procedure SetFractionDigits(AValue: Byte);
		procedure SetDecimalSeparator(AValue: Char);
		procedure SetHoverColor(AValue: TColor);
		procedure SetHoverAlpha(AValue: Byte);
		procedure SetMin(AValue: Cardinal);
		procedure SetMax(AValue: Cardinal);
	protected
		class function GetControlClassDefaultSize: TSize; override;
		function  GetZoneAt(X, Y: Integer): Integer;
		function  GetStyleExtension: String; override;
		function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
		procedure ChangeScale(Multiplier, Divider: Integer); override;
		procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
		          {%H-}WithThemeSpace: boolean); override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseLeave; override;
		procedure TextChanged; override;
		procedure DrawControl; override;
		procedure RenderControl; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;
		procedure UpdateControl; override; // Called by EndUpdate
		{ Streaming }
		{$IFDEF FPC}
		procedure SaveToFile(AFileName: string); override;
		procedure LoadFromFile(AFileName: string); override;
		{$ENDIF}
		procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
			var ComponentClass: TComponentClass);
	published
		property Action;
		property Align;
		property Anchors;
		property AssignStyle;
		property Background: TBCBackground read FBackground write SetBackground;
		property Bevel: TBCBevel read FBevel write SetBevel;
		property Border: TBCBorder read FBorder write SetBorder;
		property BorderSpacing;
		property Cursor;
		property Enabled;
		property FontEx: TBCFont read FFontEx write SetFontEx;
		property Height;
		property HelpContext;
		property HelpKeyword;
		property HelpType;
		property Hint;
		property InnerMargin: Single read FInnerMargin write SetInnerMargin;
		property Left;
		property PopupMenu;
		property Rounding: TBCRounding read FRounding write SetRounding;
		property ShowHint;
		property Tag;
		property Top;
		property Visible;
		property Width;
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
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property Value: Double read FValue write SetValue;
		property IntegerDigits:  Byte read FIntegerDigits  write SetIntegerDigits;
		property FractionDigits: Byte read FFractionDigits write SetFractionDigits;
		property DecimalSeparator: Char read FDecimalSeparator write SetDecimalSeparator;
		property HoverColor: TColor read FHoverColor write SetHoverColor;
		property HoverAlpha: Byte read FHoverAlpha write SetHoverAlpha;
		property Min: Cardinal read FMin write SetMin;
		property Max: Cardinal read FMax write SetMax;
		property DragHeight: Word read FDragHeight write FDragHeight;
		property InvertDragDir:  Boolean read FInvertDragDir write FInvertDragDir;
		property InvertWheelDir: Boolean read FInvertWheelDir write FInvertWheelDir;
		property FullSideSelect: Boolean read FFullSideSelect write FFullSideSelect;
	end;

	procedure Register;

implementation

uses
	Math, LCLType, BCTools;

procedure Register;
begin
	RegisterComponents('Custom', [TDecksValueLabel]);
end;

{ TDecksValueLabel }

procedure TDecksValueLabel.Render;
var
	r, rs, ro: TRect;
	i, w: Integer;
	bm:  TBGRABitmap;
	hal: TAlignment;
	val: TTextLayout;
	st:  TTextStyle;
begin
	if (csCreating in ControlState) or IsUpdating then Exit;

	FBGRA.NeedRender := False;

	bm := TBGRABitmap(FBGRA);
	FBGRA.SetSize(Width, Height);
	FBGRA.Fill(BGRAPixelTransparent); // Clear;
	ro := FBGRA.ClipRect;
	CalculateBorderRect(FBorder, ro);

	AssignBCFont(FFontEx, bm);

	hal := BCAlign2HAlign(FFontEx.TextAlignment);
	val := BCAlign2VAlign(FFontEx.TextAlignment);
	FillChar({%H-}st, SizeOf({%H-}st),0);
	st.Alignment   := hal;
	st.Layout      := val;

	DecksBevel.RenderBackgroundAndBorder(ro, FBackground, bm,
		FRounding, FBorder, FBevel, FInnerMargin);

	r := ro;
	r.Left   += FFontEx.PaddingLeft;
	r.Right  -= FFontEx.PaddingRight;
	r.Top    += FFontEx.PaddingTop + 2;
	r.Bottom -= FFontEx.PaddingBottom;
	w := r.Width div Length(Caption);
	r.Right := r.Left + w;

	// calculate rectangles for digits
	for i := 1 to Length(Caption) do
	begin
		DigitZones[i-1] := r;
		r.Offset(w, 0);
	end;

	// paint hover rectangle
	if HoveredZone <> dvlZoneNone then
	begin
		r := ro;
		r.Inflate(-1, -2);
		w := 6;
		case HoveredZone of
			dvlZoneLeft:
			begin
				if FFractionDigits > 0 then
					r.Right := DigitZones[FIntegerDigits].Left  + w - 1;
				Drag.Length := FIntegerDigits;
			end;
			dvlZoneRight:
				if FFractionDigits > 0 then
				begin
					r.Left  := DigitZones[FIntegerDigits].Right - w;
					Drag.Length := FFractionDigits;
				end;
			else
			begin
				r := DigitZones[HoveredZone];
				if HoveredZone < FIntegerDigits then
				begin
					if FFractionDigits > 0 then
						r.Right := DigitZones[FIntegerDigits].Left
					else
						r.Right := DigitZones[FIntegerDigits-1].Right;
					Drag.Length := FIntegerDigits - HoveredZone;
				end
				else
				if HoveredZone > FIntegerDigits then
				begin
					r.Right := DigitZones[FIntegerDigits + FFractionDigits].Right;
					Drag.Length := FFractionDigits - (HoveredZone - FIntegerDigits - 1);
				end;
			end;
		end;
		bm.FillRect(r, FHoverColor, dmSet, FHoverAlpha*256);
	end;

	// paint numbers
	for i := 1 to Length(Caption) do
	begin
		r := DigitZones[i-1];
		if FFontEx.Shadow then
		begin
			rs := r;
			rs.Offset(FFontEx.ShadowOffsetX, FFontEx.ShadowOffsetY);
			bm.TextRect(rs, rs.Left, rs.Top, Caption[i], st,
				ColorToBGRA(ColorToRGB(FFontEx.ShadowColor), FFontEx.ShadowColorOpacity));
		end;
		bm.TextRect(r, r.Left, r.Top, Caption[i], st, FFontEx.Color);
	end;

	{$IFNDEF FPC}//# //@  IN DELPHI NEEDRENDER NEED TO BE TRUE. IF FALSE COMPONENT IN BGRANORMAL BE BLACK AFTER INVALIDATE.
	FBGRA.NeedRender := True;
	{$ENDIF}
end;

// =================================================================================================
// Utility
// =================================================================================================

procedure TDecksValueLabel.Redraw;
var
	S: String;
begin
	if FFractionDigits = 0 then
	begin
		S := Trunc(FValue).ToString;
		while Length(S) < FIntegerDigits do S := '0' + S;
	end
	else
	begin
		S := FValue.ToString(ffFixed, FIntegerDigits, FFractionDigits, FFormatSettings);
		while Length(S) <= Word(FIntegerDigits + FFractionDigits) do S := '0' + S;
	end;
	Caption := S;

	RenderControl;
	UpdateSize;
	Invalidate;
end;

procedure TDecksValueLabel.UpdateSize;
begin
	InvalidatePreferredSize;
	AdjustSize;
end;

procedure TDecksValueLabel.CalculatePreferredSize(var PreferredWidth,
	PreferredHeight: integer; WithThemeSpace: boolean);
begin
	if (Parent <> nil) and (Parent.HandleAllocated) then
		CalculateTextSize(Caption, FFontEx, PreferredWidth, PreferredHeight);
end;

procedure TDecksValueLabel.DrawControl;
begin
	inherited DrawControl;
	if FBGRA.NeedRender then
		Render;
	FBGRA.Draw(Self.Canvas, 0, 0, False);
	{$IFNDEF FPC}//# //@  IN DELPHI RenderControl NEDD. IF NO RenderControl BE BLACK AFTER INVALIDATE.
	FBGRA.NeedRender := True;
	{$ENDIF}
end;

procedure TDecksValueLabel.RenderControl;
begin
	inherited RenderControl;
	if FBGRA <> nil then
		FBGRA.NeedRender := True;
end;

procedure TDecksValueLabel.UpdateControl;
begin
	RenderControl;
	inherited UpdateControl; // invalidate
end;

class function TDecksValueLabel.GetControlClassDefaultSize: TSize;
begin
	Result.cx := 120;
	Result.cy := 32;
end;

function TDecksValueLabel.GetStyleExtension: String;
begin
	Result := 'bclbl';
end;

function TDecksValueLabel.GetZoneAt(X, Y: Integer): Integer;
var
	i, DigitCount: Integer;
	P: TPoint;
begin
	Result := dvlZoneNone;
	if FFullSideSelect then
	begin
		if (FFractionDigits = 0) or (X < DigitZones[FIntegerDigits].Left) then
			Result := dvlZoneLeft
		else
		if (X > DigitZones[FIntegerDigits].Right) then
			Result := dvlZoneRight;
	end
	else
	begin
		P := Point(X, Y);
		DigitCount := FIntegerDigits + FFractionDigits;

		for i := 0 to DigitCount do
			if DigitZones[i].Contains(P) then
				Exit(i);
		if X < DigitZones[0].Left then
			Result := dvlZoneLeft
		else
		if X >= DigitZones[DigitCount].Right then
		begin
			if FFractionDigits > 0 then
				Result := dvlZoneRight
			else
				Result := dvlZoneLeft;
		end;
	end;
end;

// =================================================================================================
// Streaming
// =================================================================================================

procedure TDecksValueLabel.LoadFromFile(AFileName: string);
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

procedure TDecksValueLabel.SaveToFile(AFileName: string);
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

// =================================================================================================
// Events
// =================================================================================================

procedure TDecksValueLabel.OnChangeProperty(Sender: TObject; Data: BGRAPtrInt);
begin
	Redraw;
end;

procedure TDecksValueLabel.OnFindClass(Reader: TReader; const AClassName: string;
	var ComponentClass: TComponentClass);
begin
	if CompareText(AClassName, 'TDecksValueLabel') = 0 then
		ComponentClass := TDecksValueLabel;
end;

procedure TDecksValueLabel.TextChanged;
begin
	inherited;
	Redraw;
end;

// =================================================================================================
// Getters/Setters
// =================================================================================================

procedure TDecksValueLabel.SetInnerMargin(AValue: single);
begin
	if FInnerMargin = AValue then Exit;
	FInnerMargin := AValue;
	Redraw;
end;

procedure TDecksValueLabel.SetRounding(AValue: TBCRounding);
begin
	if FRounding = AValue then Exit;
	FRounding.Assign(AValue);
	Redraw;
end;

procedure TDecksValueLabel.SetBackground(AValue: TBCBackground);
begin
	FBackground.Assign(AValue);
	Redraw;
end;

procedure TDecksValueLabel.SetBorder(AValue: TBCBorder);
begin
	FBorder.Assign(AValue);
	Redraw;
end;

procedure TDecksValueLabel.SetFontEx(AValue: TBCFont);
begin
	FFontEx.Assign(AValue);
	Render;
end;

procedure TDecksValueLabel.SetBevel(AValue: TBCBevel);
begin
	if FBevel = AValue then Exit;
	FBevel.Assign(AValue);
	Redraw;
end;

procedure TDecksValueLabel.SetValue(AValue: Double);
begin
	if (FValue = AValue) or (AValue < FMin) or (AValue > FMax) then Exit;
	FValue := AValue;
	Redraw;
	if (Assigned(FOnChange)) and (not (csDesigning in ComponentState)) and
		(not (csCreating in FControlState)) then
			FOnChange(Self);
end;

procedure TDecksValueLabel.SetFractionDigits(AValue: Byte);
begin
	if (FFractionDigits = AValue) or (AValue > 7) then Exit;
	FFractionDigits := AValue;
	Redraw;
end;

procedure TDecksValueLabel.SetIntegerDigits(AValue: Byte);
begin
	if (FIntegerDigits = AValue) or (AValue < 1) or (AValue > 7) then Exit;
	FIntegerDigits := AValue;
	Redraw;
end;

procedure TDecksValueLabel.SetDecimalSeparator(AValue: Char);
begin
	if FDecimalSeparator = AValue then Exit;
	FDecimalSeparator := AValue;
	FFormatSettings.DecimalSeparator := FDecimalSeparator;
	Redraw;
end;

procedure TDecksValueLabel.SetHoverColor(AValue: TColor);
begin
	if FHoverColor = AValue then Exit;
	FHoverColor := AValue;
	Redraw;
end;

procedure TDecksValueLabel.SetHoverAlpha(AValue: Byte);
begin
	if FHoverAlpha = AValue then Exit;
	FHoverAlpha := AValue;
	Redraw;
end;

procedure TDecksValueLabel.SetMin(AValue: Cardinal);
begin
	if FMin = AValue then Exit;
	FMin := AValue;
	if FValue < FMin then SetValue(FMin);
end;

procedure TDecksValueLabel.SetMax(AValue: Cardinal);
begin
	if FMax = AValue then Exit;
	FMax := AValue;
	if FValue > FMax then SetValue(FMax);
end;

// =================================================================================================
// Interaction
// =================================================================================================

function TDecksValueLabel.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
	i: Double;
begin
	Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
	if (not Result) and (HoveredZone <> dvlZoneNone) then
	begin
		if (FFractionDigits = 0) or (HoveredZone = dvlZoneLeft) or
			(HoveredZone in [0..FIntegerDigits]) then
		begin
			i := 1;
		end
		else
		begin
			i := 1 / (Power(10, FFractionDigits-1));
		end;
		if InvertWheelDir then i := -i;
		SetValue(FValue + (i * Sign(WheelDelta)));
		Result := True;
	end;
end;

procedure TDecksValueLabel.ChangeScale(Multiplier, Divider: Integer);
begin
	if Multiplier <> Divider then
		FFontEx.Height := MulDiv(FBGRA.FontHeight, Multiplier, Divider);
	inherited ChangeScale(Multiplier, Divider);
end;

procedure TDecksValueLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	case Button of
		mbLeft:
		begin
			(*if FCursorHide then
			begin
				oldMousePos := Mouse.CursorPos;
				{$IFDEF WINDOWS}ShowCursor(False);{$ENDIF}
			end;*)
			if HoveredZone <> dvlZoneNone then
			begin
				Drag.Zone := HoveredZone;
				Drag.Origin := Mouse.CursorPos;
				case Drag.Zone of
					dvlZoneLeft:  Drag.Value := Trunc(FValue);
					dvlZoneRight: Drag.Value := Trunc(Frac(FValue) * Power(10, FFractionDigits));
				else
					Drag.Value := StrToInt(Copy(Caption, Drag.Zone+1, Drag.Length));
				end;

				if Drag.Zone = dvlZoneLeft then
				begin
					Drag.Min := FMin;
					Drag.Max := FMax;
				end
				else
				begin
					Drag.Min := 0;
					Drag.Max := Trunc(Power(10, Drag.Length))-1;
				end;

			end;
		end;
	end;
	inherited;
end;

procedure TDecksValueLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	Drag.Zone := dvlZoneNone;
	inherited;
end;

procedure TDecksValueLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	i: Integer;
	S, VS: String;
begin
	if Drag.Zone <> dvlZoneNone then
	begin
		i := Mouse.CursorPos.Y - Drag.Origin.Y;
		i := Trunc(i * (Drag.Max - Drag.Min) / (FDragHeight));
		if not FInvertDragDir then i := -i;
		i := Math.Max(Drag.Min, Drag.Value + i);
		i := Math.Min(i, Drag.Max);

		case Drag.Zone of
			dvlZoneLeft:
				SetValue(i + Frac(FValue));
			dvlZoneRight:
				SetValue(Trunc(FValue) + (i / (Drag.Max+1)));
			else
			begin
				// this doesn't feel like the best way to accomplish this...
				S := IntToStr(i);
				while Length(S) < Drag.Length do S := '0' + S;
				VS := Caption;
				for i := 1 to Drag.Length do
					VS[i+Drag.Zone] := S[i];
				SetValue(StrToFloat(VS, FFormatSettings));
			end;
		end;
	end
	else
	begin
		i := GetZoneAt(X, Y);

		if (i = FIntegerDigits) and (FFractionDigits > 0) then
			i := dvlZoneNone
		else
		if i = 0 then
			i := dvlZoneLeft
		else
		if i = (FIntegerDigits + 1) then
			i := dvlZoneRight;

		if i <> HoveredZone then
		begin
			HoveredZone := i;
			Redraw;
		end;
	end;

	inherited;
end;

procedure TDecksValueLabel.MouseLeave;
begin
	HoveredZone := -1;
	Redraw;
	inherited;
end;

// =================================================================================================
// Initialization
// =================================================================================================

constructor TDecksValueLabel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	DisableAutoSizing;
	Include(FControlState, csCreating);

	BeginUpdate;
	try
		with GetControlClassDefaultSize do
			SetInitialBounds(0, 0, CX, CY);

		FIntegerDigits := 3;
		FFractionDigits:= 0;
		FMin := 0;
		FMax := 999;
		FValue := 0;
		FInvertDragDir  := False;
		FInvertWheelDir := False;
		FFullSideSelect := False;
		FDragHeight := 400;
		FDecimalSeparator := ' ';
		HoveredZone := -1;
		Drag.Zone := dvlZoneNone;

		FBGRA := TBGRABitmapEx.Create(Width, Height);
		FBackground         := TBCBackground.Create(Self);
		FBevel              := TBCBevel.Create(Self);
		FBorder             := TBCBorder.Create(Self);
		FRounding           := TBCRounding.Create(Self);
		FFontEx             := TBCFont.Create(Self);

		ParentColor         := True;
		AutoSize := False;

		FHoverColor := clAqua;
		FHoverAlpha := 40;

		FBackground.OnChange := OnChangeProperty;
		FBevel.OnChange      := OnChangeProperty;
		FBorder.OnChange     := OnChangeProperty;
		FRounding.OnChange   := OnChangeProperty;
		FFontEx.OnChange     := OnChangeProperty;

		FBackground.Color   := $444444;
		FBackground.Style   := bbsColor;

		FBorder.Color       := $333333;
		FBorder.Style       := bboSolid;

		FBevel.ColorDark    := $666666;
		FBevel.ColorLight   := $FFFFFF;
		FBevel.Opacity      := 50;
		FBevel.OuterBevel   := bcbBottom;

		FRounding.RoundX := 0;
		FRounding.RoundY := 0;

		FFontEx.Color       := $CCCCCC;
		FFontEx.Shadow      := True;
		FFontEx.ShadowColor := $000000;
		FFontEx.ShadowColorOpacity := 100;
		FFontEx.ShadowOffsetX := -1;
		FFontEx.ShadowOffsetY := -1;
		FFontEx.ShadowRadius := 1;
		FFontEx.Style := [fsBold];
		FFontEx.TextAlignment := bcaCenter;
		FFontEx.PaddingBottom := 5;
		FFontEx.PaddingLeft   := 10;
		FFontEx.PaddingRight  := 10;
		FFontEx.PaddingTop    := 0;
		FFontEx.Height := 20;

	finally
		EnableAutoSizing;
		EndUpdate;
		Exclude(FControlState, csCreating);
		SetDecimalSeparator('.');
	end;
end;

destructor TDecksValueLabel.Destroy;
begin
	FBGRA.Free;
	FBackground.Free;
	FBevel.Free;
	FBorder.Free;
	FFontEx.Free;
	FRounding.Free;

	inherited Destroy;
end;

end.


