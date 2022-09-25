(*
 * ThKnob for FreePascal
 * Joel Toivonen (hukka) 2004-2009, 2020 (Freeware)
 *                hukkax gmail / http://hukka.ncn.fi
 *
 * A powerful knob component based on TKnob 1.0 (C) 1999 Gary B. Hardman.
 *)

unit hKnob;

{$mode delphi}

interface

uses
	Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Forms,
	LMessages, LResources,
	BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

{$WARN 5024 off : Parameter "$1" not used}

type
	TKnobLineSubProperty = class(TPersistent)
	private
		FOnChange: TNotifyEvent;
		FLineColor: TColor;
	protected
		procedure SetLineColor(const Value: TColor);
	public
		procedure Changed;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	published
		property LineColor: TColor read FLineColor write SetLineColor;
	end;

	TKnobLineProperty = class(TPersistent)
	private
		FOnChange: TNotifyEvent;
		FOwner: TObject;
		FLineWidth: Integer;
		FActive, FDisabled, FHovered: TKnobLineSubProperty;
		FIndicatorInnerPos, FIndicatorOuterPos: Integer; // radius
		FOffsetX, FOffsetY: Integer; // pixel offset bias of indicator
	protected
		procedure Changed;
		procedure StyleChanged(Sender: TObject);
		procedure SetLineWidth(const Value: Integer);
		procedure SetInnerRadius(Value: Integer);
		procedure SetOuterRadius(Value: Integer);
		procedure SetOffsetX(Value: Integer);
		procedure SetOffsetY(Value: Integer);
	public
		constructor Create(AOwner: TObject);
		destructor  Destroy; override;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	published
		property Normal: TKnobLineSubProperty read FActive write FActive;
		//property Inactive: TKnobLineSubProperty read FInactive write FInactive;
		property Hovered:  TKnobLineSubProperty read FHovered write FHovered;
		property Disabled: TKnobLineSubProperty read FDisabled write FDisabled;
		property LineWidth: Integer read FLineWidth write SetLineWidth;
		property RadiusInner: Integer read FIndicatorInnerpos write SetInnerRadius;
		property RadiusOuter: Integer read FIndicatorOuterpos write SetOuterRadius;
		property OffsetX: Integer read FOffsetX write SetOffsetX;
		property OffsetY: Integer read FOffsetY write SetOffsetY;
	end;

	ThKnob = class(TBGRAGraphicCtrl)
	private
		Buffer: TBGRABitmap;
		FDrawing: Boolean;
		FKnobColor: TColor;			{ Knob color }
		FBorderColor: TColor; 		{ Knob border color }
		FBorderWidth: Integer; 		{ Knob border width }
		FMin: Integer;				{ Minimum value }
		FMax: Integer;				{ Maximum value }
		FSnap: Integer;				{ Snap threshold (in pixels) }
		FPosition: Integer;			{ From fMin to fMax inclusive }
		FPositionLabel: TLabel;		{ A label control to which position info is sent}
		FSpringLoaded: Boolean;		{ True - knob returns to zero when released}
		FVerticalMove: Boolean;		{ True - move mouse up and down to adjust knob }
		FPageSize: Word;			{ The increment/decrement of pgup, pgdown}
		FSteps: Integer;			{ Number of steps from Min to Max }
		FSmallChange: Integer;		{ Smallchange }
		FAngleInterval: Single;		{ The angle each step represents }
		FAngle: Integer;			{ The current angle of the indicator }
		FMouseAngle: Integer;		{ The current mouse 'angle' over the knob }
		FArc: Word;
		FDragging: Boolean;			{ Knob position is being 'changed' }
		FSensitivity: Integer;		{ Movement area when fVerticalMove }
		FMultiplier: Integer;		{ Multiplier for integer/float conversion }
		FCursorHide: Boolean;		{ Hide mouse pointer when fVerticalMove? }
		FIndicator: TKnobLineProperty;
		FIndicatorState: TKnobLineSubProperty;
		FOnChange: TNotifyEvent;

		MouseOrigX: Integer;		{ when fVerticalMove }
		MouseOrigY: Integer;		{ when fVerticalMove }
		oldMousePos: TPoint;
		OldP: Integer;

		procedure SetMin(const NewMinValue: Integer);
		procedure SetMax(const NewMaxValue: Integer);
		procedure SetPosition(const NewPosition: Integer);
		procedure SetParams(APosition, AMin, AMax: Integer);
		procedure SetSteps;
		procedure CalcAngle;
		function  CalcPosition(TheAngle: Integer): Integer;

		procedure SetPositionLabel(const NewLabel: TLabel);
		procedure ShowPosition(const ThePosition: Integer); overload;
		procedure SetSpringLoaded(const Sprung: Boolean);
		procedure SetKnobColor(NewColor: TColor);
		procedure SetBorderColor(NewColor: TColor);
		procedure SetBorderWidth(NewWidth: Integer);

		{Windows Messages}
		{procedure WMMove(var Msg: TLMMove);             message LM_MOVE;
		procedure WMSetFocus(var Msg: TLMSETFOCUS);     message LM_SETFOCUS;
		procedure WMKillFocus(var Msg: TLMKILLFOCUS);   message LM_KILLFOCUS;
		procedure WMGetDlgCode(var Msg: TLMGetDlgCode); message LM_GETDLGCODE;}

		{Delphi Component Messages}
		procedure CMMouseEnter(var Msg: TLMessage);          message CM_MouseEnter;
		procedure CMMouseLeave(var Msg: TLMessage);          message CM_MouseLeave;
		procedure CMEnabledChanged(var Msg: TLMessage);      message CM_EnabledChanged;
		procedure CMVisibleChanged(var Msg: TLMessage);      message CM_VisibleChanged;
		procedure CM_ParentColorChanged(var Msg: TLMessage); message CM_ParentColorChanged;
		procedure CM_TextChanged(var Msg: TLMessage);        message CM_TextChanged;
		function  GetFloatPosition: Single;
		procedure SetFloatPosition(Value: Single);
		procedure SetArc(Value: Word);
	protected
		function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

		procedure Paint; override;
		procedure Draw;
		procedure Resize; override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		//procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
		procedure IndicatorChanged(Sender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

		procedure ShowPosition; overload;
	published
		property Anchors;
		property ParentShowHint;
		property ShowHint;
		property Min: Integer read FMin write SetMin default 0;
		property Max: Integer read FMax write SetMax default 100;
		property SmallChange: Integer read FSmallChange write FSmallChange default 1;
		property Caption;
		property Arc: Word read FArc write SetArc;
		property Indicator: TKnobLineProperty read FIndicator write FIndicator;
		property KnobColor: TColor read FKnobColor write SetKnobColor default clBlack;
		property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
		property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 2;
		property Position: Integer read FPosition write SetPosition default 50;
		property FloatPosition: Single read GetFloatPosition write SetFloatPosition;
		property PageSize: Word read FPageSize write FPageSize default 10;
		property Multiplier: Integer read FMultiplier write FMultiplier default 0;
		property PositionLabel: TLabel read FPositionLabel write SetPositionLabel;
		property Sensitivity: Integer read FSensitivity write FSensitivity default 100;
		property SpringLoaded: Boolean read FSpringLoaded write SetSpringLoaded default False;
		property Snap: Integer read FSnap write FSnap default 9;
		property CursorHide: Boolean read FCursorHide write FCursorHide default False;
		//property TabStop default True;
		//property TabOrder;
		property PopupMenu;
		property Visible;
		property Enabled;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property OnClick;
		//property OnEnter;
		//property OnExit;
		//property OnKeyDown;
		//property OnKeyPress;
		//property OnKeyUp;
		property OnMouseDown;
		property OnMouseUp;
		property OnMouseEnter;
		property OnMouseLeave;
		property OnMouseMove;
		property OnMouseWheel;
		property OnMouseWheelDown;
		property OnMouseWheelUp;
	end;

	procedure Register;

implementation

uses
	{$IFDEF WINDOWS}Windows, Types,{$ENDIF}
	Math, LCLIntf, LCLType;

procedure Register;
begin
	RegisterComponents('Custom', [ThKnob]);
end;

(*********************************)
(***   TKnobLineSubProperty    ***)
(*********************************)

procedure TKnobLineSubProperty.Changed;
begin
	if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TKnobLineSubProperty.SetLineColor(const Value: TColor);
begin
	if Value <> FLineColor then
	begin
		FLineColor := Value;
		Changed;
	end;
end;

(******************************)
(***   TKnobLineProperty    ***)
(******************************)

constructor TKnobLineProperty.Create(AOwner: TObject);
begin
	inherited Create;
	FOwner := AOwner;
	FActive := TKnobLineSubProperty.Create;
	FActive.OnChange := StyleChanged;
	//FInactive := TKnobLineSubProperty.Create;
	//FInactive.OnChange := StyleChanged;
	FDisabled := TKnobLineSubProperty.Create;
	FDisabled.OnChange := StyleChanged;
	FHovered := TKnobLineSubProperty.Create;
	FHovered.OnChange := StyleChanged;
	FIndicatorInnerpos := 14;
	FIndicatorOuterpos := 3;
end;

destructor TKnobLineProperty.Destroy;
begin
	inherited;
end;

procedure TKnobLineProperty.StyleChanged(Sender: TObject);
begin
	Changed;
end;

procedure TKnobLineProperty.Changed;
begin
	if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TKnobLineProperty.SetLineWidth(const Value: Integer);
begin
	if Value <> FLineWidth then
	begin
		FLineWidth := Value;
		Changed;
	end;
end;

procedure TKnobLineProperty.SetInnerRadius(Value: Integer);
begin
	if Value <> FIndicatorInnerpos then
	begin
		FIndicatorInnerpos := Value;
		Changed;
	end;
end;

procedure TKnobLineProperty.SetOuterRadius(Value: Integer);
begin
	if Value <> FIndicatorOuterpos then
	begin
		FIndicatorOuterpos := Value;
		Changed;
	end;
end;

procedure TKnobLineProperty.SetOffsetX(Value: Integer);
begin
	if Value <> FOffsetX then
	begin
		FOffsetX := Value;
		Changed;
	end;
end;

procedure TKnobLineProperty.SetOffsetY(Value: Integer);
begin
	if Value <> FOffsetY then
	begin
		FOffsetY := Value;
		Changed;
	end;
end;

(***********************)
(***      ThKnob     ***)
(***********************)

constructor ThKnob.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	Width := 40;
	Height := 40;
	FSensitivity := 100;
	FMin := 0;
	FMax := 100;
	FPosition := 50;
	FSnap := 9;
	FArc := 60;
	FSpringLoaded := False;
	FVerticalMove := True;
	FCursorHide := False;
	//TabStop := True;
	MouseOrigY := 0;

	FKnobColor := clNone;
	FBorderColor := clGray;
	FBorderWidth := 3;
	FIndicator := TKnobLineProperty.Create(Self);
	with FIndicator do
	begin
		OnChange := IndicatorChanged;

		FLineWidth := 5;
		FIndicatorInnerpos := 14;
		FIndicatorOuterPos := 2;

		Normal.FLineColor   := clSilver;
		//Inactive.FLineColor := clWhite;
		Disabled.FLineColor := clGray;
		Hovered.FLineColor  := clWhite;
	end;
	FIndicatorState := FIndicator.Normal;
	// ControlStyle := ControlStyle - [csOpaque];

	Buffer := TBGRABitmap.Create(ClientWidth, ClientHeight);

	SetSteps;
	CalcAngle;
end;

destructor ThKnob.Destroy;
begin
	FIndicator.FActive.Free;
	//FIndicator.FInactive.Free;
	FIndicator.FDisabled.Free;
	FIndicator.FHovered.Free;
	FIndicator.Free;
	Buffer.Free;
	inherited Destroy;
end;

procedure ThKnob.IndicatorChanged(Sender: TObject);
begin
	Invalidate;
end;

procedure ThKnob.SetKnobColor(NewColor : TColor);
begin
	if (FKnobColor <> NewColor) then
	begin
		FKnobColor := NewColor;
		Invalidate;
	end;
end;

procedure ThKnob.SetBorderColor(NewColor: TColor);
begin
	if (FBorderColor <> NewColor) then
	begin
		FBorderColor := NewColor;
		Invalidate;
	end;
end;

procedure ThKnob.SetBorderWidth(NewWidth: Integer);
begin
	if (FBorderWidth <> NewWidth) then
	begin
		FBorderWidth := NewWidth;
		Invalidate;
	end;
end;

procedure ThKnob.SetPositionLabel(const NewLabel: TLabel);
begin
	if FPositionLabel <> NewLabel then
	begin
		FPositionLabel := NewLabel;
		if FPositionLabel <> nil then
			ShowPosition(Position);
	end;
end;

procedure ThKnob.SetSpringLoaded(const Sprung: Boolean);
begin
	if FSpringLoaded <> Sprung then
	begin
		FSpringLoaded := Sprung;
		if Sprung then Position := 0;
	end;
end;

procedure ThKnob.SetPosition(const NewPosition: Integer);
begin
	if Position <> NewPosition then
		SetParams(NewPosition, FMin, FMax);
end;

function ThKnob.GetFloatPosition: Single;
begin
	if not IsZero(FMultiplier) then
		Result := FPosition / FMultiplier
	else
		Result := FPosition;
end;

procedure ThKnob.SetFloatPosition(Value: Single);
begin
	if not IsZero(FMultiplier) then
		Position := Trunc(Value * FMultiplier)
	else
		Position := Trunc(Value);
end;

procedure ThKnob.SetMin(const NewMinValue: Integer);
begin
	if Min <> NewMinValue then
		SetParams(FPosition, NewMinValue, FMax);
end;

procedure ThKnob.SetMax(const NewMaxValue: Integer);
begin
	if Max <> NewMaxValue then
		SetParams(FPosition, FMin, NewMaxValue);
end;

{Called whenever Min or Max is changed}
procedure ThKnob.SetSteps;
begin
	FSteps := FMax - FMin;
	if FSteps = 0 then
		FAngleInterval := 0
	else
	begin
		FAngleInterval := (180 + (FArc*2)) / FSteps;
		FSteps := Abs(FSteps);
	end;
end;

procedure ThKnob.SetArc(Value: Word);
begin
	Value := Math.Min(Value, 88);
	if FArc <> Value then
	begin
		FArc := Value;
		SetSteps;
		CalcAngle;
		Invalidate;
	end;
end;

{Calculate characteristics of knob when Position, Max or Min are changed}
procedure ThKnob.SetParams(APosition, AMin, AMax: Integer);
begin
	if (fMin <> AMin) then {Min has Changed}
	begin
		fMin := AMin;
		SetSteps;	  {updates fSteps and fAngleInterval}
	end;
	if (fMax <> AMax) then
	begin
		fMax := AMax;
		SetSteps;	  {updates fSteps and fAngleInterval}
	end;
	if fAngleInterval >= 0 then {Max is greater than Min}
	begin
		APosition := EnsureRange(APosition, AMin, AMax);
	end else
	begin						 {Min is Greater than Max}
		if APosition > AMin then APosition := AMax;
		if APosition < AMax then APosition := AMin;
	end;
	if fPosition <> APosition then fPosition := APosition;

	CalcAngle;					{Set fAngle}
	ShowPosition(fPosition);	{Update the PositionLabel caption}
	Invalidate;

	{Fire the OnChange event if not in Designing state}
	if (Assigned(fOnChange)) and not (csDesigning in ComponentState) then
		fOnChange(Self);
end;

{If the PositionLabel is removed then point it to nil }
procedure ThKnob.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited Notification(AComponent, Operation);
	if (AComponent = FPositionLabel) and (Operation = opRemove) then
		FPositionLabel := nil;
end;

{Display the Position value in an associated PositionLabel control}
procedure ThKnob.ShowPosition(const ThePosition: Integer);
begin
	if FPositionLabel <> nil then
	begin
		if not IsZero(FMultiplier) then
		begin
			if Pos('%', Caption) > 0 then
				FPositionLabel.Caption := Format(Caption, [GetFloatPosition])
			else
				FPositionLabel.Caption := Format('%.2f', [GetFloatPosition])
		end
		else
			FPositionLabel.Caption := Format(Caption, [ThePosition]);
	end;
end;


{*** Message handlers ***}

(*
procedure ThKnob.WMSetFocus(var Msg: TLMSETFOCUS);
begin
	if Enabled then FIndicatorState := FIndicator.FActive;
	Invalidate;
	inherited;
end;

procedure ThKnob.WMKillFocus(var Msg: TLMKILLFOCUS);
begin
	FDragging := False; {Release dragging flag}
	if FSpringLoaded then Position := 0;
	if Enabled then FIndicatorState := FIndicator.FInactive;
	Invalidate; {Paint as non-focused}
	inherited;
end;

procedure ThKnob.WMMove(var Msg: TLMMove);
begin
	inherited;
	Invalidate;
end;

{Process Arrow, Page and Home/End keys}
procedure ThKnob.WMGetDlgCode(var Msg: TLMGetDlgCode);
begin
	inherited;
//	Msg.Result := DLGC_WANTARROWS; !!!
end;
*)

function ThKnob.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
	MousePos: TPoint): Boolean;
begin
	Result := True;
	if Enabled then
	begin
		if WheelDelta > 0 then
			Position := FPosition + FPageSize
		else
		if WheelDelta < 0 then
			Position := FPosition - FPageSize;
	end;
	inherited;
	{if Assigned(OnMouseWheel) then
		OnMouseWheel(Self, [], Msg.WheelDelta,
		Point(Msg.XPos, Msg.YPos), Handled);}
end;

procedure ThKnob.CMMouseEnter(var Msg: TLMessage);
begin
	inherited;
	MouseEnter;
	if Enabled then
	begin
		FIndicatorState := FIndicator.Hovered;
		Invalidate;
	end;
end;

{Reset the PositionLabel caption on mouse exit}
procedure ThKnob.CMMouseLeave(var Msg: TLMessage);
begin
	ShowPosition(Position);
	MouseLeave;
	if Enabled then
	begin
		//if Focused then
			FIndicatorState := FIndicator.FActive;
		//else
		//	FIndicatorState := FIndicator.FInactive;
		Invalidate;
	end;
	inherited;
end;

procedure ThKnob.CMEnabledChanged(var Msg: TLMessage);
begin
	if not Enabled then
		FIndicatorState := FIndicator.Disabled
	else
		FIndicatorState := FIndicator.Normal;
	if PositionLabel <> nil then
		PositionLabel.Enabled := Enabled;
	Invalidate;
	inherited;
end;

procedure ThKnob.CMVisibleChanged(var Msg: TLMessage);
begin
	{Show or hide the position Label in sync with the Knob}
	if PositionLabel <> nil then
		PositionLabel.Visible := Visible;
	inherited;
end;

procedure ThKnob.CM_ParentColorChanged(var Msg: TLMessage);
begin
	Invalidate;
	inherited;
end;

{If the caption changes then redraw position label}
procedure ThKnob.CM_TextChanged(var Msg: TLMessage);
begin
	ShowPosition(Position);
end;

{ *** event handlers *** }

procedure ThKnob.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
//var
//	NewPosition: Integer;
begin
	if fDragging then
	begin
		if fVerticalMove then
		begin
			fMouseAngle := Mouse.CursorPos.Y;
			if fCursorHide then
			begin
				Mouse.CursorPos := oldMousePos;
				{$IFDEF WINDOWS}ShowCursor(True);{$ENDIF}
			end;
		end;
		if SpringLoaded then Position := 0;
		{
		else
		begin
		  NewPosition := CalcPosition(fMouseAngle);
		  if Position <> NewPosition then fPosition := NewPosition;
		end;
		}
		fDragging := False;
	end;
	inherited MouseUp(Button,Shift,X,Y);
end;

procedure ThKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	RegionHandle: HRGN;
	n: Integer;
begin
	//if TabStop and CanFocus then SetFocus;

	case Button of

		mbLeft:
		begin
			FVerticalMove := True;
			MouseOrigX := Mouse.CursorPos.X;
			MouseOrigY := Mouse.CursorPos.Y;
			if not FSpringLoaded then
			begin
				n := ((FMax - FMin) div 2) + FMin;
				if FPosition < n then Dec(MouseOrigY, FSnap) else
					if FPosition > n then Inc(MouseOrigY, FSnap);
			end;
			OldP := FPosition;
			if FCursorHide then
			begin
				oldMousePos := Mouse.CursorPos;
				{$IFDEF WINDOWS}ShowCursor(False);{$ENDIF}
			end;
			FDragging := True;
		end;

		mbRight:
		if not Assigned(OnMouseDown) then
		begin
			FDragging := True;
			FVerticalMove := False;
			MouseMove(Shift, X, Y);
			RegionHandle := CreateEllipticRgnIndirect(
				Types.Rect(Left, Top, Math.Min(Width, Height), Math.Min(Width, Height)));
			if RegionHandle > 0 then
				if PtInRegion(RegionHandle, X,Y) then
				try
					fDragging := True;
					Position := CalcPosition(fMouseAngle);
				finally
					DeleteObject(RegionHandle);
				end;
		end;

		mbMiddle:
			Position := (Max + Min) div 2;

	end;

	Invalidate;
	inherited MouseDown(Button, Shift, X, Y);
end;

{This is where the MouseAngle value is changed,
 (the 'Position' value is calculated from fMouseAngle) }
procedure ThKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	ii, NewPosition: Integer;
begin
	ii := Math.Min(Width, Height) div 2;
	if fDragging then
	begin
		if fVerticalMove then
		begin
			fMouseAngle := Mouse.CursorPos.Y;
			NewPosition := CalcPosition(fMouseAngle);
		end
		else
		begin
			if X = ii then
			begin
				if Y > ii
					then fMouseAngle := 180+FArc
					else fMouseAngle := 90; // rotation behaviour
			end
			else
			begin
				fMouseAngle := Round(ArcTan((ii-Y) / (X-ii)) * 180/pi);
				if X < Width div 2 then
					fMouseAngle := (fMouseAngle + 540) mod 360;
			end;
			if (fMouseAngle > (180+FArc)) and (fMouseAngle <= (180+30+FArc)) then
				fMouseAngle := 180+FArc;
			if (fMouseAngle < -FArc) then
				fMouseAngle := -FArc;
			NewPosition := CalcPosition(fMouseAngle);
		end; // not fVerticalMove
		if fPosition <> NewPosition then
			Position := NewPosition;
	end;
	inherited MouseMove(Shift, X, Y);
end;

(*procedure ThKnob.KeyDown(var Key: Word; Shift: TShiftState);
begin
	inherited KeyDown(Key, Shift);
	case Key of
		VK_PRIOR: Position := fPosition + fPageSize;
		VK_NEXT	: Position := fPosition - fPageSize;
		VK_END	: Position := fMax;
		VK_HOME	: Position := fMin;
		VK_LEFT	: Position := fPosition - fSmallChange;
		VK_RIGHT: Position := fPosition + fSmallChange;
	end;
end;*)

procedure ThKnob.Paint;
begin
	inherited;
	if csDesigning in ComponentState then
	with inherited Canvas do
	begin
		Pen.Style := psDash;
		Brush.Style := bsClear;
		Rectangle(0, 0, Width, Height);
	end;
	Draw;
end;

{Calculate fAngle based on fMin, fPosition and fAngleInterval}
procedure ThKnob.CalcAngle;
begin
	fAngle := (180+FArc) - Round((fPosition - fMin) * fAngleInterval);
end;

{Calculate fPosition based on fMin, fMax, Angle parameter and fAngleInterval}
function ThKnob.CalcPosition(TheAngle: Integer): Integer;
var
	e: Single;
	n: Integer;
begin
	if fVerticalMove then
	begin
		e := (OldP - fMin) / (fMax - fMin) * fSensitivity;
		TheAngle := MouseOrigY - TheAngle + Integer(Round(e));
		if not SpringLoaded then
		begin
			n := fSensitivity div 2;
			if Abs(TheAngle - n) < fSnap then
				TheAngle := n
			else
			begin
				if TheAngle < n then TheAngle := TheAngle + fSnap
				else
				if TheAngle > n then TheAngle := TheAngle - fSnap;
			end;
		end;
		if TheAngle > fSensitivity+fSnap then TheAngle := fSensitivity + fSnap;
		if TheAngle <= -fSnap then TheAngle := -fSnap;
		e := ((TheAngle) / fSensitivity) * (fMax - fMin);
		Result := Trunc(e) + fMin;
		if Result < fMin then Result := fMin;
		if Result > fMax then Result := fMax;
	end
	else
	begin
		if fAngleInterval = 0 then
			Result := fMin
		else
			Result := fMin + Round(((180+FArc) - TheAngle) / fAngleInterval);
	end;
end;

procedure ThKnob.Draw;
var
	Radius, AngleInRadians, CosAngle, SinAngle: Single;
	Save: Boolean;
	FillColor: TBGRAPixel;
begin
	Save := FDrawing;
	FDrawing := True;

	Radius := (ClientWidth-1) / 2; //(Math.Min(ClientWidth, ClientHeight) - 1) / 2;
	AngleInRadians := FAngle * Pi / 180;
	CosAngle := Cos(AngleInRadians);
	SinAngle := Sin(AngleInRadians);

	Buffer.Fill(BGRAPixelTransparent);

	if FKnobColor = clNone then
		FillColor := BGRAPixelTransparent
	else
		FillColor := ColorToBGRA(FKnobColor);

	if (FBorderWidth < 1) or (FBorderColor = clNone) then
	begin
		Buffer.FillEllipseAntialias(Radius, Radius, Radius, Radius,
			FillColor);
		if FBorderColor <> clNone then
			Buffer.EllipseAntialias(Radius, Radius, Radius, Radius,
				ColorToBGRA(FBorderColor), FBorderWidth);
	end
	else
		Buffer.Arc(Radius, Radius, Radius-FBorderWidth, Radius-FBorderWidth,
			DegToRad(0-FArc), DegToRad(180+FArc),
			ColorToBGRA(FBorderColor),
			FBorderWidth, False,
			FillColor);

	if FIndicator.FLineWidth > 0 then
		Buffer.DrawLineAntialias(
			Radius + ((Radius-FIndicator.FIndicatorOuterPos)*CosAngle) + FIndicator.FOffsetX,
			Radius - ((Radius-FIndicator.FIndicatorOuterPos)*SinAngle) + FIndicator.FOffsetY,
			Radius + ((Radius-FIndicator.FIndicatorInnerPos)*CosAngle) + FIndicator.FOffsetX,
			Radius - ((Radius-FIndicator.FIndicatorInnerPos)*SinAngle) + FIndicator.FOffsetY,
			ColorToBGRA(FIndicatorState.FLineColor),
			FIndicator.FLineWidth);

	Buffer.Draw(Canvas, 0, 0, False);
	FDrawing := Save;
end;

procedure ThKnob.Resize;
begin
	inherited;
	if Buffer <> nil then
		Buffer.SetSize(ClientWidth, ClientHeight);
end;

procedure ThKnob.ShowPosition;
begin
	ShowPosition(FPosition);
end;


end.

