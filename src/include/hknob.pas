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
	Classes, Types, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Forms,
	LMessages, LResources,
	BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

{$WARN 5024 off : Parameter "$1" not used}

type
	ThKnob            = class;
	TKnobBase         = class;
	TKnobIndicator    = class;
	TKnobIndicators   = class;

	TIndicators = class(TOwnedCollection)
	private
		FKnob: ThKnob;

		procedure Changed;
		function  GetItem(Index: Integer): TKnobIndicator;
		procedure SetItem(Index: Integer; const Value: TKnobIndicator);
	public
		constructor Create(AOwner: ThKnob);

		function Add: TKnobIndicator;

		property Items[Index: Integer]: TKnobIndicator read GetItem write SetItem;
	end;

	TKnobBase = class(TPersistent)
	private
		FOwner:       ThKnob;

		FFillColor:   TColor;
		FBorderColor: TColor;
		FBorderWidth: Integer;
	protected
		procedure Changed;

		procedure SetFillColor  (NewColor: TColor);
		procedure SetBorderColor(NewColor: TColor);
		procedure SetBorderWidth(NewWidth: Integer);
	public
		constructor Create(AOwner: ThKnob);
	published
		property FillColor:   TColor  read FFillColor   write SetFillColor   stored True;
		property BorderColor: TColor  read FBorderColor write SetBorderColor stored True;
		property BorderWidth: Integer read FBorderWidth write SetBorderWidth stored True;
	end;

	TKnobIndicator = class(TCollectionItem)
	private
		FOwner:        TIndicators;
		FKnob:         ThKnob;

		FVisible:      Boolean;
		FPosition:     Integer;

		FLineColor:    TColor;
		FLineWidth:    Integer;
		FInnerRadius,
		FOuterRadius:  Integer;
	protected
		procedure Changed;

		procedure SetVisible(Value: Boolean);
		procedure SetPosition(Value: Integer);

		procedure SetLineColor  (Value: TColor);
		procedure SetLineWidth  (Value: Integer);
		procedure SetInnerRadius(Value: Integer);
		procedure SetOuterRadius(Value: Integer);
	public
		constructor Create(AOwner: TCollection); override;
	published
		property Visible:     Boolean read FVisible     write SetVisible     stored True;
		property Position:    Integer read FPosition    write SetPosition    stored True;

		property LineColor:   TColor  read FLineColor   write SetLineColor   stored True;
		property LineWidth:   Integer read FLineWidth   write SetLineWidth   stored True;
		property RadiusInner: Integer read FInnerRadius write SetInnerRadius stored True;
		property RadiusOuter: Integer read FOuterRadius write SetOuterRadius stored True;
	end;

	TKnobIndicators = class(TPersistent)
	private
		FOwner:           ThKnob;
		FOnChange:        TNotifyEvent;

		FExtraIndicators: TIndicators;
		FDummy:           TKnobIndicator;

		FLineColor:       TColor;
		FLineWidth:       Integer;
		FInnerRadius,
		FOuterRadius:     Integer;
		FOffsetX,
		FOffsetY:         Integer; // pixel offset bias of indicator

		FDefault,
		FFocused,
		FUnfocused,
		FChanging,
		FHovered,
		FDisabled:        TColor;
	protected
		procedure SetLineColor  (Value: TColor);
		procedure SetLineWidth  (Value: Integer);
		procedure SetInnerRadius(Value: Integer);
		procedure SetOuterRadius(Value: Integer);
		procedure SetOffsetX    (Value: Integer);
		procedure SetOffsetY    (Value: Integer);
	public
		constructor Create(AOwner: ThKnob);
		destructor  Destroy; override;

		function  AddIndicator: TKnobIndicator;

		procedure Changed;
	published
		property OnChange:  TNotifyEvent read FOnChange write FOnChange;

		property ExtraIndicators: TIndicators read FExtraIndicators write FExtraIndicators stored True;

		property LineColor:   TColor  read FLineColor   write SetLineColor   stored True;
		property LineWidth:   Integer read FLineWidth   write SetLineWidth   stored True;
		property RadiusInner: Integer read FInnerRadius write SetInnerRadius stored True;
		property RadiusOuter: Integer read FOuterRadius write SetOuterRadius stored True;
		property OffsetX:     Integer read FOffsetX     write SetOffsetX stored True;
		property OffsetY:     Integer read FOffsetY     write SetOffsetY stored True;

		property Default:   TColor read FDefault   write FDefault   stored True;
		property Focused:   TColor read FFocused   write FFocused   stored True;
		property Unfocused: TColor read FUnfocused write FUnfocused stored True;
		property Changing:  TColor read FChanging  write FChanging  stored True;
		property Hovered:   TColor read FHovered   write FHovered   stored True;
		property Disabled:  TColor read FDisabled  write FDisabled  stored True;
	end;

	ThKnob = class(TBGRAGraphicCtrl)
	private
		Buffer:   TBGRABitmap;
		FDrawing: Boolean;

		FKnob:       TKnobBase;
		FIndicators: TKnobIndicators;
		FIndicator:  TKnobIndicator;
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
		FMouseAngle: Integer;		{ The current mouse 'angle' over the knob }
		FArc: Word;
		FDragging: Boolean;			{ Knob position is being 'changed' }
		FSensitivity: Integer;		{ Movement area when fVerticalMove }
		FMultiplier: Integer;		{ Multiplier for integer/float conversion }
		FCursorHide: Boolean;		{ Hide mouse pointer when fVerticalMove? }

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
		function  CalcAngle(APosition: Integer): Integer;
		function  CalcPosition(TheAngle: Integer): Integer;

		procedure SetPositionLabel(const NewLabel: TLabel);
		procedure ShowPosition(const ThePosition: Integer); overload;
		procedure SetSpringLoaded(const Sprung: Boolean);

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
	public
		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

		procedure ShowPosition; overload;
		function  GetExtraIndicator(Index: Integer): TKnobIndicator;
	published
		property Knob:       TKnobBase       read FKnob       write FKnob        stored True;
		property Indicators: TKnobIndicators read FIndicators write FIndicators  stored True;
		//property ExtraIndicators
		property Anchors;
		property Align;
		property BorderSpacing;
		property ParentShowHint;
		property ShowHint;
		property Min: Integer read FMin write SetMin default 0;
		property Max: Integer read FMax write SetMax default 100;
		property SmallChange: Integer read FSmallChange write FSmallChange default 1;
		property Caption;
		property Arc: Word read FArc write SetArc;
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
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	Math, LCLIntf, LCLType;

procedure Register;
begin
	RegisterComponents('Decks', [ThKnob]);
end;

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

	FKnob := TKnobBase.Create(Self);
	FIndicators := TKnobIndicators.Create(Self);

	FIndicator := TKnobIndicator.Create(nil);

	FIndicator.FLineColor := FIndicators.FDefault;
	FIndicator.FVisible := True;

	// ControlStyle := ControlStyle - [csOpaque];
	Buffer := TBGRABitmap.Create(ClientWidth, ClientHeight);

	SetSteps;
end;

destructor ThKnob.Destroy;
begin
	FKnob.Free;
	FIndicators.Free;
	FIndicator.Free;
	Buffer.Free;

	inherited Destroy;
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
		Invalidate;
	end;
end;

// Calculate characteristics of knob when Position, Max or Min are changed
procedure ThKnob.SetParams(APosition, AMin, AMax: Integer);
begin
	if FMin <> AMin then
	begin
		FMin := AMin;
		SetSteps; // updates FSteps and FAngleInterval
	end;
	if FMax <> AMax then
	begin
		FMax := AMax;
		SetSteps; // updates FSteps and FAngleInterval
	end;
	if FAngleInterval >= 0 then // Max is greater than Min
	begin
		APosition := EnsureRange(APosition, AMin, AMax);
	end else
	begin						// Min is Greater than Max
		if APosition > AMin then APosition := AMax;
		if APosition < AMax then APosition := AMin;
	end;
	if FPosition <> APosition then FPosition := APosition;
	FIndicator.FPosition := FPosition;

	ShowPosition(FPosition); // update the PositionLabel caption
	Invalidate;

	// fire the OnChange event if not in Designing state
	if (Assigned(FOnChange)) and not (csDesigning in ComponentState) then
		FOnChange(Self);
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
		FIndicator.FLineColor := FIndicators.FHovered;
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
			FIndicator.FLineColor := FIndicators.FDefault;
		//else
		//	FIndicatorColor := @FIndicator.FInactive;
		Invalidate;
	end;
	inherited;
end;

procedure ThKnob.CMEnabledChanged(var Msg: TLMessage);
begin
	if not Enabled then
		FIndicator.FLineColor := FIndicators.FDisabled
	else
		FIndicator.FLineColor := FIndicators.FDefault;
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
	ii := Width div 2;
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
			{if (fMouseAngle > (180-FArc)) and (fMouseAngle <= (180+FArc)) then
				fMouseAngle := 180;
			if (fMouseAngle < -FArc) then
				fMouseAngle := -FArc;}
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
function ThKnob.CalcAngle(APosition: Integer): Integer;
begin
	Result := (180 + FArc) - Round((APosition - FMin) * FAngleInterval);
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
		begin
			n := Math.Max(0, (180 + FArc) - TheAngle);
			if n > 0 then
				Result := Round(fMin + n / fAngleInterval)
			else
				Result := fMin;
		end;
	end;
end;

procedure ThKnob.Draw;
var
	Radius: Single;

	procedure DrawIndicator(const Ind: TKnobIndicator);
	var
		AngleInRadians, CosAngle, SinAngle: Single;
	begin
		if (Ind = nil) or (not Ind.FVisible) or
			(Ind.FLineWidth <= 0) or (Ind.FLineColor = clNone) then Exit;

		AngleInRadians := CalcAngle(Ind.FPosition) * Pi / 180;
		CosAngle := Cos(AngleInRadians);
		SinAngle := Sin(AngleInRadians);
		Buffer.DrawLineAntialias(
			Radius + ((Radius-Ind.FOuterRadius) * CosAngle) + FIndicators.FOffsetX,
			Radius - ((Radius-Ind.FOuterRadius) * SinAngle) + FIndicators.FOffsetY,
			Radius + ((Radius-Ind.FInnerRadius) * CosAngle) + FIndicators.FOffsetX,
			Radius - ((Radius-Ind.FInnerRadius) * SinAngle) + FIndicators.FOffsetY,
			ColorToBGRA(Ind.FLineColor), Ind.FLineWidth);
	end;

var
	i: Integer;
	Save: Boolean;
	FillColor: TBGRAPixel;
begin
	Save := FDrawing;
	FDrawing := True;

	Radius := (ClientWidth-1) / 2; //(Math.Min(ClientWidth, ClientHeight) - 1) / 2;

	Buffer.Fill(BGRAPixelTransparent);

	if FKnob.FFillColor = clNone then
		FillColor := BGRAPixelTransparent
	else
		FillColor := ColorToBGRA(FKnob.FFillColor);

	if (FKnob.FBorderWidth < 1) or (FKnob.FBorderColor = clNone) then
	begin
		Buffer.FillEllipseAntialias(Radius, Radius, Radius, Radius, FillColor);
		if FKnob.FBorderColor <> clNone then
			Buffer.EllipseAntialias(Radius, Radius, Radius, Radius,
				ColorToBGRA(FKnob.FBorderColor), FKnob.FBorderWidth);
	end
	else
	begin
		Buffer.Arc(Radius, Radius, Radius-FKnob.FBorderWidth, Radius-FKnob.FBorderWidth,
			DegToRad(0-FArc), DegToRad(180+FArc),
			ColorToBGRA(FKnob.FBorderColor),
			FKnob.FBorderWidth, False, FillColor);
	end;

	for i := 0 to Indicators.ExtraIndicators.Count-1 do
		DrawIndicator(Indicators.ExtraIndicators.Items[i]);
	DrawIndicator(FIndicator);

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

function ThKnob.GetExtraIndicator(Index: Integer): TKnobIndicator;
var
	C: Integer;
begin
	C := Indicators.ExtraIndicators.Count;
	if Index < 0 then Index := C-1;
	if (C > 0) and (Index >= 0) and (Index < C) then
		Result := Indicators.ExtraIndicators.Items[Index]
	else
		Result := Indicators.FDummy;
end;

// ======================================================================================
// TKnobBase
// ======================================================================================

constructor TKnobBase.Create(AOwner: ThKnob);
begin
	inherited Create;

	FOwner := AOwner;
	FBorderColor := clGray;
	FBorderWidth := 3;
	FFillColor   := clNone;
end;

procedure TKnobBase.Changed;
begin
	if FOwner <> nil then FOwner.Changed;
end;

procedure TKnobBase.SetFillColor(NewColor: TColor);
begin
	if FFillColor <> NewColor then
	begin
		FFillColor := NewColor;
		Changed;
	end;
end;

procedure TKnobBase.SetBorderColor(NewColor: TColor);
begin
	if FBorderColor <> NewColor then
	begin
		FBorderColor := NewColor;
		Changed;
	end;
end;

procedure TKnobBase.SetBorderWidth(NewWidth: Integer);
begin
	if FBorderWidth <> NewWidth then
	begin
		FBorderWidth := NewWidth;
		Changed;
	end;
end;

// ======================================================================================
// TKnobIndicators
// ======================================================================================

constructor TKnobIndicators.Create(AOwner: ThKnob);
begin
	inherited Create;

	FOwner := AOwner;
	FExtraIndicators := TIndicators.Create(AOwner);

	FDummy     := TKnobIndicator.Create(nil);
	FDummy.FKnob := AOwner;

	FDefault   := clSilver;
	FFocused   := clWhite;
	FUnfocused := clSilver;
	FDisabled  := clGray;
	FHovered   := clWhite;
end;

destructor TKnobIndicators.Destroy;
begin
	FExtraIndicators.Free;
	FDummy.Free;

	inherited Destroy;
end;

function TKnobIndicators.AddIndicator: TKnobIndicator;
begin
	Result := FExtraIndicators.Add;
	if Result <> nil then
	with Result do
	begin
		FKnob        := Self.FOwner;
		FLineColor   := clGray;
		FLineWidth   := 4;
		FInnerRadius := 14;
		FOuterRadius := 2;
		FPosition    := Result.FOwner.FKnob.Position;
	end;
end;

procedure TKnobIndicators.Changed;
begin
	if (FOwner <> nil) and (FOwner.FIndicator <> nil) then
	begin
		FOwner.FIndicator.FLineColor   := FLineColor;
		FOwner.FIndicator.FLineWidth   := FLineWidth;
		FOwner.FIndicator.FInnerRadius := FInnerRadius;
		FOwner.FIndicator.FOuterRadius := FOuterRadius;
	end;
	if Assigned(FOnChange) then FOnChange(Self);
	FOwner.Invalidate;
end;

procedure TKnobIndicators.SetLineColor(Value: TColor);
begin
	if Value <> FLineColor then
	begin
		FLineColor := Value;
		Changed;
	end;
end;

procedure TKnobIndicators.SetLineWidth(Value: Integer);
begin
	if Value <> FLineWidth then
	begin
		FLineWidth := Value;
		Changed;
	end;
end;

procedure TKnobIndicators.SetInnerRadius(Value: Integer);
begin
	if Value <> FInnerRadius then
	begin
		FInnerRadius := Value;
		Changed;
	end;
end;

procedure TKnobIndicators.SetOuterRadius(Value: Integer);
begin
	if Value <> FOuterRadius then
	begin
		FOuterRadius := Value;
		Changed;
	end;
end;

procedure TKnobIndicators.SetOffsetX(Value: Integer);
begin
	if Value <> FOffsetX then
	begin
		FOffsetX := Value;
		Changed;
	end;
end;

procedure TKnobIndicators.SetOffsetY(Value: Integer);
begin
	if Value <> FOffsetY then
	begin
		FOffsetY := Value;
		Changed;
	end;
end;

// ======================================================================================
// TIndicators
// ======================================================================================

constructor TIndicators.Create(AOwner: ThKnob);
begin
	inherited Create(AOwner, TKnobIndicator);
	FKnob := AOwner;
end;

function TIndicators.Add: TKnobIndicator;
begin
	Result := TKnobIndicator(inherited Add);
end;

procedure TIndicators.Changed;
begin
	inherited Changed;
	if FKnob <> nil then
		FKnob.Invalidate;
end;

function TIndicators.GetItem(Index: Integer): TKnobIndicator;
begin
	Result := TKnobIndicator(inherited GetItem(Index));
end;

procedure TIndicators.SetItem(Index: Integer; const Value: TKnobIndicator);
begin
	inherited SetItem(Index, Value);
end;

// ======================================================================================
// TKnobIndicator
// ======================================================================================

constructor TKnobIndicator.Create(AOwner: TCollection);
begin
	inherited Create(AOwner);

	FOwner       := TIndicators(AOwner);
	FVisible     := True;
	FPosition    := 0;
	FLineColor   := clGray;
	FLineWidth   := 3;
	FInnerRadius := 4;
	FOuterRadius := 14;
end;

procedure TKnobIndicator.Changed;
begin
	if FOwner <> nil then FOwner.Changed;
end;

procedure TKnobIndicator.SetLineColor(Value: TColor);
begin
	if Value <> FLineColor then
	begin
		FLineColor := Value;
		Changed;
	end;
end;

procedure TKnobIndicator.SetLineWidth(Value: Integer);
begin
	if Value <> FLineWidth then
	begin
		FLineWidth := Value;
		Changed;
	end;
end;

procedure TKnobIndicator.SetInnerRadius(Value: Integer);
begin
	if Value <> FInnerRadius then
	begin
		FInnerRadius := Value;
		Changed;
	end;
end;

procedure TKnobIndicator.SetOuterRadius(Value: Integer);
begin
	if Value <> FOuterRadius then
	begin
		FOuterRadius := Value;
		Changed;
	end;
end;

procedure TKnobIndicator.SetVisible(Value: Boolean);
begin
	if FVisible <> Value then
	begin
		FVisible := Value;
		Changed;
	end;
end;

procedure TKnobIndicator.SetPosition(Value: Integer);
begin
	if FPosition <> Value then
	begin
		FPosition := Value;
		Changed;
	end;
end;

end.

