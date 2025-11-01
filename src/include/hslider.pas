unit hSlider;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf <Andre@metaException.de>
 * Marc Lafon
 *
 * ***** END LICENSE BLOCK ***** *)
interface

{$MODE DELPHI}

uses
	{$IFDEF FPC}
	LCLIntf, LMessages, LCLType, Graphics, Controls, Forms, Dialogs, ExtCtrls,
	{$IFDEF Windows} Windows, {$ENDIF}
	{$ELSE}
	Windows, Messages, {$IFDEF INLININGSUPPORTED}Types,{$ENDIF}
	Graphics, Controls, Forms, Dialogs, ExtCtrls,
	{$ENDIF}
	SysUtils, Classes;

{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6060 off : Case statement does not handle all possible cases}

type
	TRBIncrement = 1..32768;

	TRBDirection = (drLeft, drUp, drRight, drDown);

	TRBDirections = set of TRBDirection;

	TRBZone = (zNone, zBtnPrev, zTrackPrev, zHandle, zTrackNext, zBtnNext);

	TRBGetSizeEvent = procedure(Sender: TObject; var Size: Integer) of object;

	TDecksSlider = class(TCustomControl)
	private
		FBorderStyle: TBorderStyle;
		FButtonSize: Integer;
		FHandleColor: TColor;
		FButtoncolor:TColor;
		FHighLightColor:TColor;
		FShadowColor:TColor;
		FBorderColor:TColor;
		FLightAmount: Integer; // hover contrast
		FArrowColor: TColor;  // arrow glyph color
		FKind: TScrollBarKind;
		FShowArrows: Boolean;
		FFlat: Boolean;
		FWrapAround: Boolean;
		FOnChange: TNotifyEvent;
		FOnUserChange: TNotifyEvent;

		procedure SetButtonSize(Value: Integer);
		procedure SetHandleColor(Value: TColor);
		procedure SetHighLightColor(Value: TColor);
		procedure SetShadowColor(Value: TColor);
		procedure SetButtonColor(Value: TColor);
		procedure SetBorderColor(Value: TColor);
		procedure SetArrowColor(Value: TColor);
		procedure SetLightAmount(Value: Integer);
		procedure SetKind(Value: TScrollBarKind);
		procedure SetShowArrows(Value: Boolean);
		procedure SetFlat(Value: Boolean);

		{$IFDEF FPC}
		procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
		procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
		procedure WMNCCalcSize(var Message: TLMNCCalcSize); message LM_NCCALCSIZE;
		procedure WMEraseBkgnd(var Message: TLmEraseBkgnd); message LM_ERASEBKGND;
		{$IFDEF Windows}
		procedure WMNCPaint(var Message: TWMNCPaint); message LM_NCPAINT;
		{$ENDIF}
		{$ELSE}
		procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
		procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
		procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
		{$ENDIF}
	protected
		FGenChange: Boolean;
		FDragZone: TRBZone;
		FHotZone: TRBZone;
		FTimer: TTimer;
		FTimerMode: Integer;
		FStored: TPoint;
		FPosBeforeDrag: Single;

		procedure DoChange; virtual;
		procedure DoDrawButton(R: TRect; Direction: TRBDirection; Pushed, Enabled, Hot: Boolean); virtual;
		procedure DoDrawHandle(R: TRect; Horz: Boolean; Pushed, Hot: Boolean); virtual;
		procedure DoDrawTrack(R: TRect; Direction: TRBDirection; Pushed, Enabled, Hot: Boolean); virtual;

		function  DrawEnabled: Boolean; virtual;
		function  GetBorderSize: Integer;
		function  GetHandleRect: TRect; virtual;
		function  GetButtonSize: Integer;
		function  GetTrackBoundary: TRect;
		function  GetZone(X, Y: Integer): TRBZone;
		function  GetZoneRect(Zone: TRBZone): TRect;

		procedure MouseLeft; virtual;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure Paint; override;
		procedure StartDragTracking;
		procedure StartHotTracking;
		procedure StopDragTracking;
		procedure StopHotTracking;
		procedure TimerHandler(Sender: TObject); virtual;
		procedure SetBorderStyle(Value: TBorderStyle); {$IFDEF FPC} override; {$ENDIF}
	public
		constructor Create(AOwner: TComponent); override;

		property ArrowColor: TColor read FArrowColor write SetArrowColor default clBtnText;
		property Color default clScrollBar;
		property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
		property ButtonSize: Integer read FButtonSize write SetButtonSize default 0;
		property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnFace;
		property BorderColor: TColor read FBorderColor write SetBorderColor default clWindowFrame;
		property HandleColor: TColor read FHandleColor write SetHandleColor default clBtnShadow;
		property HighLightColor: TColor read FHighLightColor write SetHighLightColor default clBtnHighlight;
		property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
		property LightAmount: Integer read FLightAmount write SetLightAmount default 12;
		property Flat: Boolean read FFlat write SetFlat default True;
		property WrapAround: Boolean read FWrapAround write FWrapAround default False;
		property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
		property ShowArrows: Boolean read FShowArrows write SetShowArrows default True;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
	end;

	TCustomRangeBar = class(TDecksSlider)
	private
		FCentered: Boolean;
		FEffectiveWindow: Integer;
		FIncrement: TRBIncrement;
		FPosition: Single;
		FRange: Integer;
		FWindow: Integer;

		function  IsPositionStored: Boolean;

		procedure SetPosition(Value: Single);
		procedure SetRange(Value: Integer);
		procedure SetWindow(Value: Integer);
	protected
		function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
		function  DrawEnabled: Boolean; override;
		function  GetHandleRect: TRect; override;

		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure TimerHandler(Sender: TObject); override;
		procedure UpdateEffectiveWindow;
		procedure AdjustPosition;

		property EffectiveWindow: Integer read FEffectiveWindow;
	public
		constructor Create(AOwner: TComponent); override;

		procedure Resize; override;
		procedure SetParams(NewRange, NewWindow: Integer);

		property Centered: Boolean read FCentered write FCentered;
		property Increment: TRBIncrement read FIncrement write FIncrement default 8;
		property Position: Single read FPosition write SetPosition stored IsPositionStored;
		property Range: Integer read FRange write SetRange default 0;
		property Window: Integer read FWindow write SetWindow default 0;
	end;

	TDecksRangeBar = class(TCustomRangeBar)
	published
		property Align;
		property Anchors;
		property Constraints;
		property Color;
		property BorderStyle;
		property ButtonSize;
		property Enabled;
		property HandleColor;
		property ButtonColor;
		property HighLightColor;
		property ShadowColor;
		property BorderColor;
		property ArrowColor;
		property LightAmount;
		property Increment;
		property Kind;
		property Flat;
		property WrapAround;
		property Range;
		property Visible;
		property Window;
		property ShowArrows;
		property Position; // this should be located after the Range property
		property OnChange;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnMouseEnter;
		property OnMouseLeave;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnMouseWheelUp;
		property OnMouseWheelDown;
		property OnStartDrag;
		property OnUserChange;
	end;

	TCustomGaugeBar = class(TDecksSlider)
	private
		FHandleSize: Integer;
		FLargeChange: Integer;
		FMax: Integer;
		FMin: Integer;
		FPosition: Integer;
		FSmallChange: Integer;

		procedure SetHandleSize(Value: Integer);
		procedure SetMax(Value: Integer);
		procedure SetMin(Value: Integer);
		procedure SetPosition(Value: Integer);
		procedure SetLargeChange(Value: Integer);
		procedure SetSmallChange(Value: Integer);
	protected
		function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
		function  GetHandleRect: TRect; override;
		function  GetHandleSize: Integer;

		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure TimerHandler(Sender: TObject); override;
		procedure AdjustPosition;
	public
		constructor Create(AOwner: TComponent); override;

		property HandleSize: Integer read FHandleSize write SetHandleSize default 0;
		property LargeChange: Integer read FLargeChange write SetLargeChange default 1;
		property Max: Integer read FMax write SetMax default 100;
		property Min: Integer read FMin write SetMin default 0;
		property Position: Integer read FPosition write SetPosition;
		property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
		property OnChange;
		property OnUserChange;
	end;

	TDecksGaugeBar = class(TCustomGaugeBar)
	published
		property Align;
		property Anchors;
		property Constraints;
		property Color;
		property BorderStyle;
		property ButtonSize;
		property Enabled;
		property HandleColor;
		property ButtonColor;
		property HighLightColor;
		property ShadowColor;
		property BorderColor;
		property ArrowColor;
		property LightAmount;
		property HandleSize;
		property Kind;
		property Flat;
		property WrapAround;
		property LargeChange;
		property Max;
		property Min;
		property ShowArrows;
		property SmallChange;
		property Visible;
		property Position;
		property OnChange;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnMouseEnter;
		property OnMouseLeave;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseWheelUp;
		property OnMouseWheelDown;
		property OnMouseUp;
		property OnStartDrag;
		property OnUserChange;
	end;

	procedure Register;


implementation

uses
	Math;

const
	OppositeDirection: array [TRBDirection] of TRBDirection = (drRight, drDown, drLeft, drUp);
	tmScrollFirst = 1;
	tmScroll = 2;
	tmHotTrack = 3;

procedure Register;
begin
	RegisterComponents('Decks', [TDecksRangeBar, TDecksGaugeBar]);
end;

function ClrLighten(C: TColor; Amount: Integer): TColor;
var
	R, G, B: Integer;
begin
	{$IFDEF Windows}
	if C < 0 then C := GetSysColor(C and $000000FF);
	{$ELSE}
	C := ColorToRGB(C);
	{$ENDIF}
	R := C and $FF + Amount;
	G := C shr 8 and $FF + Amount;
	B := C shr 16 and $FF + Amount;
	if R < 0 then R := 0 else if R > 255 then R := 255;
	if G < 0 then G := 0 else if G > 255 then G := 255;
	if B < 0 then B := 0 else if B > 255 then B := 255;
	Result := R or (G shl 8) or (B shl 16);
end;

function MixColors(C1, C2: TColor; W1: Integer): TColor;
var
	W2: Cardinal;
begin
	Assert(W1 in [0..255]);
	W2 := W1 xor 255;
	{$IFDEF Windows}
	if Integer(C1) < 0 then C1 := GetSysColor(C1 and $000000FF);
	if Integer(C2) < 0 then C2 := GetSysColor(C2 and $000000FF);
	{$ELSE}
	C1 := ColorToRGB(C1);
	C2 := ColorToRGB(C2);
	{$ENDIF}
	Result := Integer(
		((Cardinal(C1) and $FF00FF) * Cardinal(W1) +
		(Cardinal(C2) and $FF00FF) * W2) and $FF00FF00 +
		((Cardinal(C1) and $00FF00) * Cardinal(W1) +
		(Cardinal(C2) and $00FF00) * W2) and $00FF0000) shr 8;
end;

procedure DitherRect(Canvas: TCanvas; const R: TRect; C: TColor);
var
	{$IFDEF FPC}
	Brush: TBrush;
	OldBrush: TBrush;
	{$ELSE}
	B: TBitmap;
	Brush: HBRUSH;
	{$ENDIF}
begin
	if R.IsEmpty then Exit;
	{$IFDEF FPC}
	Brush := TBrush.Create;
	try
		Brush.Color := ColorToRGB(C);
		OldBrush := TBrush.Create;
		try
			OldBrush.Assign(Canvas.Brush);
			Canvas.Brush.Assign(Brush);
			Canvas.FillRect(R);
			Canvas.Brush.Assign(OldBrush);
		finally
			OldBrush.Free;
		end;
	finally
		if Assigned(Brush.Bitmap) then
			Brush.Bitmap.Free;
		Brush.Free;
	end;
	{$ELSE}
	Brush := CreateSolidBrush(ColorToRGB(C));
	FillRect(Canvas.Handle, R, Brush);
	DeleteObject(Brush);
	{$ENDIF}
end;

procedure DrawRectEx(Canvas: TCanvas; var R: TRect; Sides: TRBDirections; C: TColor);
begin
	if Sides = [] then Exit;

	with Canvas, R do
	begin
		Pen.Color := C;

		if drUp in Sides then
		begin
			MoveTo(Left, Top); LineTo(Right, Top); Inc(Top);
		end;
		if drDown in Sides then
		begin
			Dec(Bottom); MoveTo(Left, Bottom); LineTo(Right, Bottom);
		end;
		if drLeft in Sides then
		begin
			MoveTo(Left, Top); LineTo(Left, Bottom); Inc(Left);
		end;
		if drRight in Sides then
		begin
			Dec(Right); MoveTo(Right, Top); LineTo(Right, Bottom);
		end;
	end;
end;

procedure Frame3D(Canvas: TCanvas; var ARect: TRect; TopColor, BottomColor: TColor; AdjustRect: Boolean = True);
var
	TopRight, BottomLeft: TPoint;
begin
	with Canvas, ARect do
	begin
		Pen.Width := 1;
		Dec(Bottom); Dec(Right);
		TopRight.X := Right;
		TopRight.Y := Top;
		BottomLeft.X := Left;
		BottomLeft.Y := Bottom;
		Pen.Color := TopColor;
		PolyLine([BottomLeft, TopLeft, TopRight]);
		Pen.Color := BottomColor;
		Dec(Left);
		PolyLine([TopRight, BottomRight, BottomLeft]);
		if AdjustRect then
		begin
			Inc(Top); Inc(Left, 2);
		end
		else
		begin
			Inc(Left); Inc(Bottom); Inc(Right);
		end;
	end;
end;

procedure DrawArrow(Canvas: TCanvas; R: TRect;
	Direction: TRBDirection; Color: TColor);
var
	X, Y, Sz, Shift: Integer;
begin
	X := (R.Left + R.Right - 1) div 2;
	Y := (R.Top + R.Bottom - 1) div 2;
	Sz := (Min(X - R.Left, Y - R.Top)) * 3 div 4 - 1;
	if Sz = 0 then Sz := 1;
	if Direction in [drUp, drLeft] then
		Shift := (Sz + 1) * 1 div 3
	else
		Shift := Sz * 1 div 3;
	Canvas.Pen.Color := Color;
	Canvas.Brush.Color := Color;

	case Direction of
		drUp:
		begin
			Inc(Y, Shift);
			Canvas.Polygon([Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)]);
		end;
		drDown:
		begin
			Dec(Y, Shift);
			Canvas.Polygon([Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)]);
		end;
		drLeft:
		begin
			Inc(X, Shift);
			Canvas.Polygon([Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)]);
		end;
		drRight:
		begin
			Dec(X, Shift);
			Canvas.Polygon([Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)]);
		end;
	end;
end;

const
	FIRST_DELAY = 600;
	SCROLL_INTERVAL = 100;
	HOTTRACK_INTERVAL = 150;
	MIN_SIZE = 17;

{ TDecksSlider }

{$IFDEF FPC}
procedure TDecksSlider.CMEnabledChanged(var Message: TLMessage);
{$ELSE}
procedure TDecksSlider.CMEnabledChanged(var Message: TMessage);
{$ENDIF}
begin
	inherited;
	Invalidate;
end;

{$IFDEF FPC}
procedure TDecksSlider.CMMouseLeave(var Message: TLMessage);
{$ELSE}
procedure TDecksSlider.CMMouseLeave(var Message: TMessage);
{$ENDIF}
begin
	MouseLeft;
	inherited;
end;

constructor TDecksSlider.Create(AOwner: TComponent);
begin
	inherited;

	ControlStyle := ControlStyle - [csAcceptsControls, csDoubleClicks] + [csOpaque];
	Width := 100;
	Height := 16;
	ParentColor := False;
	Color := clScrollBar;
	FTimer := TTimer.Create(Self);
	FTimer.OnTimer := TimerHandler;
	FShowArrows := True;
	FFlat := True;
	FBorderStyle := bsSingle;
	FHandleColor := clBtnShadow;
	FButtonColor := clBtnFace;
	FHighLightColor := clBtnHighlight;
	FShadowColor := clBtnShadow;
	FBorderColor := clWindowFrame;
	FArrowColor := clBtnText;
	FLightAmount := 12;
end;

procedure TDecksSlider.DoChange;
begin
	if Assigned(FOnChange) then FOnChange(Self);
	if FGenChange and Assigned(FOnUserChange) then FOnUserChange(Self);
end;

procedure TDecksSlider.DoDrawButton(R: TRect; Direction: TRBDirection; Pushed, Enabled, Hot: Boolean);
var
	Edges: TRBDirections;
begin
	Edges := [drLeft, drUp, drRight, drDown];
	Exclude(Edges, OppositeDirection[Direction]);

	if not DrawEnabled then
	begin
		DrawRectEx(Canvas, R, Edges, ShadowColor);
		Canvas.Brush.Color := Color; //ButtonColor;
		FillRect(Canvas.Handle, R, Canvas.Brush.Reference.Handle);
//		GR32.InflateRect(R, -1, -1);
//		GR32.OffsetRect(R, 1, 1);
//		DrawArrow(Canvas, R, Direction, HighLightColor);
//		GR32.OffsetRect(R, -1, -1);
		DrawArrow(Canvas, R, Direction, ShadowColor);
	end
	else
	begin
		DrawRectEx(Canvas, R, Edges, BorderColor);
		if Hot then
			Canvas.Brush.Color := ClrLighten(ButtonColor, FLightAmount)
		else
			Canvas.Brush.Color := ButtonColor;

		if Pushed then
		begin
			FillRect(Canvas.Handle, R, Canvas.Brush.Reference.Handle);
			R.Offset(1, 1);
			R.Inflate(-1, -1);
		end
		else
		begin
			if not FFlat then
				Frame3D(Canvas, R, HighLightColor, ShadowColor, True);
			FillRect(Canvas.Handle, R, Canvas.Brush.Reference.Handle);
		end;
		DrawArrow(Canvas, R, Direction, FArrowColor);
	end;
end;

procedure TDecksSlider.DoDrawHandle(R: TRect; Horz, Pushed, Hot: Boolean);
var
	C: TColor;
begin
	if Hot then
		C := ClrLighten(HandleColor, FLightAmount)
	else
		C := HandleColor;

	Canvas.Brush.Color := BorderColor;
	FrameRect(Canvas.Handle, R, Canvas.Brush.Reference.Handle);

	R.Inflate(-1, -1);
	if not FFlat then
	begin
		if Pushed then
			Frame3D(Canvas, R, ShadowColor, C)
		else
			Frame3D(Canvas, R, HighLightColor, ShadowColor);
	end;
	Canvas.Brush.Color := C;
	Canvas.FillRect(R);
end;


procedure TDecksSlider.DoDrawTrack(R: TRect; Direction: TRBDirection; Pushed, Enabled, Hot: Boolean);
var
	C: TColor;
	Edges: set of TRBDirection;
begin
	if (R.Right > R.Left) and (R.Bottom > R.Top) then
	with Canvas, R do
	begin
		if DrawEnabled then
			C := BorderColor
		else
			C := ShadowColor;
		Edges := [drLeft, drUp, drRight, drDown];
		Exclude(Edges, OppositeDirection[Direction]);
		DrawRectEx(Canvas, R, Edges, C);
		if Pushed then
		begin
			C := ClrLighten(Color, -FLightAmount);
			DitherRect(Canvas, R, C);
		end
		else
		if not R.IsEmpty then
		with R do
		begin
			if DrawEnabled then
			begin
				Pen.Color := Color; //MixColors(fBorderColor, MixColors(fHighLightColor, Color, 127), 32);

				case Direction of
					drLeft, drUp:
					begin
						MoveTo(Left, Bottom - 1); LineTo(Left, Top); LineTo(Right, Top);
						Inc(Top); Inc(Left);
					end;
					drRight:
					begin
						MoveTo(Left, Top); LineTo(Right, Top);
						Inc(Top);
					end;
					drDown:
					begin
						MoveTo(Left, Top); LineTo(Left, Bottom);
						Inc(Left);
					end;
				end;

				C := Color;
				if Hot then
					C := ClrLighten(C, FLightAmount);
				DitherRect(Canvas, R, C);
			end
			else // disabled
			begin
				Brush.Color := Color; //ButtonColor;
				FillRect(R);
			end;
		end; // R
	end; // with
end;

function TDecksSlider.DrawEnabled: Boolean;
begin
	Result := Enabled;
end;

function TDecksSlider.GetBorderSize: Integer;
const
	CSize: array [Boolean] of Integer = (0, 1);
begin
	Result := CSize[BorderStyle = bsSingle];
end;

function TDecksSlider.GetButtonSize: Integer;
var
	W, H: Integer;
begin
	if not ShowArrows then
		Result := 0
	else
	begin
		Result := ButtonSize;
		if Kind = sbHorizontal then
		begin
			W := ClientWidth;
			H := ClientHeight;
		end
		else
		begin
			W := ClientHeight;
			H := ClientWidth;
		end;
		if Result = 0 then Result := Min(H, 32);
		if Result * 2 >= W then Result := W div 2;
		Dec(Result); // !!! Mac
		if Result < 2 then Result := 0;
	end;
end;

function TDecksSlider.GetHandleRect: TRect;
begin
	Result := Rect(0, 0, 0, 0);
end;

function TDecksSlider.GetTrackBoundary: TRect;
begin
	Result := ClientRect;
	if Kind = sbHorizontal then
		Result.Inflate(-GetButtonSize, 0)
	else
		Result.Inflate(0, -GetButtonSize);
end;

function TDecksSlider.GetZone(X, Y: Integer): TRBZone;
var
	P: TPoint;
	R, R1: TRect;
	Sz: Integer;
begin
	Result := zNone;

	P := Point(X, Y);
	R := ClientRect;
	if not R.Contains(P) then Exit;

	Sz := GetButtonSize;
	R1 := R;
	if Kind = sbHorizontal then
	begin
		R1.Right := R1.Left + Sz;
		if R1.Contains(P) then
			Result := zBtnPrev
		else
		begin
			R1.Right := R.Right;
			R1.Left := R.Right - Sz;
			if R1.Contains(P) then Result := zBtnNext;
		end;
	end
	else
	begin
		R1.Bottom := R1.Top + Sz;
		if R1.Contains(P) then
			Result := zBtnPrev
		else
		begin
			R1.Bottom := R.Bottom;
			R1.Top := R.Bottom - Sz;
			if R1.Contains(P) then Result := zBtnNext;
		end;
	end;

	if Result = zNone then
	begin
		R := GetHandleRect;
		P := Point(X, Y);
		if R.Contains(P) then
			Result := zHandle
		else
		begin
			if Kind = sbHorizontal then
			begin
				if (X > 0) and (X < R.Left) then
					Result := zTrackPrev
				else
				if (X >= R.Right) and (X < ClientWidth - 1) then
					Result := zTrackNext;
			end
			else
			begin
				if (Y > 0) and (Y < R.Top) then
					Result := zTrackPrev
				else
				if (Y >= R.Bottom) and (Y < ClientHeight - 1) then
					Result := zTrackNext;
			end;
		end;
	end;
end;

function TDecksSlider.GetZoneRect(Zone: TRBZone): TRect;
const
	CEmptyRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
	BtnSize: Integer;
	Horz: Boolean;
	R: TRect;
begin
	Horz := Kind = sbHorizontal;
	BtnSize:= GetButtonSize;

	case Zone of

		zNone:
			Result := CEmptyRect;

		zBtnPrev:
		begin
			Result := ClientRect;
			if Horz then Result.Right := Result.Left + BtnSize
			else Result.Bottom := Result.Top + BtnSize;
		end;

		zTrackPrev..zTrackNext:
		begin
			Result := GetTrackBoundary;
			R := GetHandleRect;
			if not DrawEnabled or R.IsEmpty then
			begin
				R.Left := (Result.Left + Result.Right) div 2;
				R.Top := (Result.Top + Result.Bottom) div 2;
				R.Right := R.Left;
				R.Bottom := R.Top;
			end;
			case Zone of
				zTrackPrev:
					if Horz then Result.Right := R.Left
					else Result.Bottom := R.Top;
				zHandle:
					Result := R;
				zTrackNext:
					if Horz then Result.Left := R.Right
					else Result.Top := R.Bottom;
			end;
		end;

		zBtnNext:
		begin
			Result := ClientRect;
			if Horz then Result.Left := Result.Right - BtnSize
			else Result.Top := Result.Bottom - BtnSize;
		end;

	end;
end;

procedure TDecksSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	inherited;

	if Button = mbLeft then
	begin
		FDragZone := GetZone(X, Y);
		Invalidate;
		FStored.X := X;
		FStored.Y := Y;
		StartDragTracking;
	end;
end;

procedure TDecksSlider.MouseLeft;
begin
  StopHotTracking;
end;

procedure TDecksSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	NewHotZone: TRBZone;
begin
	inherited;

	if (FDragZone = zNone) and DrawEnabled then
	begin
		NewHotZone := GetZone(X, Y);
		if NewHotZone <> FHotZone then
		begin
			FHotZone := NewHotZone;
			if FHotZone <> zNone then StartHotTracking;
			Invalidate;
		end;
	end;
end;

procedure TDecksSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	inherited;

	FDragZone := zNone;
	Invalidate;
	StopDragTracking;
	if Assigned(OnMouseUp) then
		OnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TDecksSlider.Paint;
const
	CPrevDirs: array [Boolean] of TRBDirection = (drUp, drLeft);
	CNextDirs: array [Boolean] of TRBDirection = (drDown, drRight);
var
	BSize: Integer;
	ShowEnabled: Boolean;
	R, BtnRect, HandleRect: TRect;
	Horz, ShowHandle: Boolean;
begin
	R := ClientRect;
	Horz := Kind = sbHorizontal;
	ShowEnabled := DrawEnabled;
	BSize := GetButtonSize;

	if ShowArrows then
	begin
		{ left / top button }
		BtnRect := R;
		with BtnRect do
			if Horz then Right := Left + BSize else Bottom := Top + BSize;
		DoDrawButton(BtnRect, CPrevDirs[Horz], FDragZone = zBtnPrev, ShowEnabled, FHotZone = zBtnPrev);

		{ right / bottom button }
		BtnRect := R;
		with BtnRect do
			if Horz then Left := Right - BSize else Top := Bottom - BSize;
		DoDrawButton(BtnRect, CNextDirs[Horz], FDragZone = zBtnNext, ShowEnabled, FHotZone = zBtnNext);
	end;

	if Horz then
		R.Inflate(-BSize, 0)
	else
		R.Inflate(0, -BSize);
	if ShowEnabled then
		HandleRect := GetHandleRect
	else
		HandleRect := TRect.Empty;
	ShowHandle := not HandleRect.IsEmpty;

	DoDrawTrack(GetZoneRect(zTrackPrev), CPrevDirs[Horz], FDragZone = zTrackPrev, ShowEnabled, FHotZone = zTrackPrev);
	DoDrawTrack(GetZoneRect(zTrackNext), CNextDirs[Horz], FDragZone = zTrackNext, ShowEnabled, FHotZone = zTrackNext);
	if ShowHandle then
		DoDrawHandle(HandleRect, Horz, FDragZone = zHandle, FHotZone = zHandle);
end;

procedure TDecksSlider.SetBorderStyle(Value: TBorderStyle);
begin
	if Value <> FBorderStyle then
	begin
		FBorderStyle := Value;
		{$IFNDEF FPC}
		RecreateWnd;
		{$ELSE}
		Invalidate;
		{$ENDIF}
	end;
end;

procedure TDecksSlider.SetFlat(Value: Boolean);
begin
	if Value <> FFlat then
	begin
		FFlat := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetButtonSize(Value: Integer);
begin
	if Value <> FButtonSize then
	begin
		FButtonSize := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetHandleColor(Value: TColor);
begin
	if Value <> FHandleColor then
	begin
		FHandleColor := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetHighLightColor(Value: TColor);
begin
	if Value <> FHighLightColor then
	begin
		FHighLightColor := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetButtonColor(Value: TColor);
begin
	if Value <> FButtonColor then
	begin
		FButtonColor := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetBorderColor(Value: TColor);
begin
	if Value <> FBorderColor then
	begin
		FBorderColor := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetShadowColor(Value: TColor);
begin
	if Value <> FShadowColor then
	begin
		FShadowColor := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetArrowColor(Value: TColor);
begin
	if Value <> FArrowColor then
	begin
		FArrowColor := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetLightAmount(Value: Integer);
begin
	if Value <> FLightAmount then
	begin
		FLightAmount := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetKind(Value: TScrollBarKind);
var
	Tmp: Integer;
begin
	if Value <> FKind then
	begin
		FKind := Value;
		if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
		begin
			Tmp := Width;
			Width := Height;
			Height := Tmp;
		end;
		Invalidate;
	end;
end;

procedure TDecksSlider.SetShowArrows(Value: Boolean);
begin
	if Value <> FShowArrows then
	begin
		FShowArrows := Value;
		Invalidate;
	end;
end;

procedure TDecksSlider.StartDragTracking;
begin
	FTimer.Interval := FIRST_DELAY;
	FTimerMode := tmScroll;
	TimerHandler(Self);
	FTimerMode := tmScrollFirst;
	FTimer.Enabled := True;
end;

procedure TDecksSlider.StartHotTracking;
begin
	FTimer.Interval := HOTTRACK_INTERVAL;
	FTimerMode := tmHotTrack;
	FTimer.Enabled := True;
end;

procedure TDecksSlider.StopDragTracking;
begin
	StartHotTracking;
end;

procedure TDecksSlider.StopHotTracking;
begin
	FTimer.Enabled := False;
	FHotZone := zNone;
	Invalidate;
end;

procedure TDecksSlider.TimerHandler(Sender: TObject);
var
	Pt: TPoint;
begin
	case FTimerMode of
		tmScrollFirst:
		begin
			FTimer.Interval := SCROLL_INTERVAL;
			FTimerMode := tmScroll;
		end;
		tmHotTrack:
		begin
			Pt := ScreenToClient(Mouse.CursorPos);
			if not ClientRect.Contains(Pt) then
			begin
				StopHotTracking;
				Invalidate;
			end;
		end;
	end;
end;

{$IFDEF FPC}
procedure TDecksSlider.WMEraseBkgnd(var Message: TLmEraseBkgnd);
begin
	Message.Result := -1;
end;

procedure TDecksSlider.WMNCCalcSize(var Message: TLMNCCalcSize);
var
	Sz: Integer;
begin
	Sz := GetBorderSize;
	Message.CalcSize_Params.rgrc[0].Inflate(-Sz, -Sz);
end;

{$IFDEF Windows}
procedure TDecksSlider.WMNCPaint(var Message: TWMNCPaint);

	procedure DrawNCArea(ADC: HDC; const Clip: HRGN);
	var
		DC: HDC;
		R: TRect;
	begin
		if BorderStyle = bsNone then Exit;

		if ADC = 0 then
			DC := GetWindowDC(Handle)
		else
			DC := ADC;
		try
			GetWindowRect(Handle, R{%H-});
			OffsetRect(R, -R.Left, -R.Top);
			DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT);
		finally
			if ADC = 0 then
				ReleaseDC(Handle, DC);
		end;
	end;

begin
	DrawNCArea(0, Message.RGN);
end;
{$ENDIF}
{$ELSE}
procedure TDecksSlider.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
	Message.Result := -1;
end;

procedure TDecksSlider.WMNCCalcSize(var Message: TWMNCCalcSize);
var
	Sz: Integer;
begin
	Sz := GetBorderSize;
	GR32.InflateRect(Message.CalcSize_Params.rgrc[0], -Sz, -Sz);
end;

procedure TDecksSlider.WMNCPaint(var Message: TWMNCPaint);

	procedure DrawNCArea(ADC: HDC; const Clip: HRGN);
	var
		DC: HDC;
		R: TRect;
	begin
		if BorderStyle = bsNone then Exit;

		if ADC = 0 then
			DC := GetWindowDC(Handle)
		else
			DC := ADC;
		try
			GetWindowRect(Handle, R);
			GR32.OffsetRect(R, -R.Left, -R.Top);
			DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT);
		finally
			if ADC = 0 then
				ReleaseDC(Handle, DC);
		end;
	end;

begin
	DrawNCArea(0, Message.RGN);
end;
{$ENDIF}

{ TCustomRangeBar }

procedure TCustomRangeBar.AdjustPosition;
begin
	if FPosition > Range - FEffectiveWindow then
		FPosition := Range - FEffectiveWindow;
	if FPosition < 0 then
		FPosition := 0;
end;

constructor TCustomRangeBar.Create(AOwner: TComponent);
begin
	inherited;
	FIncrement := 8;
end;

function TCustomRangeBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
const OneHundredTwenteenth = 1 / 120;
begin
	if Kind = sbVertical then WheelDelta := -WheelDelta;
	Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
	if not Result then Position := Position + Increment * WheelDelta * OneHundredTwenteenth;
	Result := True;
end;

function TCustomRangeBar.DrawEnabled: Boolean;
begin
	Result := Enabled and (Range > EffectiveWindow);
end;

function TCustomRangeBar.GetHandleRect: TRect;
var
	BtnSz, ClientSz: Integer;
	HandleSz, HandlePos: Integer;
	R: TRect;
	Horz: Boolean;
begin
	R := Rect(0, 0, ClientWidth, ClientHeight);
	Horz := Kind = sbHorizontal;
	BtnSz := GetButtonSize;

	if Horz then
	begin
		R.Inflate(-BtnSz, 0);
		ClientSz := R.Right - R.Left;
	end
	else
	begin
		R.Inflate(0, -BtnSz);
		ClientSz := R.Bottom - R.Top;
	end;
	if ClientSz < 18 then
	begin
		Result := Rect(0, 0, 0, 0);
		Exit;
	end;

	if Range > EffectiveWindow then
	begin
		HandleSz := Round(ClientSz * EffectiveWindow / Range);
		if HandleSz >= MIN_SIZE then HandlePos := Round(ClientSz * Position / Range)
		else
		begin
			HandleSz := MIN_SIZE;
			HandlePos := Round((ClientSz - MIN_SIZE) * Position / (Range - EffectiveWindow));
		end;

		Result := R;

		if Horz then
		begin
			Result.Left  := R.Left + HandlePos;
			Result.Right := R.Left + HandlePos + HandleSz;
		end
		else
		begin
			Result.Top    := R.Top + HandlePos;
			Result.Bottom := R.Top + HandlePos + HandleSz;
		end;
	end
	else
		Result := R;
end;

function TCustomRangeBar.IsPositionStored: Boolean;
begin
	Result := FPosition > 0;
end;

procedure TCustomRangeBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	R, H: TRect;
	P: Single;
begin
	if Button = mbRight then
	begin
		case GetZone(X, Y) of

			// jump to start
			zBtnPrev:
				Position := 0;

			// jump to end
			zBtnNext:
				Position := Range;

			// jump slider to mouse position on right click
			zTrackPrev, zTrackNext:
			begin
				R := GetTrackBoundary;
				H := GetHandleRect;

				if Kind = sbVertical then
				begin
					Dec(Y, GetButtonSize);
					Dec(Y, H.Height div 2);
					P := Y / R.Height;
				end
				else
				begin
					Dec(X, GetButtonSize);
					Dec(X, H.Width div 2);
					P := R.Width / X;
				end;

				SetPosition(Range * P);
			end;
		else
			inherited;
		end;
	end
	else
	begin
		if Range <= EffectiveWindow then
			FDragZone := zNone
		else
		begin
			inherited;

			if FDragZone = zHandle then
			begin
				StopDragTracking;
				FPosBeforeDrag := Position;
			end;
		end;
	end;
end;

procedure TCustomRangeBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	Delta: Single;
	WinSz: Single;
	ClientSz, HandleSz: Integer;
begin
	inherited;

	if FDragZone <> zHandle then Exit;
	WinSz := EffectiveWindow;
	if Range <= WinSz then Exit;

	if Kind = sbHorizontal then
		Delta := X - FStored.X
	else
		Delta := Y - FStored.Y;

	if Kind = sbHorizontal then
		ClientSz := ClientWidth
	else
		ClientSz := ClientHeight;

	Dec(ClientSz, GetButtonSize * 2);
	if BorderStyle = bsSingle then
		Dec(ClientSz, 2);
	HandleSz := Round(ClientSz * WinSz / Range);

	if HandleSz < MIN_SIZE then
		Delta := Round(Delta * (Range - WinSz) / (ClientSz - MIN_SIZE))
	else
		Delta := Delta * Range / ClientSz;

	FGenChange := True;
	Position := FPosBeforeDrag + Delta;
	FGenChange := False;
end;

procedure TCustomRangeBar.Resize;
var
	OldWindow: Integer;
	Center: Single;
begin
	if Centered then
	begin
		OldWindow := EffectiveWindow;
		UpdateEffectiveWindow;
		if Range > EffectiveWindow then
		begin
			if (Range > OldWindow) and (Range <> 0) then
				Center := (FPosition + OldWindow * 0.5) / Range
			else
				Center := 0.5;
			FPosition := Center * Range - EffectiveWindow * 0.5;
		end;
	end;

	AdjustPosition;

	inherited;
end;

procedure TCustomRangeBar.SetParams(NewRange, NewWindow: Integer);
var
	OldWindow, OldRange: Integer;
	Center: Single;
begin
	if NewRange  < 0 then NewRange  := 0;
	if NewWindow < 0 then NewWindow := 0;

	if (NewRange <> FRange) or (NewWindow <> EffectiveWindow) then
	begin
		OldWindow := EffectiveWindow;
		OldRange := Range;
		FRange := NewRange;
		FWindow := NewWindow;
		UpdateEffectiveWindow;

		if Centered and (Range > EffectiveWindow) then
		begin
			if (OldRange > OldWindow) and (OldRange <> 0) then
				Center := (FPosition + OldWindow * 0.5) / OldRange
			else
				Center := 0.5;
			FPosition := Center * Range - EffectiveWindow * 0.5;
		end;

		AdjustPosition;
		Invalidate;
	end;
end;

procedure TCustomRangeBar.SetPosition(Value: Single);
var
	OldPosition: Single;
begin
	if Value <> FPosition then
	begin
		OldPosition := FPosition;
		FPosition := Value;
		AdjustPosition;
		if OldPosition <> FPosition then
		begin
			Invalidate;
			DoChange;
		end;
	end;
end;

procedure TCustomRangeBar.SetRange(Value: Integer);
begin
	SetParams(Value, Window);
end;

procedure TCustomRangeBar.SetWindow(Value: Integer);
begin
	SetParams(Range, Value);
end;

procedure TCustomRangeBar.TimerHandler(Sender: TObject);
var
	OldPosition: Single;
	Pt: TPoint;

	function MousePos: TPoint;
	begin
		Result := ScreenToClient(Mouse.CursorPos);
		if Result.X < 0 then Result.X := 0;
		if Result.Y < 0 then Result.Y := 0;
		if Result.X >= ClientWidth  then Result.X := ClientWidth  - 1;
		if Result.Y >= ClientHeight then Result.Y := ClientHeight - 1
	end;

begin
	inherited;

	FGenChange := True;
	OldPosition := Position;

	case FDragZone of

		zBtnPrev:
		begin
			Position := Position - Increment;
			if Position = OldPosition then StopDragTracking;
		end;

		zBtnNext:
		begin
			Position := Position + Increment;
			if Position = OldPosition then StopDragTracking;
		end;

		zTrackNext:
		begin
			Pt := MousePos;
			if GetZone(Pt.X, Pt.Y) in [zTrackNext, zBtnNext] then
				Position := Position + EffectiveWindow;
		end;

		zTrackPrev:
		begin
			Pt := MousePos;
			if GetZone(Pt.X, Pt.Y) in [zTrackPrev, zBtnPrev] then
				Position := Position - EffectiveWindow;
		end;
	end;

	FGenChange := False;
end;

procedure TCustomRangeBar.UpdateEffectiveWindow;
begin
	if FWindow > 0 then
		FEffectiveWindow := FWindow
	else
	begin
		if Kind = sbHorizontal then
			FEffectiveWindow := Width
		else
			FEffectiveWindow := Height;
	end;
end;

//----------------------------------------------------------------------------//

{ TCustomGaugeBar }

procedure TCustomGaugeBar.AdjustPosition;
begin
	if Position < Min then
		Position := Min
	else
	if Position > Max then
		Position := Max;
end;

constructor TCustomGaugeBar.Create(AOwner: TComponent);
begin
	inherited;

	FLargeChange := 1;
	FMax := 100;
	FSmallChange := 1;
end;

function TCustomGaugeBar.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
	if Kind = sbVertical then WheelDelta := -WheelDelta;
	Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
	if not Result then
		Position := Position + FLargeChange * WheelDelta div 120;
	Result := True;
end;

function TCustomGaugeBar.GetHandleRect: TRect;
var
	Sz, HandleSz: Integer;
	Horz: Boolean;
	Pos: Integer;
begin
	Result := GetTrackBoundary;
	Horz := Kind = sbHorizontal;
	HandleSz := GetHandleSize;

	if Horz then
		Sz := Result.Right - Result.Left
	else
		Sz := Result.Bottom - Result.Top;

	Pos := Round((Position - Min) / (Max - Min) * (Sz - GetHandleSize));

	if Horz then
	begin
		Inc(Result.Left, Pos);
		Result.Right := Result.Left + HandleSz;
	end
	else
	begin
		Inc(Result.Top, Pos);
		Result.Bottom := Result.Top + HandleSz;
	end;
end;

function TCustomGaugeBar.GetHandleSize: Integer;
var
	R: TRect;
	Sz: Integer;
begin
	Result := HandleSize;

	if Result = 0 then
	begin
		if Kind = sbHorizontal then
			Result := ClientHeight
		else
			Result := ClientWidth;
	end;

	R := GetTrackBoundary;
	if Kind = sbHorizontal then
		Sz := R.Right - R.Left
	else
		Sz := R.Bottom - R.Top;
	if Sz - Result < 1 then
		Result := Sz - 1;
	if Result < 0 then
		Result := 0;
end;

procedure TCustomGaugeBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbRight then
	begin
		case GetZone(X, Y) of
			zBtnPrev: Position := Min;
			zBtnNext: Position := Max;
			{zTrackPrev, zTrackNext: // !!! TODO
			begin
				// jump slider to mouse position on right click
				R := GetTrackBoundary;
				H := GetHandleRect;

				if Kind = sbVertical then
				begin
					Dec(Y, GetButtonSize);
					Dec(Y, H.Height div 2);
					P := Y / R.Height;
				end
				else
				begin
					Dec(X, GetButtonSize);
					Dec(X, H.Width div 2);
					P := R.Width / X;
				end;
				SetPosition(Range * P);
			end;}
		else
			inherited;
		end;
	end
	else
	begin
		inherited;

		if FDragZone = zHandle then
		begin
			StopDragTracking;
			FPosBeforeDrag := Position;
		end;
	end;
end;

procedure TCustomGaugeBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	Delta: Single;
	R: TRect;
	ClientSz: Integer;
begin
	inherited;

	if FDragZone = zHandle then
	begin
		if Kind = sbHorizontal then
			Delta := X - FStored.X
		else
			Delta := Y - FStored.Y;

		R := GetTrackBoundary;

		if Kind = sbHorizontal then
			ClientSz := R.Right - R.Left
		else
			ClientSz := R.Bottom - R.Top;

		Delta := Delta * (Max - Min) / (ClientSz - GetHandleSize);
		FGenChange := True;
		Position := Round(FPosBeforeDrag + Delta);
		FGenChange := False;
	end;
end;

procedure TCustomGaugeBar.SetHandleSize(Value: Integer);
begin
	if Value < 0 then Value := 0;
	if Value <> FHandleSize then
	begin
		FHandleSize := Value;
		Invalidate;
	end;
end;

procedure TCustomGaugeBar.SetLargeChange(Value: Integer);
begin
	if Value < 1 then Value := 1;
	FLargeChange := Value;
end;

procedure TCustomGaugeBar.SetMax(Value: Integer);
begin
	if (Value <= FMin) and not (csLoading in ComponentState) then
		Value := FMin + 1;
	if Value <> FMax then
	begin
		FMax := Value;
		AdjustPosition;
		Invalidate;
	end;
end;

procedure TCustomGaugeBar.SetMin(Value: Integer);
begin
	if (Value >= FMax) and not (csLoading in ComponentState) then
		Value := FMax - 1;
	if Value <> FMin then
	begin
		FMin := Value;
		AdjustPosition;
		Invalidate;
	end;
end;

procedure TCustomGaugeBar.SetPosition(Value: Integer);
begin
	if (FWrapAround) and (FDragZone <> zHandle) then
	begin
		if Value < Min then Value := Max
		else if Value > Max then Value := Min;
	end
	else
	begin
		if Value < Min then Value := Min
		else if Value > Max then Value := Max;
	end;

	if Round(FPosition) <> Value then
	begin
		FPosition := Value;
		Invalidate;
		DoChange;
	end;
end;

procedure TCustomGaugeBar.SetSmallChange(Value: Integer);
begin
	if Value < 1 then Value := 1;
	FSmallChange := Value;
end;

procedure TCustomGaugeBar.TimerHandler(Sender: TObject);
var
	OldPosition: Single;
	Pt: TPoint;

	function MousePos: TPoint;
	begin
		Result := ScreenToClient(Mouse.CursorPos);
		if Result.X < 0 then Result.X := 0;
		if Result.Y < 0 then Result.Y := 0;
		if Result.X >= ClientWidth then Result.X := ClientWidth - 1;
		if Result.Y >= ClientHeight then Result.Y := ClientHeight - 1
	end;

begin
	inherited;

	FGenChange := True;
	OldPosition := Position;

	case FDragZone of

		zBtnPrev:
		begin
			Position := Position - SmallChange;
			if Position = OldPosition then StopDragTracking;
		end;

		zBtnNext:
		begin
			Position := Position + SmallChange;
			if Position = OldPosition then StopDragTracking;
		end;

		zTrackNext:
		begin
			Pt := MousePos;
			if GetZone(Pt.X, Pt.Y) in [zTrackNext, zBtnNext] then
				Position := Position + LargeChange;
		end;

		zTrackPrev:
		begin
			Pt := MousePos;
			if GetZone(Pt.X, Pt.Y) in [zTrackPrev, zBtnPrev] then
				Position := Position - LargeChange;
		end;
	end;

	FGenChange := False;
end;

end.

