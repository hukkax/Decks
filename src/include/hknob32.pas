// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}

unit hKnob;

{$MODE DELPHI}
{$I bgracontrols.inc}

interface

uses
	Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
	BCBaseCtrls, BGRAGradients, BGRABitmap, BGRABitmapTypes;

type
	ThKnobPositionType = (kptLineSquareCap, kptLineRoundCap, kptFilledCircle, kptHollowCircle);
	ThKnobValueChangedEvent = procedure(Sender: TObject; Value: single) of object;

	ThKnob = class(TBGRAGraphicCtrl)
	private
		FPhong: TPhongShading;
		FCurveExponent: single;
		FKnobBmp: TBGRABitmap;
		FKnobColor: TColor;
		FAngularPos: single;
		FPositionColor: TColor;
		FPositionMargin: single;
		FPositionOpacity: byte;
		FPositionType: ThKnobPositionType;
		FPositionWidth: single;
		FSettingAngularPos: boolean;
		FUsePhongLighting: boolean;
		FMinValue, FMaxValue: single;
		FOnKnobValueChange: ThKnobValueChangedEvent;
		FStartFromBottom: boolean;
		procedure CreateKnobBmp;
		function GetLightIntensity: integer;
		function GetValue: single;
		procedure SetCurveExponent(const AValue: single);
		procedure SetLightIntensity(const AValue: integer);
		procedure SetStartFromBottom(const AValue: boolean);
		procedure SetValue(AValue: single);
		procedure SetMaxValue(AValue: single);
		procedure SetMinValue(AValue: single);
		procedure SetPositionColor(const AValue: TColor);
		procedure SetPositionMargin(AValue: single);
		procedure SetPositionOpacity(const AValue: byte);
		procedure SetPositionType(const AValue: ThKnobPositionType);
		procedure SetPositionWidth(const AValue: single);
		procedure SetUsePhongLighting(const AValue: boolean);
		procedure UpdateAngularPos(X, Y: integer);
		procedure SetKnobColor(const AValue: TColor);
	protected
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
		procedure Paint; override;
		procedure Resize; override;
		function ValueCorrection(var AValue: single): boolean; overload; virtual;
		function ValueCorrection: boolean; overload; virtual;
	public
		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;
		procedure SaveToFile(AFileName: string);
		procedure LoadFromFile(AFileName: string);
		procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
			var ComponentClass: TComponentClass);
	published
		property Anchors;
		property CurveExponent: single read FCurveExponent write SetCurveExponent;
		property KnobColor: TColor read FKnobColor write SetKnobColor;
		property LightIntensity: integer read GetLightIntensity write SetLightIntensity;
		property PositionColor: TColor read FPositionColor write SetPositionColor;
		property PositionWidth: single read FPositionWidth write SetPositionWidth;
		property PositionOpacity: byte read FPositionOpacity write SetPositionOpacity;
		property PositionMargin: single read FPositionMargin write SetPositionMargin;
		property PositionType: ThKnobPositionType read FPositionType write SetPositionType;
		property UsePhongLighting: boolean read FUsePhongLighting write SetUsePhongLighting;
		property MinValue: single read FMinValue write SetMinValue;
		property MaxValue: single read FMaxValue write SetMaxValue;
		property Value: single read GetValue write SetValue;
		property OnValueChanged: ThKnobValueChangedEvent read FOnKnobValueChange write FOnKnobValueChange;
		property StartFromBottom: boolean read FStartFromBottom write SetStartFromBottom;
	end;

	procedure Register;

implementation

uses
	Math;

procedure Register;
begin
	RegisterComponents('Custom', [hKnob]);
end;

{ ThKnob }

constructor ThKnob.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	with GetControlClassDefaultSize do
		SetInitialBounds(0, 0, CX, CY);

	FPhong := TPhongShading.Create;
	FPhong.LightPositionZ := 100;
	FPhong.LightSourceIntensity := 300;
	FPhong.NegativeDiffusionFactor := 0.8;
	FPhong.AmbientFactor := 0.5;
	FPhong.DiffusionFactor := 0.6;

	FKnobBmp := nil;
	FOnKnobValueChange := nil;

	FCurveExponent := 0.2;
	FKnobColor := clBtnFace;
	FPositionColor := clBtnText;
	FPositionOpacity := 192;
	FPositionWidth := 4;
	FPositionMargin := 4;
	FPositionType := kptLineSquareCap;
	FUsePhongLighting := True;
	FStartFromBottom := True;
	FMinValue := 30;
	FMaxValue := 330;
end;

destructor ThKnob.Destroy;
begin
	FPhong.Free;
	FKnobBmp.Free;
	inherited Destroy;
end;

procedure ThKnob.CreateKnobBmp;
var
	xb, yb, tx, ty: Integer;
	h, d2: Single;
	center, v: TPointF;
	p: PBGRAPixel;
	BGRAKnobColor: TBGRAPixel;
	mask, Map: TBGRABitmap;
begin
	tx := ClientWidth;
	ty := ClientHeight;
	if (tx = 0) or (ty = 0) then Exit;

	FreeAndNil(FKnobBmp);

	FKnobBmp := TBGRABitmap.Create(tx, ty);
	center := PointF((tx - 1) / 2, (ty - 1) / 2);
	BGRAKnobColor := KnobColor;

	if UsePhongLighting then
	begin
		//compute knob height map
		Map := TBGRABitmap.Create(tx, ty);
		for yb := 0 to ty-1 do
		begin
			p := map.ScanLine[yb];
			for xb := 0 to tx - 1 do
			begin
				//compute vector between center and current pixel
				v := PointF(xb, yb) - center;
				//scale down to unit circle (with 1 pixel margin for soft border)
				v.x := v.x / (tx / 2 + 1);
				v.y := v.y / (ty / 2 + 1);
				//compute squared distance with scalar product
				d2 := v * v;
				//interpolate as quadratic curve and apply power function
				if d2 > 1 then
					h := 0
				else
					h := power(1 - d2, FCurveExponent);
				p^ := MapHeightToBGRA(h, 255);
				Inc(p);
			end;
		end;

		//antialiased border
		mask := TBGRABitmap.Create(tx, ty, BGRABlack);
		Mask.FillEllipseAntialias(center.x, center.y, tx / 2, ty / 2, BGRAWhite);
		map.ApplyMask(mask);
		Mask.Free;

		FPhong.Draw(FKnobBmp, Map, 30, 0, 0, BGRAKnobColor);
		Map.Free;
	end
	else
	begin
		FKnobBmp.FillEllipseAntialias(center.x, center.y, tx / 2, ty / 2, BGRAKnobColor);
	end;
end;

function ThKnob.GetLightIntensity: integer;
begin
	Result := Round(FPhong.LightSourceIntensity);
end;

function ThKnob.GetValue: single;
begin
	Result := FAngularPos * 180 / Pi;
	if Result < 0 then
		Result := Result + 360;
	Result := 270 - Result;
	if Result < 0 then
		Result := Result + 360;
end;

procedure ThKnob.SetCurveExponent(const AValue: single);
begin
	if FCurveExponent = AValue then Exit;
	FCurveExponent := AValue;
	FreeAndNil(FKnobBmp);
	Invalidate;
end;

procedure ThKnob.SetKnobColor(const AValue: TColor);
begin
	if FKnobColor = AValue then Exit;
	FKnobColor := AValue;
	FreeAndNil(FKnobBmp);
	Invalidate;
end;

procedure ThKnob.SetLightIntensity(const AValue: integer);
begin
	if AValue <> FPhong.LightSourceIntensity then
	begin
		FPhong.LightSourceIntensity := AValue;
		FreeAndNil(FKnobBmp);
		Invalidate;
	end;
end;

procedure ThKnob.SetStartFromBottom(const AValue: boolean);
begin
	if FStartFromBottom = AValue then Exit;
	FStartFromBottom := AValue;
	Invalidate;
end;

procedure ThKnob.SetValue(AValue: single);
var
	NewAngularPos: Single;
begin
	ValueCorrection(AValue);
	NewAngularPos := 3 * Pi / 2 - AValue * Pi / 180;
	if NewAngularPos > Pi then
		NewAngularPos := NewAngularPos -(2 * Pi);
	if NewAngularPos < -Pi then
		NewAngularPos := NewAngularPos +(2 * Pi);
	if NewAngularPos <> FAngularPos then
	begin
		FAngularPos := NewAngularPos;
		Invalidate;
	end;
end;

procedure ThKnob.SetMaxValue(AValue: single);
begin
	if AValue < 0 then AValue := 0
	else
	if AValue > 360 then AValue := 360;
	if FMaxValue = AValue then Exit;
	FMaxValue := AValue;
	if FMinValue > FMaxValue then
		FMinValue := FMaxValue;
	if ValueCorrection then
		Invalidate;
end;

procedure ThKnob.SetMinValue(AValue: single);
begin
	if AValue < 0 then AValue := 0
	else
	if AValue > 360 then AValue := 360;
	if FMinValue = AValue then Exit;
	FMinValue := AValue;
	if FMaxValue < FMinValue then
		FMaxValue := FMinValue;
	if ValueCorrection then
		Invalidate;
end;

procedure ThKnob.SetPositionColor(const AValue: TColor);
begin
	if FPositionColor = AValue then Exit;
	FPositionColor := AValue;
	Invalidate;
end;

procedure ThKnob.SetPositionMargin(AValue: single);
begin
	if FPositionMargin = AValue then Exit;
	FPositionMargin := AValue;
	Invalidate;
end;

procedure ThKnob.SetPositionOpacity(const AValue: byte);
begin
	if FPositionOpacity = AValue then Exit;
	FPositionOpacity := AValue;
	Invalidate;
end;

procedure ThKnob.SetPositionType(const AValue: ThKnobPositionType);
begin
	if FPositionType = AValue then Exit;
	FPositionType := AValue;
	Invalidate;
end;

procedure ThKnob.SetPositionWidth(const AValue: single);
begin
	if FPositionWidth = AValue then Exit;
	FPositionWidth := AValue;
	Invalidate;
end;

procedure ThKnob.SetUsePhongLighting(const AValue: boolean);
begin
	if FUsePhongLighting = AValue then Exit;
	FUsePhongLighting := AValue;
	FreeAndNil(FKnobBmp);
	Invalidate;
end;

procedure ThKnob.UpdateAngularPos(X, Y: integer);
var
	FPreviousPos, Sign: single;
begin
	FPreviousPos := FAngularPos;
	if FStartFromBottom then
		Sign := 1
	else
		Sign := -1;
	FAngularPos := ArcTan2((-Sign) * (Y - ClientHeight / 2) / ClientHeight,
	Sign * (X - ClientWidth / 2) / ClientWidth);
	ValueCorrection;
	Invalidate;
	if (FPreviousPos <> FAngularPos) and Assigned(FOnKnobValueChange) then
		FOnKnobValueChange(Self, Value);
end;

procedure ThKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
	inherited MouseDown(Button, Shift, X, Y);
	if Button = mbLeft then
	begin
		FSettingAngularPos := True;
		UpdateAngularPos(X, Y);
	end;
end;

procedure ThKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
	inherited MouseUp(Button, Shift, X, Y);
	if Button = mbLeft then
		FSettingAngularPos := False;
end;

procedure ThKnob.MouseMove(Shift: TShiftState; X, Y: integer);
begin
	inherited MouseMove(Shift, X, Y);
	if FSettingAngularPos then
		UpdateAngularPos(X, Y);
end;

procedure ThKnob.Paint;
var
	Bmp: TBGRABitmap;
	Center, Pos: TPointF;
	PosColor: TBGRAPixel;
	PosLen: single;
begin
	if (ClientWidth < 1) or (ClientHeight < 1) then Exit;
	if FKnobBmp = nil then
	begin
		CreateKnobBmp;
		if FKnobBmp = nil then Exit;
	end;
	Bmp := TBGRABitmap.Create(ClientWidth, ClientHeight);
	Bmp.BlendImage(0, 0, FKnobBmp, boLinearBlend);

	//draw current position
	PosColor := ColorToBGRA(ColorToRGB(FPositionColor), FPositionOpacity);
	Center := PointF(ClientWidth / 2, ClientHeight / 2);
	Pos.X := Cos(FAngularPos) * (ClientWidth / 2);
	Pos.Y := -Sin(FAngularPos) * (ClientHeight / 2);
	if not FStartFromBottom then
		Pos := -Pos;
	PosLen := sqrt(Pos * Pos);

	Pos := Pos * ((PosLen - PositionMargin - FPositionWidth) / PosLen);
	Pos := Center + Pos;

	case PositionType of
		kptLineSquareCap:
		begin
			Bmp.LineCap := pecSquare;
			Bmp.DrawLineAntialias(Center.X, Center.Y, Pos.X, Pos.Y, PosColor, FPositionWidth);
		end;
		kptLineRoundCap:
		begin
			Bmp.LineCap := pecRound;
			Bmp.DrawLineAntialias(Center.X, Center.Y, Pos.X, Pos.Y, PosColor, FPositionWidth);
		end;
		kptFilledCircle:
		begin
			Bmp.FillEllipseAntialias(Pos.X, Pos.Y, FPositionWidth, FPositionWidth, PosColor);
		end;
		kptHollowCircle:
		begin
			Bmp.EllipseAntialias(Pos.X, Pos.Y, FPositionWidth * 2 / 3,
			FPositionWidth * 2 / 3, PosColor, FPositionWidth / 3);
		end;
	end;

	Bmp.Draw(Canvas, 0, 0, False);
	Bmp.Free;
end;

procedure ThKnob.Resize;
begin
	inherited Resize;
	if (FKnobBmp <> nil) and ((ClientWidth <> FKnobBmp.Width) or
		(ClientHeight <> FKnobBmp.Height)) then
			FreeAndNil(FKnobBmp);
end;

function ThKnob.ValueCorrection(var AValue: single): boolean;
begin
	if AValue < MinValue then
	begin
		AValue := MinValue;
		Result := True;
	end
	else
	if AValue > MaxValue then
	begin
		AValue := MaxValue;
		Result := True;
	end
	else
		Result := False;
end;

function ThKnob.ValueCorrection: boolean;
var
	LValue: Single;
begin
	LValue := Value;
	Result := ValueCorrection(LValue);
	if Result then
		Value := LValue;
end;

procedure ThKnob.SaveToFile(AFileName: string);
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

procedure ThKnob.LoadFromFile(AFileName: string);
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

procedure ThKnob.OnFindClass(Reader: TReader; const AClassName: string;
	var ComponentClass: TComponentClass);
begin
	if CompareText(AClassName, 'ThKnob') = 0 then
		ComponentClass := ThKnob;
end;

end.

