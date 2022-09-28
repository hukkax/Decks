unit DecksBevel;

{$MODE Delphi}

interface

uses
	Classes, SysUtils, Graphics, Controls, ExtCtrls,
	LCLType, LResources, FPCanvas,
	BGRABitmap, BGRABitmapTypes, BCTypes, BCBasectrls;

type
	TBCBevelKind = ( bcbNone, bcbTopLeft, bcbTop, bcbBottomRight, bcbBottom );

	TBCSide = ( bcsTop, bcsBottom, bcsLeft, bcsRight );
	TBCSides = set of TBCSide;

	TBCBackground2 = class(TBCBackground)
	private
		FFillStyle: TFPBrushStyle;
		FFillColor: TColor;
		procedure SetFillStyle(AValue: TFPBrushStyle);
		procedure SetFillColor(AValue: TColor);
	public
		procedure   Assign(Source: TPersistent); override;
		constructor Create(AControl: TControl); override;
		destructor  Destroy; override;
	published
		property FillStyle: TFPBrushStyle read FFillStyle write SetFillStyle;
		property FillColor: TColor read FFillColor write SetFillColor;
	end;

	TBCBevel = class(TBCProperty)
	private
		FOpacity: Byte;
		FInnerBevel, FOuterBevel: TBCBevelKind;
		FSides: TBCSides;
		FColorLight, FColorDark: TColor;
		FColBri, FColDark: TBGRAPixel;
		Col: array[Boolean] of array[TBCSide] of TBGRAPixel;
		procedure SetOpacity(Value: Byte);
		procedure SetInnerBevel(Value: TBCBevelKind);
		procedure SetOuterBevel(Value: TBCBevelKind);
		procedure SetColorLight(Value: TColor);
		procedure SetColorDark(Value: TColor);
		procedure ColorsChanged;
		procedure SetSides(Value: TBCSides);
	public
		constructor Create(AControl: TControl); override;
		procedure   Assign(Source: TPersistent); override;
		procedure   Render(const ARect: TRect; ATargetBGRA: TBGRABitmap;
		            ARounding: TBCRounding; ABorder: TBCBorder);
	published
		property Opacity: Byte read FOpacity write SetOpacity default 50;
		property ColorLight: TColor read FColorLight write SetColorLight default clWhite;
		property ColorDark:  TColor read FColorDark  write SetColorDark  default clBlack;
		property InnerBevel: TBCBevelKind read FInnerBevel write SetInnerBevel;
		property OuterBevel: TBCBevelKind read FOuterBevel write SetOuterBevel;
		property Sides: TBCSides read FSides write SetSides;
	end;

	TDecksBevel = class
	private
	public
	end;


	function  Lighten(C: TColor; Light: Integer): TColor; inline;
	procedure RenderBackgroundAndBorder(const ARect: TRect;
		ABackground: TBCBackground; ATargetBGRA: TBGRABitmap;
		ARounding: TBCRounding; ABorder: TBCBorder; ABevel: TBCBevel; AInnerMargin: Single);


implementation

uses
	Math, BCTools;

// =================================================================================================
// Utility
// =================================================================================================

function Lighten(C: TColor; Light: Integer): TColor;
var
	P: THSLAPixel;
begin
	P := C;
	P.lightness := Min(65535, P.lightness + Light);
	Result := P.ToColor;
end;

procedure RenderBackgroundAndBorder(const ARect: TRect;
	ABackground: TBCBackground; ATargetBGRA: TBGRABitmap;
	ARounding: TBCRounding; ABorder: TBCBorder; ABevel: TBCBevel; AInnerMargin: Single);
var
	w, ht, hb: Single;
begin
	ATargetBGRA.Canvas.FillRect(ARect);

	if ABorder.Style = bboNone then
		w := AInnerMargin - 0.5
	else
		w := (ABorder.Width-1) / 2 + AInnerMargin;

	ht := 0; hb := 0;
	if ABevel.OuterBevel in [bcbBottom, bcbBottomRight, bcbTop, bcbTopLeft] then
	begin
		if bcsTop    in ABevel.Sides then ht := 1;
		if bcsBottom in ABevel.Sides then hb := 1;
	end;

	RenderBackgroundF(ARect.Left+w, ARect.Top+w, ARect.Right-1-w, ARect.Bottom-1-w,
		ABackground, ATargetBGRA, ARounding);

	ABevel.Render(ARect, ATargetBGRA, ARounding, ABorder);

	if ABorder.Style <> bboNone then
	begin
		RenderBorderF(ARect.Left+w, ARect.Top+w+ht, ARect.Right-1-w, ARect.Bottom-1-w-hb,
			ABorder, ATargetBGRA, ARounding);
	end;
end;

{ TBCBackground2 }

procedure TBCBackground2.SetFillStyle(AValue: TFPBrushStyle);
begin
	if FFillStyle = AValue then Exit;
	FFillStyle := AValue;
	Change;
end;

procedure TBCBackground2.SetFillColor(AValue: TColor);
begin
	if FFillColor = AValue then Exit;
	FFillColor := AValue;
	Change;
end;

procedure TBCBackground2.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TBCBackground2 then
  begin
    FFillStyle := TBCBackground2(Source).FFillStyle;
    FFillColor := TBCBackground2(Source).FFillColor;
  end;
end;

constructor TBCBackground2.Create(AControl: TControl);
begin
	inherited Create(AControl);
	FFillStyle := bsClear;
	FFillColor := $202020;
end;

destructor TBCBackground2.Destroy;
begin
	inherited Destroy;
end;

// =================================================================================================
// TBCBevel
// =================================================================================================

procedure TBCBevel.Render(const ARect: TRect; ATargetBGRA: TBGRABitmap;
	ARounding: TBCRounding; ABorder: TBCBorder);
var
	PaddingX, PaddingY: Integer;

	procedure DoRender(B: Boolean); // False=Inner, True=Outer bevel
	var
		px, py: Integer;
	begin
		// top
		if bcsTop in Sides then
			ATargetBGRA.FastBlendHorizLine(
				ARect.Left+PaddingX, ARect.Top+PaddingY, ARect.Right-1-PaddingX, Col[B,bcsTop]);

		// bottom
		if bcsBottom in Sides then
			ATargetBGRA.FastBlendHorizLine(
				ARect.Left+PaddingX, ARect.Bottom-1-PaddingY, ARect.Right-1-PaddingX, Col[B,bcsBottom]);

		px := PaddingY;
		py := Trunc(px + (ARounding.RoundY * 0.5));

		// left
		if bcsLeft in Sides then
			ATargetBGRA.FastBlendVertLine(
				ARect.Left+px, ARect.Top+py, ARect.Bottom-1-py, Col[B,bcsLeft]);
		// right
		if bcsRight in Sides then
			ATargetBGRA.FastBlendVertLine(
				ARect.Right-1-px, ARect.Top+py, ARect.Bottom-1-py, Col[B,bcsRight]);
	end;

begin
	if (Opacity < 1) or (ARounding.RoundX > 3) or (ARounding.RoundY > 3) then Exit;

	PaddingY := ABorder.Width + ABorder.LightWidth;
	PaddingX := Trunc(PaddingY + (ARounding.RoundX * 0.5));

	if FInnerBevel <> bcbNone then
	begin
		DoRender(False);
	end;
	if FOuterBevel <> bcbNone then
	begin
		PaddingY := 0;
		DoRender(True);
	end;
end;

procedure TBCBevel.SetOpacity(Value: Byte);
begin
	if FOpacity = Value then Exit;

	FOpacity := Value;
	ColorsChanged;
end;

procedure TBCBevel.SetColorDark(Value: TColor);
begin
	if FColorDark = Value then Exit;
	FColorDark := Value;
	ColorsChanged;
end;

procedure TBCBevel.ColorsChanged;

	procedure GetBevelColors(Bevel: TBCBevelKind);
	var
		B: Boolean;
	begin
		B := (Bevel = FOuterBevel);
		case Bevel of
			bcbNone: ;
			bcbTopLeft:
			begin
				Col[B, bcsTop]   := FColBri;
				Col[B, bcsLeft]  := FColBri;
				Col[B, bcsBottom]:= FColDark;
				Col[B, bcsRight] := FColDark;
			end;
			bcbBottomRight:
			begin
				Col[B, bcsBottom] := FColBri;
				Col[B, bcsRight]  := FColBri;
				Col[B, bcsTop]    := FColDark;
				Col[B, bcsLeft]   := FColDark;
			end;
			bcbTop:
			begin
				Col[B, bcsTop]   := FColBri;
				Col[B, bcsLeft]  := FColDark;
				Col[B, bcsBottom]:= FColDark;
				Col[B, bcsRight] := FColDark;
			end;
			bcbBottom:
			begin
				Col[B, bcsBottom] := FColBri;
				Col[B, bcsTop]    := FColDark;
				Col[B, bcsLeft]   := FColDark;
				Col[B, bcsRight]  := FColDark;
			end;
		end;
	end;

begin
	FColBri.FromColor(FColorLight, Opacity);
	FColDark.FromColor(FColorDark, Opacity);

	GetBevelColors(FInnerBevel);
	GetBevelColors(FOuterBevel);

	Change;
end;

procedure TBCBevel.SetSides(Value: TBCSides);
begin
	if FSides = Value then Exit;
	FSides := Value;
	Change;
end;

procedure TBCBevel.SetInnerBevel(Value: TBCBevelKind);
begin
	if FInnerBevel = Value then Exit;
	FInnerBevel := Value;
	ColorsChanged;
end;

procedure TBCBevel.SetOuterBevel(Value: TBCBevelKind);
begin
	if FOuterBevel = Value then Exit;
	FOuterBevel := Value;
	ColorsChanged;
end;

procedure TBCBevel.SetColorLight(Value: TColor);
begin
	if FColorLight = Value then Exit;
	FColorLight := Value;
	ColorsChanged;
end;

constructor TBCBevel.Create(AControl: TControl);
begin
	inherited Create(AControl);

	FOpacity := 50;
	FColorDark  := clBlack;
	FColorLight := clWhite;
	FInnerBevel := bcbTop;
	FOuterBevel := bcbNone;
	Sides := [bcsTop, bcsBottom, bcsLeft, bcsRight];

	ColorsChanged;
end;

procedure TBCBevel.Assign(Source: TPersistent);
var
	Src: TBCBevel;
begin
	if Source is TBCBevel then
	begin
		Src := TBCBevel(Source);
		FOpacity := Src.FOpacity;
		FColorDark  := Src.FColorDark;
		FColorLight := Src.FColorLight;
		FInnerBevel := Src.FInnerBevel;
		FOuterBevel := Src.FOuterBevel;
		FSides := Src.Sides;
		ColorsChanged;
	end
	else
		inherited Assign(Source);
end;


// =================================================================================================
// TDecksBevel
// =================================================================================================

end.

