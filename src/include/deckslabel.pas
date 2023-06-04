// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Equivalent of standard lazarus TLabel but using BGRA Controls framework for text
  render.

  Functionality:
  - Customizable background (gradients etc.)
  - Customizable border (rounding etc.)
  - FontEx (shadow, word wrap, etc.)

  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)
- Joel Toivonen (hukka)
  Modified for use in Decks 3 (2022-09-22)
***************************** END CONTRIBUTOR(S) *****************************}

unit DecksLabel;

{$MODE Delphi}

interface

uses
  Classes, SysUtils,{$IFDEF FPC}LResources,{$ENDIF}
  Types, Forms, Controls, Graphics, Dialogs,
  BCBasectrls, BGRABitmap, BGRABitmapTypes, BCTypes,
  DecksBevel;

type
  TDecksLabel = class(TBCStyleGraphicControl)
  private
    { Private declarations }
    {$IFDEF INDEBUG}
    FRenderCount: Integer;
    {$ENDIF}
    FBackground: TBCBackground;
    FBGRA: TBGRABitmapEx;
	FBevel:  TBCBevel;
    FBorder: TBCBorder;
    FFontEx: TBCFont;
    FInnerMargin: Single;
    FRounding: TBCRounding;
    procedure Render;
    procedure SetInnerMargin(AValue: single);
    procedure SetRounding(AValue: TBCRounding);
    procedure UpdateSize;
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetFontEx(AValue: TBCFont);
    procedure OnChangeProperty(Sender: TObject; {%H-}Data: BGRAPtrInt);
    procedure OnChangeFont({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
	procedure SetBevel(Value: TBCBevel);
  protected
	procedure ChangeScale(Multiplier, Divider: Integer); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure TextChanged; override;
    {$IFDEF INDEBUG}
    function GetDebugText: String; override;
    {$ENDIF}
    procedure DrawControl; override;
    procedure RenderControl; override;
    function GetStyleExtension: String; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property AutoSize default True;
    property Background: TBCBackground read FBackground write SetBackground;
	property Bevel: TBCBevel read FBevel write SetBevel;
    property Border: TBCBorder read FBorder write SetBorder;
    property BorderSpacing;
    property Caption;
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
  end;

  procedure Register;

implementation

uses
	LCLType, BCTools;

procedure Register;
begin
	RegisterComponents('Decks', [TDecksLabel]);
end;

{ TDecksLabel }

procedure TDecksLabel.Render;
var
	r: TRect;
begin
	if (csCreating in ControlState) or IsUpdating then Exit;

	FBGRA.NeedRender := False;

	FBGRA.SetSize(Width, Height);
	FBGRA.Fill(BGRAPixelTransparent); // Clear;
	r := FBGRA.ClipRect;
	CalculateBorderRect(FBorder, r);

	DecksBevel.RenderBackgroundAndBorder(r, FBackground, TBGRABitmap(FBGRA),
		FRounding, FBorder, FBevel, FInnerMargin);
	RenderText(r, FFontEx, Caption, TBGRABitmap(FBGRA), True);

	{$IFDEF INDEBUG}
	FRenderCount := FRenderCount +1;
	{$ENDIF}
	{$IFNDEF FPC}//# //@  IN DELPHI NEEDRENDER NEED TO BE TRUE. IF FALSE COMPONENT IN BGRANORMAL BE BLACK AFTER INVALIDATE.
	FBGRA.NeedRender := True;
	{$ENDIF}
end;

procedure TDecksLabel.SetInnerMargin(AValue: single);
begin
  if FInnerMargin=AValue then Exit;
  FInnerMargin:=AValue;
  RenderControl;
  Invalidate;
end;

procedure TDecksLabel.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TDecksLabel.UpdateSize;
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TDecksLabel.SetBackground(AValue: TBCBackground);
begin
  FBackground.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TDecksLabel.SetBorder(AValue: TBCBorder);
begin
  FBorder.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TDecksLabel.SetFontEx(AValue: TBCFont);
begin
  FFontEx.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TDecksLabel.OnChangeProperty(Sender: TObject; Data: BGRAPtrInt);
begin
  RenderControl;
  if (Sender = FBorder) and AutoSize then
    UpdateSize;
  Invalidate;
end;

procedure TDecksLabel.OnChangeFont(Sender: TObject; AData: BGRAPtrInt);
begin
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TDecksLabel.SetBevel(Value: TBCBevel);
begin
	if FBevel = Value then Exit;
	FBevel.Assign(Value);
	RenderControl;
	Invalidate;
end;

procedure TDecksLabel.ChangeScale(Multiplier, Divider: Integer);
begin
	if Multiplier <> Divider then
		FFontEx.Height := MulDiv(FBGRA.FontHeight, Multiplier, Divider);
	inherited ChangeScale(Multiplier, Divider);
end;

procedure TDecksLabel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: boolean);
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then
    Exit;

  CalculateTextSize(Caption, FFontEx, PreferredWidth, PreferredHeight);

  if AutoSize and (FBorder.Style<>bboNone) then
  begin
    Inc(PreferredHeight, 2 * FBorder.Width);
    Inc(PreferredWidth, 2 * FBorder.Width);
  end;
end;

class function TDecksLabel.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 100;
  Result.cy := 25;
end;

procedure TDecksLabel.TextChanged;
begin
  inherited TextChanged;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

{$IFDEF INDEBUG}
function TDecksLabel.GetDebugText: String;
begin
  Result := 'R: '+IntToStr(FRenderCount);
end;
{$ENDIF}

procedure TDecksLabel.DrawControl;
begin
  inherited DrawControl;
  if FBGRA.NeedRender then
    Render;
  FBGRA.Draw(Self.Canvas,0,0,False);
  {$IFNDEF FPC}//# //@  IN DELPHI RenderControl NEDD. IF NO RenderControl BE BLACK AFTER INVALIDATE.
  FBGRA.NeedRender := True;
  {$ENDIF}
end;

procedure TDecksLabel.RenderControl;
begin
  inherited RenderControl;
  if FBGRA<>nil then
    FBGRA.NeedRender := True;
end;

function TDecksLabel.GetStyleExtension: String;
begin
  Result := 'bclbl';
end;

procedure TDecksLabel.UpdateControl;
begin
  RenderControl;
  inherited UpdateControl; // invalidate
end;

procedure TDecksLabel.SaveToFile(AFileName: string);
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

procedure TDecksLabel.LoadFromFile(AFileName: string);
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

procedure TDecksLabel.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
	if CompareText(AClassName, 'TDecksLabel') = 0 then
		ComponentClass := TDecksLabel;
end;

constructor TDecksLabel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	{$IFDEF INDEBUG}FRenderCount := 0;{$ENDIF}

	DisableAutoSizing;
	Include(FControlState, csCreating);

	BeginUpdate;
	try
		with GetControlClassDefaultSize do
			SetInitialBounds(0, 0, CX, CY);

		FBGRA := TBGRABitmapEx.Create(Width, Height);
		FBackground         := TBCBackground.Create(Self);
		FBevel              := TBCBevel.Create(Self);
		FBorder             := TBCBorder.Create(Self);
		FRounding           := TBCRounding.Create(Self);
		FFontEx             := TBCFont.Create(Self);

		ParentColor         := True;

		FBackground.OnChange := OnChangeProperty;
		FBevel.OnChange      := OnChangeProperty;
		FBorder.OnChange     := OnChangeProperty;
		FRounding.OnChange  := OnChangeProperty;
		FFontEx.OnChange     := OnChangeFont;

		FBackground.Color   := $444444;
		FBackground.Style   := bbsClear;

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

		AutoSize := False;
	finally
		EnableAutoSizing;
		EndUpdate;
		Exclude(FControlState, csCreating);
	end;
end;

destructor TDecksLabel.Destroy;
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


