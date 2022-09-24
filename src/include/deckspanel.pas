// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Equivalent of standard lazarus TPanel but using BGRA Controls framework for render

  Functionality:
  - Customizable background (gradient etc.)
  - Customizable border (frame 3D or normal border, rounding etc)
  - FontEx (shadow etc.)

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)
- Joel Toivonen (hukka)
  Modified for use in Decks 3 (2022-09-22)
***************************** END CONTRIBUTOR(S) *****************************}

unit DecksPanel;

{$mode Delphi}

interface

uses
	Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF}
	Types, Forms, Controls, Graphics, Dialogs,
	BGRABitmap, BCBaseCtrls, BGRABitmapTypes, BCTypes, BCTools, BCPanel,
	DecksBevel;

type
  TDecksPanel = class(TBCStyleCustomControl)
  private
    { Private declarations }
    {$IFDEF INDEBUG}
    FRenderCount: Integer;
    {$ENDIF}
    FBackground: TBCBackground;
    FBGRA: TBGRABitmapEx;
    FBorder: TBCBorder;
    FFontEx: TBCFont;
    FOnAfterRenderBCPanel: TOnAfterRenderBCPanel;
    FRounding: TBCRounding;
	FBevel: TBCBevel;
    procedure SetBackground(AValue: TBCBackground);
	procedure SetBevel(Value: TBCBevel);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetFontEx(AValue: TBCFont);
    procedure SetRounding(AValue: TBCRounding);
    procedure Render;
    procedure OnChangeProperty({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
    procedure OnChangeFont({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
  protected
    { Protected declarations }
    class function GetControlClassDefaultSize: TSize; override;
    function GetDefaultDockCaption: String; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
    function GetStyleExtension: String; override;
    {$IFDEF INDEBUG}
    function GetDebugText: String; override;
    {$ENDIF}
    procedure DrawControl; override;
    procedure RenderControl; override;
    procedure SetParentBackground(const AParentBackground: Boolean); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControl; override; // Called by EndUpdate
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    property Align;
    property Anchors;
    property AssignStyle;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    {$IFDEF FPC} //#
    property OnGetDockCaption;
    {$ENDIF}
    property Background: TBCBackground read FBackground write SetBackground;
	property Bevel: TBCBevel read FBevel write SetBevel;
    property Border: TBCBorder read FBorder write SetBorder;
    property Caption;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FontEx: TBCFont read FFontEx write SetFontEx;
    property ParentBackground;
    property PopupMenu;
    property Rounding: TBCRounding read FRounding write SetRounding;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnAfterRenderBCPanel: TOnAfterRenderBCPanel read FOnAfterRenderBCPanel write FOnAfterRenderBCPanel;
  end;

	procedure Register;


implementation


procedure Register;
begin
	RegisterComponents('Custom', [TDecksPanel]);
end;

{ TDecksPanel }

procedure TDecksPanel.DrawControl;
begin
  inherited DrawControl;
  if FBGRA.NeedRender then
    Render;
  if Assigned (FRounding) then
  begin
    if (FRounding.RoundX<>0) and (FRounding.RoundY<>0) then
      FBGRA.Draw(Self.Canvas, 0, 0, False)
    else
      FBGRA.Draw(Self.Canvas, 0, 0);
  end
  else
    FBGRA.Draw(Self.Canvas, 0, 0);

  {$IFNDEF FPC}//# //@  IN DELPHI RenderControl NEDD. IF NO RenderControl BE BLACK AFTER INVALIDATE.
  FBGRA.NeedRender := True;
  {$ENDIF}
end;

procedure TDecksPanel.RenderControl;
begin
  inherited RenderControl;
  if FBGRA<>nil then
    FBGRA.NeedRender := True;
end;

procedure TDecksPanel.SetParentBackground(const AParentBackground: Boolean);
begin
  if ParentBackground=AParentBackground then
    Exit;
  if AParentBackground then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  inherited;
end;

function TDecksPanel.GetStyleExtension: String;
begin
  Result := 'bcpnl';
end;

{$IFDEF INDEBUG}
function TDecksPanel.GetDebugText: String;
begin
  Result := 'R: '+IntToStr(FRenderCount);
end;
{$ENDIF}

procedure TDecksPanel.Render;
var
	r: TRect;
begin
	if (csCreating in ControlState) or IsUpdating then Exit;

	FBGRA.NeedRender := False;

	FBGRA.SetSize(Width, Height);
	FBGRA.Fill(BGRAPixelTransparent);
	r := FBGRA.ClipRect;

	RenderBackgroundAndBorder(FBGRA.ClipRect, FBackground, TBGRABitmap(FBGRA),
		FRounding, FBorder, FBevel, 0);
	CalculateBorderRect(FBorder, r);

	if Caption <> '' then
		RenderText(r, FFontEx, Caption, TBGRABitmap(FBGRA));

	if Assigned(FOnAfterRenderBCPanel) then
		FOnAfterRenderBCPanel(Self, FBGRA, r);

	{$IFDEF INDEBUG} FRenderCount := FRenderCount + 1; {$ENDIF}
end;

procedure TDecksPanel.OnChangeProperty(Sender: TObject; AData: BGRAPtrInt);
begin
  RenderControl;
  Invalidate;
end;

procedure TDecksPanel.OnChangeFont(Sender: TObject; AData: BGRAPtrInt);
begin
  RenderControl;
  Invalidate;
end;

procedure TDecksPanel.SetBevel(Value: TBCBevel);
begin
	if FBevel = Value then Exit;
	FBevel.Assign(Value);
	RenderControl;
	Invalidate;
end;

procedure TDecksPanel.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

class function TDecksPanel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 170;
  Result.CY := 50;
end;

function TDecksPanel.GetDefaultDockCaption: String;
begin
  Result := Caption;
end;

procedure TDecksPanel.SetBackground(AValue: TBCBackground);
begin
  if FBackground = AValue then Exit;
  FBackground.Assign(AValue);
  Self.Color := AValue.Color;

  RenderControl;
  Invalidate;
end;

procedure TDecksPanel.SetBorder(AValue: TBCBorder);
begin
  if FBorder = AValue then Exit;
  FBorder.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TDecksPanel.SetFontEx(AValue: TBCFont);
begin
  if FFontEx = AValue then Exit;
  FFontEx.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TDecksPanel.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);

  RenderControl;
  Invalidate;
end;

procedure TDecksPanel.TextChanged;
begin
  {$IFDEF FPC}
  inherited TextChanged;
  {$ENDIF}

  RenderControl;
  Invalidate;
end;

constructor TDecksPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  {$IFDEF INDEBUG}
  FRenderCount := 0;
  {$ENDIF}
  {$IFDEF FPC}
  DisableAutoSizing;
  Include(FControlState, csCreating);
  {$ELSE} //#

  {$ENDIF}

  BeginUpdate;
  try
    ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
      csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
      csNoFocus, csAutoSize0x0, csOpaque]; // we need the default background
    //Self.DoubleBuffered := True;
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);

    FBGRA               := TBGRABitmapEx.Create;
    FBackground         := TBCBackground.Create(Self);
    FBorder             := TBCBorder.Create(Self);
    FFontEx             := TBCFont.Create(Self);
	FBevel              := TBCBevel.Create(Self);
    FRounding           := TBCRounding.Create(Self);

    ParentColor         := True;
    UseDockManager      := True;

    FBackground.OnChange := OnChangeProperty;
    FBorder.OnChange     := OnChangeProperty;
    FBevel.OnChange      := OnChangeProperty;
    FFontEx.OnChange     := OnChangeFont;
    FRounding.OnChange   := OnChangeProperty;

    FBackground.Style   := bbsColor;
    FBackground.Color   := $555555; //{$ifdef UseCLDefault}clDefault{$else}clBtnFace{$endif};

	FBorder.Color       := $262626;
    FBorder.Style       := bboNone;

	FBevel.ColorDark  := $222222;
	FBevel.ColorLight := $666666;
	FBevel.Opacity    := 255;
	FBevel.InnerBevel := bcbNone;
	FBevel.OuterBevel := bcbTop;

	FFontEx.Color := $CCCCCC;
	FFontEx.FontQuality := fqFineAntialiasing;
	FFontEx.Shadow := False;
	FFontEx.ShadowColorOpacity := 120;
	FFontEx.Style := [];

  finally
    {$IFDEF FPC}
    EnableAutoSizing;
    {$ENDIF}
    EndUpdate;
    {$IFDEF FPC}
    Exclude(FControlState, csCreating);
    {$ELSE} //#
    {$ENDIF}
  end;
end;

destructor TDecksPanel.Destroy;
begin
  FBackground.Free;
  FBorder.Free;
  FBevel.Free;
  FFontEx.Free;
  FBGRA.Free;
  FRounding.Free;
  inherited Destroy;
end;

procedure TDecksPanel.UpdateControl;
begin
  Render;
  inherited UpdateControl; // invalidate
end;

{$IFDEF FPC}
procedure TDecksPanel.SaveToFile(AFileName: string);
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

procedure TDecksPanel.LoadFromFile(AFileName: string);
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
{$ENDIF}

procedure TDecksPanel.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
	if CompareText(AClassName, 'TDecksPanel') = 0 then ComponentClass := TDecksPanel;
end;

end.

end.
