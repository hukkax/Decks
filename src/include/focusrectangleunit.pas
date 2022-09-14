unit FocusRectangleUnit;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Controls, ExtCtrls, Graphics,
	TeeGenericTree;

type
	TFocusRectangle = class(TShape)
	private
		FAlignedTo:  TControl;
		FBorderWidth: Integer;
		procedure SetBorderWidth(Value: Integer);
	public
		procedure AlignToControl(Ctrl: TControl);

		property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
	end;

	TFocusableControl = TNode<TControl>;

	TFocusableControls = class
	private
		procedure ShowFocusRect;
	public
		Root,
		CurrentItem: TFocusableControl;
		CurrentLevel: Integer;
		ActiveControl: TControl;
		FocusRectangle: TFocusRectangle;
		FocusColor: TColor;
		PrevCtrl:  array[0..6] of TControl;
		PrevColor: array[0..6] of TColor;

		function SelectNext:     TFocusableControl;
		function SelectPrevious: TFocusableControl;
		function Ascend:         TFocusableControl;
		function Descend:        TFocusableControl;

		constructor Create;
		destructor  Destroy; override;
	end;


implementation

// =================================================================================================
// TFocusableControls
// =================================================================================================

constructor TFocusableControls.Create;
begin
	inherited Create;

	Root := TFocusableControl.Create;

	CurrentItem := nil;
	CurrentLevel := 0;
	FocusColor := clRed;

	FocusRectangle := TFocusRectangle.Create(nil);
	FocusRectangle.BorderWidth := 2;
	FocusRectangle.Shape := stRectangle;
	FocusRectangle.Brush.Style := bsClear; // bsFDiagonal;
	//FocusRectangle.Brush.Color := clMaroon;
	FocusRectangle.Pen.Style := psSolid;
	FocusRectangle.Pen.Color := FocusColor;
	FocusRectangle.Pen.Width := FocusRectangle.BorderWidth;
	FocusRectangle.Pen.JoinStyle := pjsMiter;
	FocusRectangle.Visible := False;
	FocusRectangle.Enabled := False;
end;

destructor TFocusableControls.Destroy;
begin
	Root.Free;
	FocusRectangle.Free;
	inherited Destroy;
end;

function TFocusableControls.SelectNext: TFocusableControl;
begin
	if ActiveControl <> nil then Exit;

	if CurrentItem = nil then
		CurrentItem := Root.GetFirstChild
	else
		CurrentItem := CurrentItem.GetNextSibling;
	if ActiveControl <> nil then
		ActiveControl := CurrentItem.Data;
	Result := CurrentItem;
	ShowFocusRect;
end;

function TFocusableControls.SelectPrevious: TFocusableControl;
begin
	if ActiveControl <> nil then Exit;

	if CurrentItem = nil then
		CurrentItem := Root.GetFirstChild
	else
		CurrentItem := CurrentItem.GetPreviousSibling;
	if ActiveControl <> nil then
		ActiveControl := CurrentItem.Data;
	Result := CurrentItem;
	ShowFocusRect;
end;

function TFocusableControls.Ascend: TFocusableControl;
begin
	if CurrentItem <> nil then
	begin
		if ActiveControl <> nil then
			ActiveControl := nil
		else
			CurrentItem := CurrentItem.Parent;
	end;
	if CurrentItem = Root then
	begin
		FocusRectangle.Visible := False;
		PrevCtrl[1] := nil;
		CurrentItem := nil;
	end;
	ActiveControl := nil;
	FocusRectangle.Pen.Color := FocusColor;
	Result := CurrentItem;
	ShowFocusRect;
end;

function TFocusableControls.Descend: TFocusableControl;
begin
	if CurrentItem = nil then
		CurrentItem := Root;
	if CurrentItem.Count > 0 then
	begin
		CurrentItem := CurrentItem.GetFirstChild;
		FocusRectangle.Pen.Color := FocusColor;
		ActiveControl := nil;
	end
	else
	begin
		ActiveControl := CurrentItem.Data;
		FocusRectangle.Pen.Color := clYellow;
	end;
	Result := CurrentItem;
	ShowFocusRect;
end;

procedure TFocusableControls.ShowFocusRect;
var
	Ctrl: TControl;
	Level: Integer;
begin
	if (CurrentItem <> nil) and (CurrentItem.Data <> nil) then
	begin
		Ctrl := CurrentItem.Data;
		Level := CurrentItem.Level;
		if (CurrentLevel = Level) and (PrevCtrl[Level] = Ctrl) then Exit;

		//if (Ctrl is TPanel) or (Ctrl is ThListView) or (Ctrl is ThShellTree) then
		if (Ctrl is TWinControl) and (TWinControl(Ctrl).TabStop) then
		begin
			{if (PrevCtrl[Level] <> nil) and (PrevCtrl[Level] is TWinControl) then
				TWinControl(PrevCtrl[Level]).Color := PrevColor[Level];
			PrevColor[Level] := WCtrl.Color;
			WCtrl.Color := FocusColor;}
			PrevCtrl[Level] := Ctrl;
		end;

		CurrentLevel := Level;
		FocusRectangle.AlignToControl(Ctrl);
		FocusRectangle.Visible := True;
	end;
end;

// =================================================================================================
// TFocusRectangle
// =================================================================================================

procedure TFocusRectangle.SetBorderWidth(Value: Integer);
begin
	if FBorderWidth <> Value then
	begin
		FBorderWidth := Value;
		if FAlignedTo <> nil then
			AlignToControl(FAlignedTo);
	end;
end;

procedure TFocusRectangle.AlignToControl(Ctrl: TControl);
begin
	if Ctrl <> nil then
	begin
		if (Ctrl is TWinControl) then
		begin
			Parent := TWinControl(Ctrl);
			Align := alClient;
		end
		else
		begin
			Align := alNone;
			Parent := Ctrl.Parent;
			BoundsRect := Bounds(Ctrl.Left-FBorderWidth, Ctrl.Top-FBorderWidth,
				Ctrl.Width+(FBorderWidth*2), Ctrl.Height+(FBorderWidth*2));
			Anchors := Ctrl.Anchors;
		end;
	end;
	FAlignedTo := Ctrl;
end;


end.

