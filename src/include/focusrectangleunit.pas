unit FocusRectangleUnit;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics,
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
	const
		MAXLEVELS = 9; // maximum nesting level
	private
		FFocusColor,
		FLockedColor: TColor;
		CurrentLevel: Integer;
		PrevCtrl:  array[0..MAXLEVELS] of TControl;
		//PrevColor: array[0..MAXLEVELS] of TColor;
		procedure ShowFocusRect;
		function  CanFocusControl(Ctrl: TControl): Boolean; inline;
		procedure SetFocusColor(Value: TColor);
		procedure SetLockedColor(Value: TColor);
	public
		Root,
		CurrentItem:    TFocusableControl;
		ActiveControl:  TControl;
		FocusRectangle: TFocusRectangle;
		OnSelect,
		OnAscend,
		OnDescend,
		OnLock,
		OnUnlock:       TNotifyEvent;

		procedure Lock;
		procedure Unlock;
		procedure SelectControl(Ctrl: TControl);
		function  SelectNext:     TFocusableControl;
		function  SelectPrevious: TFocusableControl;
		function  Ascend:         TFocusableControl;
		function  Descend:        TFocusableControl;
		function  GetItem:        TControl;

		constructor Create;
		destructor  Destroy; override;

		property FocusColor:  TColor read FFocusColor  write SetFocusColor;
		property LockedColor: TColor read FLockedColor write SetLockedColor;
	end;


implementation

{$IFDEF DEBUG}
uses Form.Main;
{$ENDIF}

// =================================================================================================
// TFocusableControls
// =================================================================================================

constructor TFocusableControls.Create;
begin
	inherited Create;

	Root := TFocusableControl.Create;

	CurrentItem := nil;
	CurrentLevel := 0;
	FFocusColor  := clRed;
	FLockedColor := clYellow;

	FocusRectangle := TFocusRectangle.Create(nil);
	FocusRectangle.BorderWidth := 2;
	FocusRectangle.Shape := stRectangle;
	FocusRectangle.Brush.Style := bsClear; // bsFDiagonal;
	//FocusRectangle.Brush.Color := clMaroon;
	FocusRectangle.Pen.Style := psSolid;
	FocusRectangle.Pen.Color := FFocusColor;
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

function TFocusableControls.CanFocusControl(Ctrl: TControl): Boolean;
begin
	Result := (Ctrl <> nil) and (Ctrl.Visible) and (Ctrl.Enabled) and (Ctrl.Parent.Visible);
end;

procedure TFocusableControls.Lock;
begin
	if CurrentItem <> nil then
	begin
		FocusRectangle.Pen.Color := FLockedColor;
		ActiveControl := CurrentItem.Data;
		if ActiveControl <> nil then
			if Assigned(OnLock) then OnLock(Self);
	end;
end;

procedure TFocusableControls.Unlock;
begin
	if ActiveControl <> nil then
	begin
		ActiveControl := nil;
		FocusRectangle.Pen.Color := FFocusColor;
		if Assigned(OnUnlock) then OnUnlock(Self);
	end;
end;

procedure TFocusableControls.SelectControl(Ctrl: TControl);
var
	Item: TFocusableControl;
begin
	if ActiveControl <> nil then
		ActiveControl := Ctrl;
	Item := Root;
	if Item.Data <> Ctrl then
	repeat
		if Item <> nil then Item := Item.GetNext;
	until (Item = nil) or (Item.Data = Ctrl);
	if Item <> nil then
	begin
		CurrentItem := Item;
		if Assigned(OnSelect) then OnSelect(Self);
	end;
	ShowFocusRect;
end;

function TFocusableControls.SelectNext: TFocusableControl;
begin
	if ActiveControl <> nil then Exit;

	if CurrentItem = nil then
		CurrentItem := Root.GetFirstChild
	else
		CurrentItem := CurrentItem.GetNextSibling;

	if not CanFocusControl(CurrentItem.Data) then
		CurrentItem := SelectNext;

	if ActiveControl <> nil then
		ActiveControl := CurrentItem.Data;

	if Assigned(OnSelect) then OnSelect(Self);
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

	if not CanFocusControl(CurrentItem.Data) then
		CurrentItem := SelectPrevious;

	if ActiveControl <> nil then
		ActiveControl := CurrentItem.Data;

	if Assigned(OnSelect) then OnSelect(Self);
	Result := CurrentItem;
	ShowFocusRect;
end;

function TFocusableControls.Ascend: TFocusableControl;
begin
	if CurrentItem <> nil then
	begin
		if ActiveControl = nil then
			CurrentItem := CurrentItem.Parent;
	end;
	if CurrentItem = Root then
	begin
		FocusRectangle.Visible := False;
		PrevCtrl[1] := nil;
		CurrentItem := nil;
	end;

	Unlock;

	if Assigned(OnAscend) then OnAscend(Self);

	Result := CurrentItem;
	ShowFocusRect;
end;

function TFocusableControls.Descend: TFocusableControl;
begin
	if CurrentItem = nil then
		CurrentItem := Root;
	if CurrentItem.Count > 0 then
	begin
		Unlock;
		if Assigned(OnDescend) then OnDescend(Self);
		CurrentItem := CurrentItem.GetFirstChild;
		if Assigned(OnSelect) then OnSelect(Self);
	end
	else
		Lock;

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

		//if (CurrentLevel = Level) and (PrevCtrl[Level] = Ctrl) then Exit;

		//if (Ctrl is TPanel) or (Ctrl is ThListView) or (Ctrl is ThShellTree) then
		if (Ctrl is TFrame) or (Ctrl is TCustomForm) or
			((Ctrl is TWinControl) and (TWinControl(Ctrl).TabStop)) then
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

		{$IFDEF DEBUG}
		MainForm.Caption := Format('%s: %s (%d,%d  %d*%d)', [Ctrl.ClassName, Ctrl.Name,
			FocusRectangle.Left, FocusRectangle.Top, FocusRectangle.Width, FocusRectangle.Height]);
		{$ENDIF}
	end
	else
	begin
		FocusRectangle.Visible := False;
	end;
end;

procedure TFocusableControls.SetFocusColor(Value: TColor);
begin
	if FFocusColor <> Value then
	begin
		FFocusColor := Value;
		if ActiveControl = nil then
			FocusRectangle.Pen.Color := Value;
	end;
end;

procedure TFocusableControls.SetLockedColor(Value: TColor);
begin
	if FLockedColor <> Value then
	begin
		FLockedColor := Value;
		if ActiveControl <> nil then
			FocusRectangle.Pen.Color := Value;
	end;
end;

function TFocusableControls.GetItem: TControl;
begin
	if CurrentItem = nil then
		Result := nil
	else
		Result := CurrentItem.Data;
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
		if (Ctrl is TWinControl) or (Ctrl is TFrame) or (Ctrl is TCustomForm) then
		begin
			Parent := TWinControl(Ctrl);
			Align := alClient;
		end
		else
		begin
			Align := alNone;
			Parent := Ctrl.Parent;
			BoundsRect := Bounds(Ctrl.Left - FBorderWidth, Ctrl.Top - FBorderWidth,
				Ctrl.Width + (FBorderWidth*2), Ctrl.Height + (FBorderWidth*2));
			Anchors := Ctrl.Anchors;
		end;
		BringToFront;
	end;
	FAlignedTo := Ctrl;
end;


end.

