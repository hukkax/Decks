unit Form.Main;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
	Types, ShellCtrls, ComCtrls, Menus, FGL, LCLIntf, LCLType,
	BGRABitmap, BGRABitmapTypes, BGRAVirtualScreen,
	hListView, DecksShellTree, DecksButton, hSlider, hKnob, DecksLabel, DecksPanel,
	Decks.Config, Decks.Audio, Decks.MIDI, Decks.Effects, Decks.Deck,
	Decks.SongInfo, Decks.TagScanner, Frame.Deck;

{$WARN 5024 off : Parameter "$1" not used}

const
	SupportedFormats = '.mp3 .ogg .wav .it .s3m .xm .mod .sid .nsf';

	STR_SYM_PARENT = '📂';
	STR_SYM_FOLDER = '🖿 ';
	STR_SYM_DRIVES = '🖴';

	COLOR_FILE_PARENT    = $4488FF;
	COLOR_FILE_DIRECTORY = $88CCEE;
	COLOR_FILE_DEFAULT   = $AAAAAA;
	COLOR_FILE_HASBPM    = $DDEEFF;
	COLOR_FILE_PLAYED    = $5EB078;
	COLOR_BG_PARENT      = $12151C;
	COLOR_BG_DIRECTORY   = $111213;
	COLOR_FILE_DRIVES    = COLOR_FILE_PARENT;
	COLOR_BG_DRIVES      = COLOR_BG_PARENT;

	LI_NORMAL = 0;
	LI_HASEMBEDDEDIMAGE = 1;
	LI_ISDIRECTORY = 2;

type
	TPlayedFileInfo = class
	public
		Filename,
		Artist, Title: String;
		Timestamp:     TDateTime;
		constructor Create(const AFilename: String; ATime: TDateTime);
	end;

	{TFocusableControl = class
	public
		Control: TControl;
		Parent:  TFocusableControl;
		Level:   Integer;

		function  AddChild(AControl: TControl): TFocusableControl;
		procedure AddChildren;
	end;

	TFocusableControlList = TFPGObjectList<TFocusableControl>;

	TFocusableControls = class
	public
		Items:       TFocusableControlList;
		CurrentItem: TFocusableControl;

		function     SelectPrevious: TFocusableControl;
		function     SelectNext:     TFocusableControl;
		function     AddControl(AControl: TControl; AAddChildren: Boolean = False): TFocusableControl;

		constructor  Create;
		destructor   Destroy; override;
	end;}

	TMainForm = class(TForm)
		miMIDIInput: TMenuItem;
		PopupMenu: TPopupMenu;
		ImageListIcons: TImageList;
		miNewDeck: TMenuItem;
		miSep: TMenuItem;
		PanelTop: TPanel;
		PopupFile: TPopupMenu;
		SplitterDecks: TSplitter;
		Timer: TTimer;
		miFileActions: TMenuItem;
		miSep2: TMenuItem;
		miFileRename: TMenuItem;
		miFileDelete: TMenuItem;
		miEnableMixer: TMenuItem;
		MenuItem1: TMenuItem;
		MenuItem2: TMenuItem;
		miAbout: TMenuItem;
		miEnableEffects: TMenuItem;
		PopupListHeader: TPopupMenu;
		PanelMain: TDecksPanel;
		lBPM: TDecksLabel;
		sBPM: TDecksGaugeBar;
		bMainMenu: TDecksButton;
		bToggleEffects: TDecksButton;
		bToggleMixer: TDecksButton;
		bToggleLeftPane: TDecksButton;
		bToggleGraphLines: TDecksButton;
		bToggleWaveDual: TDecksButton;
		lCPU: TDecksLabel;
		bToggleTracklist: TDecksButton;
		MixerPanel: TDecksPanel;
		sEQ1L: ThKnob;
		sEQ1M: ThKnob;
		sEQ1H: ThKnob;
		sEQ2L: ThKnob;
		sEQ2M: ThKnob;
		sEQ2H: ThKnob;
		sFader: TDecksGaugeBar;
		pbBeats: TBGRAVirtualScreen;
		PanelWin: TDecksPanel;
		bWinMin: TDecksButton;
		bWinMax: TDecksButton;
		bWinClose: TDecksButton;
		BottomPanel: TDecksPanel;
		LeftPanel: TPanel;
		ListDirs: TDecksShellTree;
		sDirs: TDecksRangeBar;
		EmbeddedImage: TImage;
		FileList: ThListView;
		SplitterBottom: TSplitter;
		sFiles: TDecksRangeBar;
		DeckPanel: TDecksPanel;
		miZoomIn: TMenuItem;
		miZoomOut: TMenuItem;
		MenuItem3: TMenuItem;
		PopupDir: TPopupMenu;
		miDirCopyFile: TMenuItem;
		miDirMoveFile: TMenuItem;
		eFileFilter: TEdit;
		shpFileFilter: TShape;
		FilesPanel: TDecksPanel;
		FilesToolbarPanel: TDecksPanel;
		bLoadDeckNew: TDecksButton;
		bLoadDeck1: TDecksButton;
		bLoadDeck2: TDecksButton;
		bLoadDeck3: TDecksButton;
		bFileRename: TDecksButton;
		bFileDelete: TDecksButton;
		procedure DeckPanelResize(Sender: TObject);
		procedure FileListDblClick(Sender: TObject);
		procedure FileListEnter(Sender: TObject);
		procedure FileListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure FormKeyPress(Sender: TObject; var Key: Char);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure FormResize(Sender: TObject);
		procedure ListDirsChange(Sender: TObject; Node: TTreeNode);
		procedure ListDirsCollapsed(Sender: TObject; Node: TTreeNode);
		procedure FileListClickItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; Item: ThListItem);
		procedure FileListSelectItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; Item: ThListItem);
		procedure miLoadFileClick(Sender: TObject);
		procedure PopupFilePopup(Sender: TObject);
		procedure sBPMChange(Sender: TObject);
		procedure sDirsChange(Sender: TObject);
		procedure ListDirsScrolled(Kind, Pos: Integer);
		procedure sEQ1LChange(Sender: TObject);
		procedure sEQ1LMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure sFaderChange(Sender: TObject);
		procedure sFaderMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure sFaderMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure TimerTimer(Sender: TObject);
		procedure miFileRenameClick(Sender: TObject);
		procedure miFileDeleteClick(Sender: TObject);
		procedure miAboutClick(Sender: TObject);
		procedure FileListMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
			MousePos: TPoint; var Handled: Boolean);
		procedure bToggleEffectsMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure bToggleLeftPaneMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure bToggleMixerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure LeftPanelResize(Sender: TObject);
		procedure EmbeddedImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure EmbeddedImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure bToggleGraphLinesMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure FormShow(Sender: TObject);
		procedure miEnableEffectsClick(Sender: TObject);
		procedure miEnableMixerClick(Sender: TObject);
		procedure FormActivate(Sender: TObject);
		procedure shpBorderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure bWinMinClick(Sender: TObject);
		procedure bWinMaxClick(Sender: TObject);
		procedure bWinCloseClick(Sender: TObject);
		procedure miZoomInClick(Sender: TObject);
		procedure miZoomOutClick(Sender: TObject);
		procedure miDirCopyFileClick(Sender: TObject);
		procedure ListDirsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
		procedure eFileFilterChange(Sender: TObject);
		procedure eFileFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	private
		PlayedFilenames: TStringList;
		IsShiftDown: Boolean;
		QueueFileSelect: String;
		OriginalWindowRect: TRect;
		InFullscreen: Boolean;

		procedure ResizeFrames;
		function  FindFileListEntry(const Filename: String): ThListItem;
		function  ReadImageFromTags(const Filename: String): Boolean;
		procedure SaveTrackList;
		procedure SetButtonDown(Button: TDecksButton; MenuItem: TMenuItem; Down: Boolean);
		procedure UpdateToggleButtons;
		procedure AlignEmbeddedImage;
		procedure OnFocusableControlDescend(Sender: TObject);
		procedure OnFocusableControlEnter(Sender: TObject);
		function  ClickButton(Ctrl: TControl; Pressed: Boolean): Boolean;
		procedure ListDrives;
		procedure UpdateFilelistToolbar(IsDir: Boolean);
	public
		FileListIsActive: Boolean;
		EQControls: array[1..2, TEQBand] of ThKnob;

		procedure ScaleTo(Percent: Word);
		procedure ToggleFullScreen;
		procedure UpdateMixerVisibility;
		procedure ApplyMixer(ApplyEQ: Boolean = True);
		procedure SetMasterTempo(BPM: Single);

		procedure SelectFileInFileList(const Filename: String; AllowChangeDir: Boolean);
		procedure SetActiveList(RightList: Boolean);
		procedure ListBrowse(Dir: Integer);
		procedure ListFilesInDir(const Dir: String);
		procedure UpdateFileInfos;
		procedure UpdateFileInfo(Filename: String); overload;
		procedure UpdateFileInfo(Deck: TDeck); overload;
		procedure MarkSongAsPlayed(Filename: String);

//		procedure MIDIEvent(var Msg: TLMessage); message WM_MIDI;
		procedure ASyncExecute(Data: PtrInt);
		procedure Execute(Action: TDecksAction; Pressed: Boolean = True; Value: Integer = 0);
		procedure UpdateController(Deck: TDeck; Event: Integer);

		procedure StartUpdate; inline;
		procedure EndUpdate; inline;

		procedure DeckLayoutChanged;
		function  CreateDeck: TDeck;
		procedure CloseDeck(DeckFrame: TDeckFrame);
		function  FindDeck(Index: Integer): TDeck;
		function  FindDeckForm(Index: Integer): TDeckFrame;
		procedure LoadDeck(Index: Integer = 0);

		procedure OnFileScanEvent(EventKind: TTagScannerEventKind; const Filename: String; Tags: PSongTags);
	end;

	TMixerDeck = record
		Deck: TDeck;
		BeatFadeCounter: Byte;
		Volume: Single;
		EQ: array[TEQBand] of Single;
		TempEQKnob: array[TEQBand] of Integer;
		procedure Apply(ApplyEQ: Boolean = True);
	end;

var
	MainForm: TMainForm;

	SelectedListItem: record
		Kind:     Integer;
		Filename: String;
		ListItem: ThListItem;
	end;

	AudioManager: TAudioManager;
	DeckList: TFPGObjectList<TDeck>;
	MixerDeck: array[1..2] of TMixerDeck;
	MIDI: TMIDI;
	MasterBPM: Single;
	MasterDeck: TDeckFrame;
	MasterDeckIndex: Integer;
	CurrentDir: String;
	CurrentScale: Word = 100;

	procedure SetMaster(Index: Byte);
	procedure SetMasterBPM(BPM: Single);

	procedure Log(const S: String);
	procedure ErrorMessage(const MsgText: String);
	procedure LoadButtonImages(Form: TWinControl; Path: String);


implementation

{$R *.lfm}

uses
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	Math, FileUtil, LazFileUtils, StrUtils,
	MouseWheelAccelerator, TextInputDialog,
	BCTypes, BCButton, TeeGenericTree, FocusRectangleUnit,
	Form.Tracklist,
	BASS, AudioTag, basetag, file_Wave, file_mp3, file_ogg,
	Decks.Song, Decks.Beatgraph;

var
	TagScanner: TTagScannerJob;
	FocusableControls: TFocusableControls;
	DeckInFocusableControls: array[0..2, 0..2] of TNode<TControl>;
	FocusedDeck: Integer = 0;
	HoveredDirectory: TTreeNode;
	{$IFDEF USEMIDI}
	BeatLedState: array[1..3] of Boolean;
	{$ENDIF}

{$WARN 5024 off : Parameter "$1" not used}

// ================================================================================================
// Utility
// ================================================================================================

procedure Log(const S: String);
begin
	{$IFDEF DEBUG}
	{$IFDEF WINDOWS}
	//MainForm.LogMemo.Lines.Add(S);
	{$ELSE}
	WriteLn(S);
	{$ENDIF}
	{$ENDIF}
end;

procedure ErrorMessage(const MsgText: String);
begin
	MessageDlg('Error', MsgText, mtError, [mbOK], '');
end;

procedure LoadButtonImages(Form: TWinControl; Path: String);
var
	I: Integer;
	ChildControl: TControl;
	Butt: TDecksButton;
	Pic: TPicture;
begin
	Path := IncludeTrailingPathDelimiter(Path);
	for I := 0 to Form.ControlCount - 1 do
	begin
		ChildControl := Form.Controls[I];
		if ChildControl is TDecksButton then
		begin
			Butt := TDecksButton(ChildControl);
			if Butt.HelpKeyword <> '' then
			begin
				Butt.Glyph := Graphics.TBitmap.Create;
				Pic := TPicture.Create;
				try
					Pic.LoadFromFile(Path + Butt.HelpKeyword);
					Butt.Glyph.Assign(Pic.Bitmap);
				finally
					Pic.Free;
				end;
				Butt.ShowCaption := False;
				Butt.HelpKeyword := '';
			end;
		end;
	end;
end;

procedure SetMaster(Index: Byte);
var
	Deck: TDeck;
	Form: TDeckFrame;
begin
	if Index > DeckList.Count then Exit;

	MasterDeck := nil;
	MasterDeckIndex := 0;

	for Deck in DeckList do
	begin
		Form := TDeckFrame(Deck.Form);

		if Deck.Index = Index then
		begin
			MasterDeck := Form;
			MasterDeckIndex := Index;
			Form.bMaster.Background.Color := Form.bMaster.Tag;
		end
		else
			Form.bMaster.Background.Color := Form.lTime.Color;
	end;
end;

procedure SetMasterBPM(BPM: Single);
var
	Deck: TDeck;
begin
	MasterBPM := BPM;
	for Deck in DeckList do
		Deck.SetBPM(BPM);
	MainForm.lBPM.Caption := Format('%.2f', [BPM]);
end;

// ================================================================================================
// TMixerDeck
// ================================================================================================

procedure TMixerDeck.Apply(ApplyEQ: Boolean = True);
var
	Band: TEQBand;
begin
	if Assigned(Deck) then
	begin
		Deck.SetVolume(Volume);
		if ApplyEQ then
			for Band in TEQBand do
				Deck.Equalizer.SetEQ(Band, EQ[Band]);
	end;
end;

// ------------------------------------------------------------------------------------------------
// MIDI Input

procedure TMainForm.ASyncExecute(Data: PtrInt);
var
	Params: TExecuteParams;
begin
	Params := PExecuteParams(Data)^;
	try
		if (not Application.Terminated) then
			Execute(Params.Action, Params.Pressed, Params.Value);
	finally
		Dispose(PExecuteParams(Data));
	end;
end;

procedure TMainForm.Execute(Action: TDecksAction; Pressed: Boolean = True; Value: Integer = 0);
const
	EQBandFrom: array[MIXER_EQ_LOW..MIXER_EQ_HIGH] of TEQBand =
		(EQ_BAND_LOW, EQ_BAND_MID, EQ_BAND_HIGH);
var
	Deck: TDeck;
	Form: TDeckFrame;
	DeckNum: Byte;
	S: String;
begin
	if Action.Kind = NO_ACTION then Exit;

	{$IFDEF USEMIDI}
	if MIDI.Debug then
	begin
		WriteStr(S, Action.Kind);
		Caption := S + Format('(%d)=%d', [Action.Param,Value]);
	end;
    {$ENDIF}

	Deck := nil;
	Form := nil;
	DeckNum := 0;

	// get a destination deck if action needs one
	if Action.Kind in ParameterizedActions then
	begin
		DeckNum := Action.Param;
		case DeckNum of
			1, 2: Deck := MixerDeck[DeckNum].Deck;
			else  if Assigned(MasterDeck) then Deck := MasterDeck.Deck;
		end;
		if Deck <> nil then
			Form := TDeckFrame(Deck.Form);

		{if (FileListIsActive) and (Action.Kind in [UI_SELECT_TOGGLE, UI_SELECT_OPEN]) then
			Action.Kind := DECK_LOAD;}
		if (Deck = nil) and (Action.Kind <> DECK_LOAD) then Exit;
	end;

	case Action.Kind of

	DECK_PLAY:			if Pressed then Form.bPlayClick(Self);
	DECK_CUE:			if (Deck.Paused) or (Deck.Cueing) then //Form.Cue(Pressed)
							Form.bPlay.SetMouseDown(mbRight, Pressed)
							else if Pressed then Form.JumpToCue;
	DECK_SYNC:			Form.bSync.SetMouseDown(mbLeft, Pressed); //if Pressed then Form.SetSynced(not Deck.Synced);
	DECK_REVERSE:		Deck.SetReverse(Pressed, Deck.Synced);
	DECK_LOAD:			if Pressed then LoadDeck(DeckNum);
	DECK_AMP:			Form.SetKnob(Form.SliderAmp, Trunc(((Value) / 128) * 200));
	DECK_BEND:			{if Pressed then Deck.BendStart(Value > 0, False)
							else Deck.BendStop;}
						if Value > 0 then Form.bBendUp.SetMouseDown(mbLeft, Pressed)
							else Form.bBendDown.SetMouseDown(mbLeft, Pressed);
	DECK_SEEK:			Form.SetCue(Types.Point(Max(0, Form.GraphCue.X + Value), 0));

	MIXER_CROSSFADER:	sFader.Position := Round((Value / 127) * 1000);
	MIXER_EQ_KILL:		if Pressed then Deck.ToggleEQKill(EQ_BAND_LOW);
	MIXER_EQ_LOW..
	MIXER_EQ_HIGH:		if DeckNum > 0 then Form.SetKnob(EQControls[DeckNum, EQBandFrom[Action.Kind]],
							Trunc(((Value - 64) / 128) * 3000));
	DECK_FX_FILTER:		if DeckNum > 0 then
							Form.SliderFxParam0.FloatPosition := ((Value - 64) / 127) * 2;

	UI_LIST:			if Pressed then SetActiveList(not FileListIsActive); //(Value > 0);
	UI_SELECT:			if Pressed then ListBrowse(Value);
	UI_SELECT_TOGGLE:	if Pressed then ListDirs.Selected.Expanded := not ListDirs.Selected.Expanded;
	UI_SELECT_OPEN:		if Pressed then ListDirsChange(ListDirs, ListDirs.Selected);

	UI_SELECT_ENTER:	if not ClickButton(FocusableControls.ActiveControl, Pressed) then
							if Pressed then FocusableControls.Descend;
	UI_SELECT_EXIT:		if Pressed then FocusableControls.Ascend;

	UI_MENU:			; // TODO

	DECK_LOOP,
	DECK_LOOP_SONG,
	DECK_LOOP_ZONE:		if Pressed then ClickButton(Form.LoopControls[Action.Kind], Pressed);

	end;
end;

procedure TMainForm.UpdateController(Deck: TDeck; Event: Integer);
var
	Num: Byte;
	B: Boolean;
begin
	{$IFNDEF USEMIDI}Exit;{$ENDIF}

	if Deck = nil then Exit;
	if Deck = MixerDeck[1].Deck then Num := 1
	else
	if Deck = MixerDeck[2].Deck then Num := 2
	else Exit;

	B := (Num = 2);

	case Event of

		MODE_PLAY_START:
			if Deck.Cueing then
				MIDI.SetLed(DECK_CUE, B)         // Cue on
			else
			begin
				MIDI.SetLed(DECK_PLAY, B);       // Play on
				MIDI.SetLed(DECK_CUE, B, False); // Cue off
			end;

		MODE_PLAY_WAITSYNC:
			MIDI.SetLed(DECK_CUE, B);

		MODE_PLAY_STOP, MODE_PLAY_PAUSE, MODE_PLAY_FAILURE:
			if Deck.Cueing then
				MIDI.SetLed(DECK_CUE,  B, False)  // Cue
			else
				MIDI.SetLed(DECK_PLAY, B, False); // Play

		MODE_EQ_KILL_ON, MODE_EQ_KILL_OFF:
		begin
			EQControls[Num, EQ_BAND_LOW].BorderColor :=
				IfThen(Event = MODE_EQ_KILL_ON, MixerPanel.Color, $00727578);
			MIDI.SetLed(MIXER_EQ_KILL, B, Event = MODE_EQ_KILL_ON);
		end;

		MODE_SYNC_ON, MODE_SYNC_OFF:
			MIDI.SetLed(DECK_SYNC, B, Deck.Synced);

		MODE_LOOP_ON, MODE_LOOP_OFF:
			MIDI.SetLed(DECK_LOOP, B, Event = MODE_LOOP_ON);

	end;
end;

procedure TMainForm.ListBrowse(Dir: Integer);
var
	i: Integer;
	Node: TTreeNode;
	Ctrl: TControl;
begin
	Ctrl := FocusableControls.ActiveControl;

	if Ctrl = nil then
	begin
		if Dir < 0 then
			FocusableControls.SelectPrevious
		else
		if Dir > 0 then
			FocusableControls.SelectNext;
	end
	else
	if Ctrl = FileList then // if FileListIsActive then
	begin
		i := FileList.ItemIndex + Dir;
		if (i >= 0) and (i < FileList.Items.Count) then
		begin
			FileList.ItemIndex := i;
			FileList.ScrollToView(FileList.Items[i]);
		end;
	end
	else
	if Ctrl = ListDirs then
	begin
		ListDirs.Tag := 1;
		if ListDirs.Selected = nil then
			ListDirs.Select(ListDirs.Items.GetFirstNode);
		Node := ListDirs.Selected;
		if Node = nil then Exit;
		if Dir > 0 then
		begin
			if Node = ListDirs.Items.GetLastNode then
				ListDirs.Select(ListDirs.Items.GetFirstNode)
			else
				ListDirs.MoveToNextNode(True);
		end
		else
		begin
			if Node = ListDirs.Items.GetFirstNode then
				ListDirs.Select(ListDirs.Items.GetLastNode)
			else
				ListDirs.MoveToPrevNode(True);
		end;
		if ListDirs.Selected = nil then
			ListDirs.Selected := Node
		else
			ListDirs.Selected.MakeVisible;
	end
	else
	begin
		if Ctrl is ThKnob then
		begin
			with ThKnob(Ctrl) do
			begin
				if Multiplier = 1 then
					Position := Position + Dir
				else
					Position := Position + Round( (((Max - Min) / 100) * Sign(Dir)) );
			end;
		end
		else
		if (Ctrl is TDecksGaugeBar) then
		begin
			with TDecksGaugeBar(Ctrl) do
				if Ctrl = sBPM then
					Position := Position + (100 * Sign(Dir))
				else
					Position := Position + (SmallChange * Sign(Dir));
		end
		else
		if (Ctrl is TDecksRangeBar) then
		begin
			with TDecksRangeBar(Ctrl) do
				Position := Position + (Increment * Sign(Dir));
		end;
	end;
end;

// ================================================================================================
// TMainForm
// ================================================================================================

procedure TMainForm.FormActivate(Sender: TObject);
begin
	{$IFDEF WINDOWS}
	if not Config.Window.ShowTitlebar then
	begin
		SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not(WS_CAPTION));
		MoveWindow(Handle, Left, Top, Width-1, Height, True);
	end;
	{$ENDIF}
	OnActivate := nil;
end;

procedure TMainForm.shpBorderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	{$IFDEF WINDOWS}
	ReleaseCapture;
	SendMessage(Self.Handle, WM_SYSCOMMAND, 61458, 0);
	{$ENDIF}
end;

procedure TMainForm.bWinMinClick(Sender: TObject);
begin
	Application.Minimize;
end;

procedure TMainForm.bWinMaxClick(Sender: TObject);
begin
	if WindowState <> wsMaximized then
	begin
		WindowState := wsMaximized;
		bWinMax.Caption := '🗗';
		bWinMax.Hint := 'Restore Down';
	end
	else
	begin
		{$IFDEF WINDOWS}
		ToggleFullScreen;
		{$ENDIF}
		WindowState := wsNormal;
		bWinMax.Caption := '🗖';
		bWinMax.Hint := 'Maximize';
	end;
end;

procedure TMainForm.bWinCloseClick(Sender: TObject);
begin
	Close;
end;

procedure TMainForm.ScaleTo(Percent: Word);
var
	BR: TRect;
begin
	if (Percent < 50) or (Percent > 500) then Exit;

	BR := BoundsRect;
	StartUpdate;
	Self.ScaleBy(Percent, CurrentScale);
	CurrentScale := Percent;
	Config.Window.Zoom := Percent;
	BoundsRect := BR;
	EndUpdate;
end;

procedure TMainForm.miZoomInClick(Sender: TObject);
begin
	ScaleTo(CurrentScale + 10);
end;

procedure TMainForm.miZoomOutClick(Sender: TObject);
begin
	ScaleTo(CurrentScale - 10);
end;

procedure TMainForm.ListDirsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
	HoveredDirectory := ListDirs.NodeUnderCursor;
end;

procedure TMainForm.eFileFilterChange(Sender: TObject);
begin
	FileList.Filter(eFileFilter.Text, False);

	if FileList.Filtered then
	begin
		eFileFilter.Font.Color := $A6A8AA;
		shpFileFilter.Pen.Style := psSolid;
	end
	else
	begin
		eFileFilter.Font.Color := $555555;
		shpFileFilter.Pen.Style := psClear;
	end;
end;

procedure TMainForm.eFileFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key in [VK_ESCAPE, VK_RETURN] then // unfocus
	begin
		FocusControl(eFileFilter.Parent);
		FocusableControls.Ascend;
	end
	else
	if (Key = VK_BACK) and (Shift = [ssCtrl]) then // ctrl-backspace to clear
	begin
		Key := 0;
		eFileFilter.Text := '';
	end;
end;

procedure TMainForm.miDirCopyFileClick(Sender: TObject);
var
	Dir, Filename: String;
	i: Integer;
begin
	if HoveredDirectory = nil then Exit;
	Dir := ListDirs.GetPathFromNode(HoveredDirectory);
	if Dir.IsEmpty then Exit;
	Filename := SelectedListItem.Filename;
	if Filename.IsEmpty then Exit;

	if Sender = miDirCopyFile then
	begin
		if CopyFile(Filename, Dir + ExtractFilename(Filename), True) then
			ShowMessage('File copied.')
		else
			ShowMessage('Error copying file!');
	end
	else
	if Sender = miDirMoveFile then
	begin
		if RenameFile(Filename, Dir + ExtractFilename(Filename)) then
		begin
			ShowMessage('File moved.');
			i := FileList.ItemIndex;
			FileList.Items.Delete(i);
			FileList.ItemIndex := i;
		end
		else
			ShowMessage('Error moving file!');
	end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
	i: Integer;
	{$IFDEF USEMIDI}
	S: String;
	{$ENDIF}
	mi: TMenuItem;
//	Dev: TAudioDevice;
//	FC, CC: TFocusableControl;
//	Ctrl: TControl;
begin
	// change working directory to the exe path in case
	// we're being run from the IDE; needed to make
	// relative paths in config work
	SetCurrentDir(ExtractFilePath(ParamStr(0)));

	DefaultFormatSettings.DecimalSeparator := '.';
	Config.Load;

	CurrentDir := Config.Directory.Audio;
	if not DirectoryExists(CurrentDir) then
		CurrentDir := Config.AppPath;

	{$IFDEF WINDOWS}
	if not Config.Window.ShowTitlebar then
	begin
		BorderIcons := [];
		BorderStyle := bsSingle;
	end;
	{$ELSE}
	PanelWin.Visible := False;
    {$ENDIF}

	Caption := AppVersionString.Replace('/', '-', [rfReplaceAll]);
	SelectedListItem.Filename := '';
	SelectedListItem.Kind := 0;
	SelectedListItem.ListItem := nil;
	MasterDeck := nil;
	PlayedFilenames := TStringList.Create;
	PlayedFilenames.OwnsObjects := True;
	AudioManager := TAudioManager.Create;
	DeckList := TFPGObjectList<TDeck>.Create(True);
	sBPMChange(Self);

	Width  := Trunc(Screen.DesktopWidth  * 0.75);
	Height := Trunc(Screen.DesktopHeight * 0.75);
	ScaleTo(Config.Window.Zoom);
	i := Config.Window.DeckPanelHeight;
	if i = 0 then i := Trunc(ClientHeight * 0.3);
	DeckPanel.Height := i;

	InFullscreen := False;
	PanelWin.Visible := InFullscreen;
	// ==========================================
	// Init list controls
	//
	with FileList do
	begin
		Columns.Clear;
		AddColumn('Filename', -54).AlwaysShow := True;
		AddColumn(' BPM', 60, taCenter).AlwaysShow := True;
		AddColumn('Duration', 62+8, taCenter);
		AddColumn('Bitrate', 46+8, taCenter);
		AddColumn('Year', 50, taCenter);
		AddColumn('Genre', 100);
		AddColumn('Artist', -16);
		AddColumn('Title', -16);
		AddColumn('Comment', -16);
		AddColumn('Size', 64, taRightJustify);
		AddColumn('Date', 100);
		for i := 0 to Columns.Count-1 do
			Columns[i].Tag := i;
		Font.Color := COLOR_FILE_DEFAULT;
	end;

	ListDirs.OnScroll := ListDirsScrolled;
	ListDirs.SelectionColor := FileList.ColorSelection;
	ListDirs.HotTrackColor := FileList.ColorHover;
	ListDirs.Path := CurrentDir;

	// ==========================================
	// Misc GUI fixings
	//
	Application.HintColor := FileList.Color;
	Screen.HintFont.Color := $EEEEEE;
	Screen.HintFont.Name := FileList.Font.Name;
	Screen.HintFont.Size := FileList.Font.Size;
	Application.ShowHint := True;
	{$IFDEF LCLGTK2} // fix buggy colors
	//shpBorder.Brush.Style := bsSolid; // it's white otherwise?
	{$ENDIF}

	// ==========================================
	// Init MIDI and list devices
	//
	{$IFDEF USEMIDI}
	MIDI.Init;
	if Assigned(MIDI.InputDeviceList) then
	begin
		for i := 0 to MIDI.InputDeviceList.Count-1 do
		begin
			S := MIDI.InputDeviceList[i];
			mi := TMenuItem.Create(miMIDIInput);
			mi.Caption := S;
			mi.GroupIndex := 1;
			mi.RadioItem := True;
			//mi.AutoCheck := True;
			mi.Checked := (i = MIDI.InDevice);
			miMIDIInput.Add(mi);
		end;
	end;
	{$ENDIF}
	miMIDIInput.Visible := miMIDIInput.Count > 0;

	// ==========================================
	// List audio devices
	//
	{for Dev in AudioManager.Devices do
	begin
		mi := TMenuItem.Create(miCueDevices);
		if Dev.Index = 1 then
			mi.Caption := 'None'
		else
			mi.Caption := Dev.Name;
		mi.Tag := Dev.Index - 1;
		mi.GroupIndex := 1;
		mi.AutoCheck := True;
		mi.RadioItem := True;
		mi.Checked := (mi.Tag = Config.Audio.CueDevice);
		mi.OnClick := CueDeviceChange;
		miCueDevices.Add(mi);
	end;}

	EQControls[1, EQ_BAND_LOW]  := sEQ1L;
	EQControls[1, EQ_BAND_MID]  := sEQ1M;
	EQControls[1, EQ_BAND_HIGH] := sEQ1H;
	EQControls[2, EQ_BAND_LOW]  := sEQ2L;
	EQControls[2, EQ_BAND_MID]  := sEQ2M;
	EQControls[2, EQ_BAND_HIGH] := sEQ2H;

	for i := 0 to High(Config.Window.FileList.ColumnVisible) do
		if i < FileList.Columns.Count then
			FileList.Columns[i].Visible := Config.Window.FileList.ColumnVisible[i];

	FocusableControls := TFocusableControls.Create;
	with FocusableControls do
	begin
		with Root.Add(PanelMain) do
		begin
//			Add(bToggleLeftPane);
			Add(bToggleEffects);
			Add(bToggleMixer);
			Add(bToggleGraphLines);
//			Add(bToggleTracklist);
//			Add(bToggleWaveDual);
			Add(sBPM);
		end;

		//with Root.Add(DeckPanel) do
		begin
			for i := 0 to High(DeckInFocusableControls) do
			begin
				DeckInFocusableControls[i,0] := Root.Add(nil); // graph
				DeckInFocusableControls[i,1] := Root.Add(nil); // mixer
				DeckInFocusableControls[i,2] := Root.Add(nil); // controls
			end;
		end;

		with Root.Add(MixerPanel) do begin
			Add(sEQ1L); Add(sEQ1M); Add(sEQ1H);
			Add(sFader);
			Add(sEQ2L); Add(sEQ2M); Add(sEQ2H);
		end;

		Root.Add(ListDirs);

		with Root.Add(FilesToolbarPanel) do
		begin
			Add(bLoadDeckNew);
			Add(bLoadDeck1);
			Add(bLoadDeck2);
			Add(bLoadDeck3);
			Add(bFileRename);
			Add(bFileDelete);
			Add(eFileFilter);
		end;

		Root.Add(FileList);

		OnDescend := OnFocusableControlDescend;
		OnLock    := OnFocusableControlEnter;
	end;

{	i := 0;
	FC := FocusableControls.Root;
	for CC in FC.Items do
	begin
		Memo1.Lines.Add(Format('%d (%d) %s:%s', [i, CC.Level, CC.Data.ClassName, CC.Data.Name ]));
		if CC.Count > 0 then
			Memo1.Lines.Add(Format('    %d children', [CC.Count]));
		Inc(i);
	end;}

	eFileFilterChange(Self);

	{COLOR_FILE_PARENT    = $4488FF;
	COLOR_FILE_DIRECTORY = $88CCEE;
	COLOR_FILE_DEFAULT   = $AAAAAA;
	COLOR_FILE_HASBPM    = $DDEEFF;
	COLOR_FILE_PLAYED    = $5EB078;
	COLOR_BG_PARENT      = $12151C;
	COLOR_BG_DIRECTORY   = $111213;
	COLOR_FILE_DRIVES    = COLOR_FILE_PARENT;
	COLOR_BG_DRIVES      = COLOR_BG_PARENT;

	Config.Theme.Strings.FileList.Directory := STR_SYM_FOLDER;
	Config.Theme.Strings.FileList.ParentDirectory := STR_SYM_PARENT;
	Config.Theme.Strings.FileList.Drives := STR_SYM_DRIVES;

	with Config.Theme.Colors.FileList do
	begin
		//FileItem.FgPlayed := ;
	end;
	}

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
	UpdateMixerVisibility;
	UpdateToggleButtons;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	{$IFNDEF DEBUG}
	if PlayedFilenames.Count > 5 then
	begin
		if MessageDlg('Save Tracklist', 'Save tracklist for current session?',
			TMsgDlgType.mtConfirmation, mbYesNo, 0, mbYes) <> mrNo then
				SaveTrackList;
	end;
	{$ENDIF}

	if TagScanner <> nil then
	begin
		TagScanner.Terminate;
		TagScanner.Free;
	end;

    FileList.Tag := 0;
	OnResize := nil;
	ListDirs.OnCollapsed := nil;
	CanClose := True;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
	i: Integer;
begin
	for i := 0 to High(Config.Window.FileList.ColumnVisible) do
		if i < FileList.Columns.Count then
			Config.Window.FileList.ColumnVisible[i] := FileList.Columns[i].Visible;
	Config.Window.DeckPanelHeight := DeckPanel.Height;

	Config.Save;
	{$IFDEF USEMIDI}
	MIDI.Uninit;
	{$ENDIF}
	FocusableControls.Free;
	PlayedFilenames.Free;
	MasterDeck := nil;
	//FileList.Free;
	DeckList.Free;
	AudioManager.Free;

	CloseAction := caFree;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
	{$IFNDEF WINDOWS}
	FileList.Invalidate;
	MixerPanel.Invalidate;
	Invalidate;
	{$ENDIF}
	if FocusableControls <> nil then
		FocusableControls.FocusRectangle.Realign;
end;

procedure TMainForm.ToggleFullScreen;
begin
	{$IFDEF WINDOWS}
	if not InFullscreen then
	begin
		OriginalWindowRect := Bounds(Left, Top, Width, Height);
		SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not(WS_CAPTION));
		MoveWindow(Handle, Left, Top, Width-1, Height, True);
		WindowState := wsFullScreen;
		InFullscreen := True;
	end
	else
	begin
		SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or WS_CAPTION);
		MoveWindow(Handle, OriginalWindowRect.Left, OriginalWindowRect.Top, OriginalWindowRect.Width, OriginalWindowRect.Height, True);
		WindowState := wsNormal;
		InFullscreen := False;
	end;
	PanelWin.Visible := InFullscreen;
	{$ENDIF}
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
var
	Act: TDecksAction;
begin
	if Key = VK_SHIFT then IsShiftDown := True;

	case Key of

		{$IFDEF WINDOWS}
		VK_F11:
			ToggleFullScreen;
		{$ENDIF}

		VK_RETURN:
		begin
			Act.Kind := UI_SELECT_ENTER;
			Act.Param := 0;
			Execute(Act);
			Key := 0;
		end;

		VK_BACK:
		if not eFileFilter.Focused then
		begin
			Act.Kind := UI_SELECT_EXIT;
			Act.Param := 0;
			Execute(Act);
			Key := 0;
		end;

		VK_LEFT:
		begin
			ListBrowse(-1);
			Key := 0;
		end;

		VK_RIGHT:
		begin
			ListBrowse(+1);
			Key := 0;
		end;

		VK_UP:
		begin
			if FocusableControls.ActiveControl is TWinControl then
				ListBrowse(-1)
			else
			if MasterDeckIndex > 1 then
				SetMaster(MasterDeckIndex-1);
			Key := 0;
		end;

		VK_DOWN:
		begin
			if FocusableControls.ActiveControl is TWinControl then
				ListBrowse(+1)
			else
				SetMaster(MasterDeckIndex+1);
			Key := 0;
		end;

		VK_DELETE:
			CloseDeck(MasterDeck);

		VK_CONTROL:
		if not eFileFilter.Focused then
		begin
			if (MixerDeck[1].Deck <> nil) and (GetKeyState(VK_LCONTROL) < 0) then
				TDeckFrame(MixerDeck[1].Deck.Form).Cue(True);
			if (MixerDeck[2].Deck <> nil) and (GetKeyState(VK_RCONTROL) < 0) then
				TDeckFrame(MixerDeck[2].Deck.Form).Cue(True);
		end;

	else
		if (MasterDeck <> nil) and (MasterDeck.ProcessKeyDown(Key, Shift)) then
			Key := 0;
	end;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if MasterDeck <> nil then
		if MasterDeck.ProcessKeyPress(Key) then
		begin
			Key := #0;
			Exit;
		end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of

		VK_SHIFT:
			IsShiftDown := False;

		VK_CONTROL:
			if not eFileFilter.Focused then
			begin
				if (MixerDeck[1].Deck <> nil) and not (GetKeyState(VK_LCONTROL) < 0) then
					TDeckFrame(MixerDeck[1].Deck.Form).Cue(False);
				if (MixerDeck[2].Deck <> nil) and not (GetKeyState(VK_RCONTROL) < 0) then
					TDeckFrame(MixerDeck[2].Deck.Form).Cue(False);
				Exit;
			end;
	end;

	if MasterDeck <> nil then
		if MasterDeck.ProcessKeyUp(Key, Shift) then
		begin
			Key := 0;
			Exit;
		end;
end;

procedure TMainForm.StartUpdate;
begin
	BeginFormUpdate;
end;

procedure TMainForm.EndUpdate;
begin
	EndFormUpdate;
end;

procedure TMainForm.SetActiveList(RightList: Boolean);
const
	ColActive = clBlack;
	ColInactive = $212223;
begin
	FileListIsActive := RightList;
	if RightList then
	begin
		FocusableControls.ActiveControl := FileList;
		FileList.Color := ColActive;
		ListDirs.BackgroundColor := ColInactive;
	end
	else
	begin
		FocusableControls.ActiveControl := ListDirs;
		FileList.Color := ColInactive;
		ListDirs.BackgroundColor := ColActive;
	end;
end;

procedure TMainForm.SaveTrackList;
var
	i: Integer;
	S: String;
	Item: TPlayedFileInfo;
	First: TDateTime = 0;
	Sl: TStringList;
begin
	Sl := TStringList.Create;
	try
		for i := 0 to PlayedFilenames.Count-1 do
		begin
			Item := TPlayedFileInfo(PlayedFilenames.Objects[i]);
			if Item = nil then Continue;
			if First = 0 then
				First := Item.Timestamp;
			if (Item.Artist.IsEmpty) or (Item.Title.IsEmpty) then
				S := Item.Filename
			else
				S := Format('%s - %s', [Item.Artist, Item.Title]);
			Sl.Add(Format('%s %s', [FormatDateTime('hh:nn:ss', Item.Timestamp-First), S]));
		end;
		if Sl.Count > 0 then
			Sl.SaveToFile(IncludeTrailingPathDelimiter(Config.AppPath) + Format(
				'tracklist-%s.txt', [FormatDateTime('YYYY-MM-DD_hh-nn', First)]));
	finally
		Sl.Free;
	end;
end;

function TMainForm.ReadImageFromTags(const Filename: String): Boolean;
var
	TagReader: TTagReader;
	ImageStream: TMemoryStream;
begin
	Result := False;
	TagReader := ReadTags(Filename);

	if Assigned(TagReader) then
	try
		TagReader.GetCommonTags;
		Result := TagReader.Tags.ImageCount > 0;
		if Result then
		begin
			TagReader.Tags.Images[0].Image.Position := 0;
			ImageStream := TMemoryStream.Create;
			ImageStream.CopyFrom(TagReader.Tags.Images[0].Image,
				TagReader.Tags.Images[0].Image.Size);
			TagReader.Tags.Images[0].Image.Position := 0;
			try
				EmbeddedImage.Picture.LoadFromStream(TagReader.Tags.Images[0].Image);
			except
				Result := False;
			end;
			ImageStream.Free;
		end;
	finally
		TagReader.Free;
	end
	else
		Result := False;
end;

procedure TMainForm.FileListClickItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; Item: ThListItem);
var
	ShiftDown: Boolean;
	Col, i: Integer;
	Tag, S: String;
	TagReader: TTagReader;
	Tags: TCommonTags;
begin
	if (Button <> mbMiddle) or (SelectedListItem.Filename.IsEmpty) then Exit;

	Col := FileList.ClickedColumn;
	S := FileList.GetSubItemFor(Item, Col);

	case Col of
		COLUMN_FILENAME:
			miFileRename.Click;

		COLUMN_YEAR, COLUMN_GENRE, COLUMN_ARTIST, COLUMN_TITLE, COLUMN_COMMENT:
		begin
			{case Col of
				COLUMN_GENRE, COLUMN_ARTIST, COLUMN_TITLE:
					Tag := ToTitleCase(S);
				else Tag := S;
			end;}
			Tag := S;
			ShiftDown := (ssShift in Shift);
			if not ShiftDown then
				if not AskString('Tag Editor: ' + FileList.Columns[Col].Caption, Tag)
					then Exit;

			if (ShiftDown) or (Tag <> S) then
			try
				TagReader := ReadTags(SelectedListItem.Filename);
				if not TagReader.isUpdateable then
				begin
					ShowMessage('Sorry, unsupported tag format!');
					Exit;
				end;

				Tags := TagReader.GetCommonTags;
				if ShiftDown then
				begin
					if Tags.Artist <> '' then Exit;
					Tag := Tags.Title;
					i := Pos(' - ', Tag);
					if i < 1 then i := Pos(' / ', Tag);
					if i < 1 then Exit;
					Tags.Artist := Trim(Copy(Tag, 1, i));
					Tags.Title  := Trim(Copy(Tag, i+3, MaxInt));
				end
				else
				begin
					case Col of
						COLUMN_YEAR:	Tags.Year := Tag;
						COLUMN_GENRE:	Tags.Genre := Tag;
						COLUMN_ARTIST:	Tags.Artist := Tag;
						COLUMN_TITLE:	Tags.Title := Tag;
						COLUMN_COMMENT:	Tags.Comment := Tag;
						else Exit;
					end;
				end;
				TagReader.SetCommonTags(Tags);
				TagReader.UpdateFile;

				if ShiftDown then
				begin
					Item.SubItems[COLUMN_ARTIST-1] := Tags.Artist;
					Item.SubItems[COLUMN_TITLE-1]  := Tags.Title;
				end
				else
					if Col > 0 then
						Item.SubItems[Col-1] := Tag;

				FileList.Invalidate;
				Application.ProcessMessages;
			finally
				TagReader.Free;
			end;
		end;
	end;
end;

procedure TMainForm.AlignEmbeddedImage;
var
	W, H: Integer;
	AR: Single;
begin
	if EmbeddedImage.Picture.Width < 10 then Exit;
	W := LeftPanel.ClientWidth;
	AR := EmbeddedImage.Picture.Height / EmbeddedImage.Picture.Width;
	H := Round(AR * W);
	EmbeddedImage.Width  := W;
	EmbeddedImage.Height := H;
end;

constructor TPlayedFileInfo.Create(const AFilename: String; ATime: TDateTime);
begin
 	inherited Create;
	Filename := AFilename;
	Timestamp := ATime;
end;

procedure TMainForm.MarkSongAsPlayed(Filename: String);
var
	i: Integer;
	Item: ThListItem;
	Info: TPlayedFileInfo;
	S: String;
begin
	if PlayedFilenames.IndexOf(Filename) >= 0 then Exit;

	i := PlayedFilenames.Add(Filename);
	Info := TPlayedFileInfo.Create(Filename, Now);
	PlayedFilenames.Objects[i] := Info;
	Item := FindFileListEntry(ExtractFileName(Filename));
	if Item <> nil then
	begin
		Info.Artist := Item.SubItems[COLUMN_ARTIST-1];
		Info.Title  := Item.SubItems[COLUMN_TITLE-1];
		Item.Color := COLOR_FILE_PLAYED;
		FileList.Invalidate;
	end;

	//if FormTracklist.Visible then
	begin
		Item := ThListItem.Create(FormTracklist.SongList);

		if Info.Artist.IsEmpty then
		begin
			if Info.Title.IsEmpty then
				S := ExtractFileNameWithoutExt(Info.Filename)
			else
				S := Info.Title;
		end
		else
		begin
			S := Format('%s - %s', [Info.Artist, Info.Title]);
		end;

		Item.Caption := S;
		with FormTracklist.SongList do
		begin
			Items.Add(Item);
			ScrollToBottom;
			ItemIndex := Items.Count-1;
		end;
	end;
end;

procedure TMainForm.OnFileScanEvent(EventKind: TTagScannerEventKind; const Filename: String; Tags: PSongTags);
var
	Item: ThListItem;
	Fn, S: String;
	I: Integer;
begin
	case EventKind of

		tsFileScanStarted:
		begin
			bToggleLeftPane.Enabled := False;
			FileList.ItemIndex := -1;
		end;

		tsDirAdded:
		begin
			Item := ThListItem.Create(FileList);
			S := ExtractFileName(Filename);
			if S = '..' then
			begin
				S := ExtractFileName(ExtractFileDir(ExcludeTrailingPathDelimiter(CurrentDir)));
				if S = '' then
				begin
					{$IFDEF WINDOWS}
					S := ExtractFileDrive(CurrentDir) + '\';
					if S = ExtractFileDir(Filename) then
					begin
						// needed for network drives
						Item.Free;
						Exit;
					end;
					{$ELSE}
					S := '/';
					{$ENDIF}
				end;
				Item.Caption := STR_SYM_PARENT + '🗦' + S + '🗧';
				Item.Hint := '..';
				Item.Color := COLOR_FILE_PARENT;
				Item.Background := COLOR_BG_PARENT;
				Item.SortIndex := -2;
			end
			else
			begin
				Item.Caption := STR_SYM_FOLDER + S;
				Item.Hint := S;
				Item.Color := COLOR_FILE_DIRECTORY;
				Item.Background := COLOR_BG_DIRECTORY;
				Item.SortIndex := -1;
			end;
			Item.Tag := LI_ISDIRECTORY;
			FileList.Items.Add(Item);
			if FileList.ItemIndex < 0 then
				FileList.ItemIndex := 0;
			FileList.Invalidate;
		end;

		tsFileAdded:
		begin
			Item := FileList.AddItem(ExtractFileName(Filename));
			if PlayedFilenames.IndexOf(Filename) >= 0 then
				Item.Color := COLOR_FILE_PLAYED;
			if FileList.ItemIndex < 0 then
				FileList.ItemIndex := 0;
			FileList.Invalidate;
		end;

		tsFileScanFinished: // we can only sort by filename at this point
		begin
			{$IFDEF WINDOWS}
			// add a drive list entry if we're at a drive root
			I := -1;
			for Item in FileList.Items do
			begin
				if Item.SortIndex = -2 then
				begin
					I := FileList.Items.IndexOf(Item);
					Break;
				end;
			end;
			if I = -1 then
			begin
				Item := FileList.AddItem(STR_SYM_DRIVES + '🗦Drives🗧');
				Item.Hint := '/';
				Item.Tag := LI_ISDIRECTORY;
				Item.Color := COLOR_FILE_DRIVES;
				Item.Background := COLOR_BG_DRIVES;
				Item.SortIndex := -2;
			end;
			{$ENDIF}

			if FileList.SortColumn = 0 then
				FileList.SortItems;
			if not QueueFileSelect.IsEmpty then
			begin
				FileList.SelectedItem := FindFileListEntry(QueueFileSelect);
				QueueFileSelect := '';
			end;
			bToggleLeftPane.Enabled := True;
		end;

		tsTagScanStarted: ;

		tsTagsRead:
		begin
			Item := FindFileListEntry(Filename);
			if Item = nil then Exit;

			Item.SubItems.BeginUpdate;
			Item.SubItems.Clear;
			for I := COLUMN_BPM to COLUMN_LAST do
				Item.SubItems.Add('');

			if Tags.Info.BPM > 1 then
			begin
				S := Format('%.2f', [Tags.Info.BPM]);
				if Tags.Info.BPM < 100 then S := ' ' + S;
				if Item.Color = clNone then
					Item.Color := COLOR_FILE_HASBPM;
			end
			else
				S := '';
			Item.SubItems[COLUMN_BPM-1] := S;
			Item.SubItems[COLUMN_DURATION-1] := Tags.Duration;
			if Tags.Info.Bitrate > 0 then
			begin
				S := Tags.Info.Bitrate.ToString;
				if Tags.Info.Bitrate < 100 then S := ' ' + S;
				Item.SubItems[COLUMN_BITRATE-1] := S;
			end;
			if Tags.Year > 0 then
				Item.SubItems[COLUMN_YEAR-1]:= Tags.Year.ToString;

			Fn := IncludeTrailingPathDelimiter(Config.Directory.Audio) + Filename;
			Tags.Info.FileSize := FileSize(Fn);
			Tags.Info.FileDate := FileDate(Fn);

			Item.SubItems[COLUMN_GENRE-1]   := Tags.Genre;
			Item.SubItems[COLUMN_ARTIST-1]  := Tags.Artist;
			Item.SubItems[COLUMN_TITLE-1]   := Tags.Title;
			Item.SubItems[COLUMN_COMMENT-1] := Tags.Comment;
			Item.SubItems[COLUMN_FILESIZE-1]:= (Tags.Info.FileSize div 1024 div 1024).ToString + ' MB';
			Item.SubItems[COLUMN_FILEDATE-1]:= FormatDateTime('yyyy-mm-dd', Tags.Info.FileDate);

			Item.Tag := IfThen(Tags.HasImage, LI_HASEMBEDDEDIMAGE, LI_NORMAL);
			Item.SubItems.EndUpdate;
			FileList.Invalidate;
		end;

		tsTagScanFinished:
			FileList.SortItems;
	end;
end;

procedure TMainForm.UpdateFileInfo(Deck: TDeck);
var
	Item: ThListItem;
	S: String;
begin
    if Deck = nil then Exit;

	Item := FindFileListEntry(ExtractFileName(Deck.Filename));
	if Item = nil then Exit;

	if Deck.Bitrate > 0 then
	begin
		S := Deck.Bitrate.ToString;
		if Deck.Bitrate < 100 then S := ' ' + S;
		Item.SubItems[COLUMN_BITRATE-1]  := S;
	end;
	if Deck.Duration >= 1 then
		Item.SubItems[COLUMN_DURATION-1] :=
			FormatDateTime('nn:ss', Deck.Duration / SecsPerDay).Replace('.',':');

	FileList.Invalidate;
	Application.ProcessMessages;
end;

procedure TMainForm.UpdateFileInfo(Filename: String);
var
	Item: ThListItem;
	Info: TSongInfo;
	S: String;
begin
	Filename := ExtractFileName(Filename);
	Item := FindFileListEntry(Filename);
	if Item = nil then Exit;

	Info := GetSongInfo(Filename, nil);
	if Info.BPM > 1 then
	begin
		S := Format('%.2f', [Info.BPM]);
		if Item.Color = clNone then
			Item.Color := COLOR_FILE_HASBPM;
	end
	else
		S := '';

	Item.SubItems[COLUMN_BPM-1] := S;
	if Info.Bitrate > 0 then
		Item.SubItems[COLUMN_BITRATE-1]  := Info.Bitrate.ToString;
	if Info.Length >= 1 then
		Item.SubItems[COLUMN_DURATION-1] :=
			FormatDateTime('nn:ss', Info.Length / SecsPerDay).Replace('.',':');

	//Item.SubItems[COLUMN_FILESIZE-1] := Info.FileSize.ToString;
	//Item.SubItems[COLUMN_FILEDATE-1] := FormatDateTime('yyyy-mm-dd hh:nn', Info.FileDate);

	FileList.Invalidate;
	Application.ProcessMessages;
end;

procedure TMainForm.UpdateFileInfos;
var
	Item: ThListItem;
begin
	if FileList.Tag <> 1 then Exit;
	for Item in FileList.Items do
		if Item.Caption <> '' then
			UpdateFileInfo(Item.Caption);
end;

procedure TMainForm.ListDirsChange(Sender: TObject; Node: TTreeNode);
begin
	if (ListDirs.Tag = 0) and (Node <> nil) then
	begin
		FileList.ScrollPos := 0;
		ListFilesInDir(ListDirs.Path);
		if (Sender = nil) then
			Node.Expanded := not Node.Expanded;
	end;
	ListDirs.Tag := 0;
end;

procedure TMainForm.ListFilesInDir(const Dir: String);
begin
	if (Dir <> '') and (DirectoryExists(Dir)) then
	begin
		CurrentDir := IncludeTrailingPathDelimiter(Dir);
		Config.Directory.Audio := CurrentDir;

		FileList.Items.Clear;
		FileList.Tag := 0;

		if TagScanner <> nil then
		begin
			TagScanner.Terminate;
			TagScanner.Free;
		end;

		TagScanner := TTagScannerJob.Create;
		TagScanner.Directory   := CurrentDir;
		TagScanner.Extensions  := SupportedFormats;
		TagScanner.OnEvent     := OnFileScanEvent;
		TagScanner.Execute(True);
	end;

	FileList.Tag := 1;
end;

procedure TMainForm.ListDirsCollapsed(Sender: TObject; Node: TTreeNode);
begin
	FormResize(Self);
end;

procedure TMainForm.miLoadFileClick(Sender: TObject);
begin
	LoadDeck((Sender as TComponent).Tag);
end;

function TMainForm.FindDeckForm(Index: Integer): TDeckFrame;
var
	Deck: TDeck;
begin
	Deck := FindDeck(Index);
	if Deck <> nil then
		Result := TDeckFrame(Deck.Form)
	else
		Result := nil;
end;

function TMainForm.FindDeck(Index: Integer): TDeck;
var
	Deck: TDeck;
begin
	if Index > 0 then
		for Deck in DeckList do
			if Deck.Index = Index then
				Exit(Deck);
	Result := nil;
end;

procedure TMainForm.LoadDeck(Index: Integer = 0);
var
	Deck: TDeck;
	HadNone: Boolean;
begin
	if (SelectedListItem.ListItem = nil) or (SelectedListItem.Kind = LI_ISDIRECTORY) then Exit;

	HadNone := DeckList.Count = 0;
	Deck := FindDeck(Index);
	if Deck = nil then
		Deck := CreateDeck
	else
		if not Deck.Paused then Exit;

    if (Deck <> nil) and (Deck.Load(SelectedListItem.Filename)) then
	begin
		if (HadNone) and (Config.Deck.FirstSetsMasterBPM) then
			SetMasterTempo(Deck.AvgBPM);
	end;
end;

procedure TMainForm.SetMasterTempo(BPM: Single);
begin
	sBPM.Position := Trunc(BPM * 1000);
end;

procedure TMainForm.PopupFilePopup(Sender: TObject);
var
	Deck: TDeck;
	LI: ThListItem;
begin
	LI := FileList.SelectedItem;
	if (LI = nil) or (LI.Tag = LI_ISDIRECTORY) then
	begin
		Abort;
		Exit;
	end;
	miSep.Visible := (DeckList.Count > 0);
	for Deck in DeckList do
		Deck.UpdateMenuItem;
end;

procedure TMainForm.DeckPanelResize(Sender: TObject);
begin
	ResizeFrames;
end;

function TMainForm.FindFileListEntry(const Filename: String): ThListItem;
var
	Item: ThListItem;
begin
	FileNameCaseSensitive := False;
	for Item in FileList.Items do
		if SameFileName(Item.Caption, Filename) then
			Exit(Item);
	Result := nil;
end;

procedure TMainForm.SelectFileInFileList(const Filename: String; AllowChangeDir: Boolean);
var
	LI: ThListItem;
	Fn: String;
begin
	if Filename.IsEmpty then Exit;
	Fn := ExtractFilename(Filename);
	if Fn = Filename then Exit;

	QueueFileSelect := '';
	LI := FindFileListEntry(Fn);
	if LI <> nil then
		FileList.SelectedItem := LI
	else
	if AllowChangeDir then
	begin
		QueueFileSelect := Fn;
		Fn := ExtractFileDir(Filename);
		if not Fn.IsEmpty then
			ListDirs.Path := Fn
		else
			QueueFileSelect := '';
	end;
end;

procedure TMainForm.UpdateFilelistToolbar(IsDir: Boolean);
begin
	bLoadDeck1.Enabled := (not IsDir) and (DeckList.Count >= 1);
	bLoadDeck2.Enabled := (not IsDir) and (DeckList.Count >= 2);
	bLoadDeck3.Enabled := (not IsDir) and (DeckList.Count >= 3);
	bLoadDeckNew.Enabled := not IsDir;
	bFileRename.Enabled  := not IsDir;
	bFileDelete.Enabled  := not IsDir;
end;

procedure TMainForm.FileListSelectItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; Item: ThListItem);
var
	S: String;
	IsDir: Boolean;
begin
	if Item = nil then Exit;

	S := Item.Caption;

	EmbeddedImage.Height := 0;
	EmbeddedImage.Picture.Clear;

	if Item.Tag = LI_HASEMBEDDEDIMAGE then
	begin
		if ReadImageFromTags(CurrentDir + S) then
		begin
			AlignEmbeddedImage;
			Application.ProcessMessages;
		end;
	end;

	SelectedListItem.Kind := Item.Tag;
	SelectedListItem.ListItem := Item;
	IsDir := Item.Tag = LI_ISDIRECTORY;

	UpdateFilelistToolbar(IsDir);

	// get directory path from the list item
	if IsDir then
	begin
		S := Item.Hint;
		{$IFDEF WINDOWS}if S <> '/' then{$ENDIF} // special item for drives list
			S := CreateAbsolutePath(S, CurrentDir);
		SelectedListItem.Filename := S;
	end
	else
		SelectedListItem.Filename := CurrentDir + S;
end;

procedure TMainForm.ListDrives;
var
	Drive: Char;
	DriveLetter: String;
	OldMode: Word;

	procedure AddDrive(const Kind: String);
	var
		Item: ThListItem;
	begin
		Item := FileList.AddItem(STR_SYM_DRIVES + ' ' + DriveLetter);
		Item.Hint := DriveLetter;
		Item.Color := COLOR_FILE_PARENT;
		Item.Background := COLOR_BG_PARENT;
		Item.SortIndex := -2;
		Item.Tag := LI_ISDIRECTORY;
		Item.SubItems.Add('');
		Item.SubItems.Add(Kind);
	end;

begin
	FileList.Items.Clear;
	{$IFDEF WINDOWS}
	OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
	try
		for Drive := 'A' to 'Z' do
		begin
			DriveLetter := Drive + ':\';
			case GetDriveType(PChar(DriveLetter)) of
				DRIVE_REMOVABLE: AddDrive('Floppy');
				DRIVE_FIXED:     AddDrive('HD');
				DRIVE_REMOTE:    AddDrive('Network');
				DRIVE_CDROM:     AddDrive('CD');
				DRIVE_RAMDISK:   AddDrive('RAM');
			end;
		end;
	finally
		SetErrorMode(OldMode);
	end;
	{$ENDIF}
end;

procedure TMainForm.FileListDblClick(Sender: TObject);
var
	I: Integer;
	S: String;
begin
	S := SelectedListItem.Filename;
	if S.IsEmpty then
	begin
		I := FileList.ItemIndex;
		if I >= 0 then
		begin
			FileListSelectItem(Self, mbLeft, [], FileList.Items[I]);
			S := SelectedListItem.Filename;
		end
		else
			Exit;
	end;
	if not S.IsEmpty then
	begin
		if SelectedListItem.Kind = LI_ISDIRECTORY then
		begin
			{$IFDEF WINDOWS}
			// list drives
			if S = '/' then
				ListDrives
			else
			{$ENDIF}
				ListDirs.Path := SelectedListItem.Filename;
		end
		else
		begin
			MasterDeckIndex := 0;
			if DeckList.Count > 1 then
				for I := 0 to DeckList.Count-1 do
				with DeckList[I] do
					if (not Loaded) or (Paused) then
					begin
						MasterDeckIndex := Index;
						Break;
					end;
			LoadDeck(MasterDeckIndex);
		end;
	end;
end;

procedure TMainForm.FileListEnter(Sender: TObject);
begin
//	SetActiveList(FileList.Focused);
end;

procedure TMainForm.FileListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_RETURN then FileListDblClick(Self);
end;

procedure TMainForm.ResizeFrames;
var
	C, W: Integer;
	ADeck: TDeck;
	AForm: TBaseDeckFrame;
begin
	if (DeckList = nil) or (DeckList.Count < 1) then Exit;

    W := DeckPanel.Width div DeckList.Count;
	C := 0;

    for ADeck in DeckList do
	begin
		if ADeck = nil then Continue;
		AForm := ADeck.Form;
		if AForm = nil then Continue;
		AForm.SetBounds(C, 0, W, DeckPanel.ClientHeight);
		Inc(C, W);
	end;
end;

procedure TMainForm.UpdateMixerVisibility;
begin
	StartUpdate;
	MixerPanel.Visible := Config.Mixer.Enabled;
	MixerPanel.Enabled := MixerPanel.Visible;
	FormTracklist.Visible := Config.Window.Tracklist.Visible;
	EndUpdate;
end;

procedure TMainForm.ApplyMixer(ApplyEQ: Boolean = True);
begin
	if not Config.Mixer.Enabled then Exit;

	MixerDeck[1].Apply(ApplyEQ);
	MixerDeck[2].Apply(ApplyEQ);
end;

procedure TMainForm.DeckLayoutChanged;
var
	i: Integer;
	Deck: TDeck;
	DF: TDeckFrame;
begin
	UpdateFilelistToolbar(False);

	for i := 0 to DeckList.Count-1 do
	begin
		Deck := DeckList[i];
		Deck.Index := i+1;
		Deck.Form.Name := Deck.Form.Name + Deck.Index.ToString;
		DF := TDeckFrame(Deck.Form);
		DF.UpdateCaption;
		if i in [0,1] then
			MixerDeck[i+1].Deck := Deck;

		if i <= High(DeckInFocusableControls) then
		if DeckInFocusableControls[i,2].Data = nil then
		begin

			with DeckInFocusableControls[i,0] do
			begin
				Data := DF.pnlGraph;
//				Add(DF.SliderTempo);
//				Add(DF.SliderTempoFrac);
				Add(DF.bStart);
				Add(DF.bStore);
			end;

			with DeckInFocusableControls[i,1] do
			begin
				Data := DF.pnlControls;
				Add(DF.bPlay);
				Add(DF.bSync);
				Add(DF.bReverse);
				Add(DF.bBendUp);
				Add(DF.bBendDown);
				//
				Add(DF.bLoopSong); Add(DF.bLoopZone);
				Add(DF.bLoopBeat); Add(DF.bLoopBeat2);
				Add(DF.bLoopBar); Add(DF.bLoopBar2); Add(DF.bLoopBar4);
				//
				Add(DF.SliderAmp);
			end;

			with DeckInFocusableControls[i,2] do
			begin
				Data := DF.pnlEffects;
				{Add(DF.pnlEffectButtons);
				Add(DF.pnlEffectKnobs);
				Add(DF.pnlEffectLoop);}
				Add(DF.bEffect0); Add(DF.bEffect1);
				Add(DF.bEffect2); Add(DF.bEffect3);
				Add(DF.bEffect4); Add(DF.bEffect5);
				Add(DF.bEffect6); //Add(DF.bEffect7);
				Add(DF.SliderFxParam0); Add(DF.SliderFxParam1);
				Add(DF.SliderFxParam2); Add(DF.SliderFxParam3);
				Add(DF.SliderFxParam4); Add(DF.SliderFxParam5);
			end;

		end;
	end;

	if MixerDeck[1].Deck <> nil then
		MixerDeck[1].Deck.OtherDeck := MixerDeck[2].Deck;
	if MixerDeck[2].Deck <> nil then
		MixerDeck[2].Deck.OtherDeck := MixerDeck[1].Deck;
end;

function TMainForm.CreateDeck: TDeck;
var
	mi: TMenuItem;
begin
	BeginFormUpdate;

	Result := TDeck.Create;

	Result.Form := TDeckFrame.Create(DeckPanel);
	Result.Form.Deck := Result;
	Result.Form.Parent := DeckPanel;
	Result.Form.ScaleBy(CurrentScale, 100);
	TDeckFrame(Result.Form).Init(Result);
	TDeckFrame(Result.Form).bMaster.Tag := FileList.ColorSelection;

	if DeckList.Count < 2 then
		SetMaster(1);

	DeckLayoutChanged;

	sFaderChange(Self);
	TDeckFrame(Result.Form).SetSynced(True);

	mi := TMenuItem.Create(PopupFile);
	mi.OnClick := miLoadFileClick;
	PopupFile.Items.Insert(PopupFile.Items.Count - 4, mi);
	Result.MenuItem := mi;
	Result.UpdateMenuItem;

	ResizeFrames;
	EndFormUpdate;

	Timer.Enabled := True;
end;

procedure TMainForm.CloseDeck(DeckFrame: TDeckFrame);
var
	I, J: Integer;
	Deck: TDeck;
begin
	if DeckFrame = nil then Exit;
	Deck := DeckFrame.Deck;
	if not Deck.Paused then Exit;

	BeginFormUpdate;
	DeckFrame.Timer.Enabled := False;

	for I := 1 to 2 do
		if MixerDeck[I].Deck = Deck then
			MixerDeck[I].Deck := nil;

	I := DeckList.IndexOf(Deck);
	if I >= 0 then
	begin
		if I <= High(DeckInFocusableControls) then
		begin
			for J := 0 to 2 do
			begin
				DeckInFocusableControls[I,J].Clear;
				DeckInFocusableControls[I,J].Data := nil;
			end;
			if I = FocusedDeck-1 then
				FocusableControls.Reset;
		end;
		DeckList.Delete(I); // destroys the frame, too
	end;
	MasterDeck := nil;

	DeckLayoutChanged;
	ResizeFrames;
	SetMaster(1);

	EndFormUpdate;
end;

procedure TMainForm.sBPMChange(Sender: TObject);
begin
	SetMasterBPM(sBPM.Position / 1000);
end;

procedure TMainForm.ListDirsScrolled(Kind, Pos: Integer);
begin
	sDirs.OnChange := nil;
	sDirs.Range := ListDirs.GetMaxScrollY + ListDirs.ClientHeight;
	sDirs.Window := ListDirs.ClientHeight;
	sDirs.Position := ListDirs.ScrollY;
	sDirs.Increment := ListDirs.DefaultItemHeight;
	sDirs.OnChange := sDirsChange;
	sDirs.Visible := ListDirs.GetMaxScrollY > 0;
	sDirs.Repaint;
end;

procedure TMainForm.sEQ1LChange(Sender: TObject);
var
	S: ThKnob;
	D: Integer;
begin
	if (Sender is ThKnob) then
	begin
		S := Sender as ThKnob;
		D := IfThen(S.Left > MixerPanel.ClientWidth div 2, 2, 1);
		if S.Position = 0 then
			S.BorderColor := $727578
		else
		if S.Position < 0 then
			S.BorderColor := clMaroon
		else
		if S.Position > 0 then
			S.BorderColor := clGreen;
		MixerDeck[D].EQ[TEQBand(S.Tag)] := S.Position / 100;
		MixerDeck[D].Apply;
	end;
end;

procedure TMainForm.sEQ1LMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
var
	S: ThKnob;
	D: Integer;
begin
	if Button <> mbRight then Exit;
	if not (Sender is ThKnob) then Exit;

	S := Sender as ThKnob;
	D := IfThen(S.Left > MixerPanel.ClientWidth div 2, 2, 1);

	if S.Position > S.Min then
	begin
		MixerDeck[D].TempEQKnob[TEQBand(S.Tag)] := S.Position;
		S.Position := S.Min;
	end
	else
	begin
		S.Position := MixerDeck[D].TempEQKnob[TEQBand(S.Tag)];
	end;
end;

procedure TMainForm.sFaderChange(Sender: TObject);
begin
	if not Config.Mixer.Enabled then Exit;

	MixerDeck[1].Volume := Min((1000 - sFader.Position) * 2, 1000) / 1000;
	MixerDeck[2].Volume := Min(sFader.Position * 2, 1000) / 1000;
	ApplyMixer(False);
end;

procedure TMainForm.sFaderMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
const
	Threshold = 150;
var
	BW: Integer;
begin
	case Button of
		mbMiddle:
			sFader.Position := 500;

		mbRight:
		begin
			sFader.Tag := sFader.Position;
			if not sFader.ShowArrows then
				BW := 0
			else
			begin
				BW := IfThen(sFader.HandleSize = 0, 26, sFader.HandleSize-6);
				Dec(X, BW);
			end;
			Y := Round(1000 / (sFader.Width - (BW * 2)) * X);
			if Y <= Threshold then
				Y := 0
			else
			if Y >= (1000 - Threshold) then
				Y := 1000;
			sFader.Position := Y;
		end;
	end;
end;

procedure TMainForm.sFaderMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbRight then
		if sFader.Tag >= 0 then
		begin
			sFader.Position := sFader.Tag;
			sFader.Tag := -1;
		end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
	i, W, H: Integer;
	B: Boolean;
	Deck: TDeck;
	Col: TBGRAPixel;
begin
	Timer.Tag := Timer.Tag + 1;
	if Timer.Tag >= 60 then
	begin
		Timer.Tag := 0;
		lCPU.Caption := Format('%.1f', [BASS_GetCPU]) + '%';
	end;

	{$IFDEF USEMIDI}
	for i := 1 to 2 do
	begin
		Deck := MixerDeck[i].Deck;
		if Deck = nil then
			B := False
		else
		if (not Deck.Paused) and (not Deck.Cueing) then
			B := Deck.BeatFadeCounter >= 254
		else
			B := Deck.Cueing;
		if BeatLedState[i] <> B then
		begin
			MIDI.SetLed(DECK_CUE, i=2, B);
			BeatLedState[i] := B;
		end;
	end;
	{$ENDIF}

	if Config.Mixer.Enabled then
	begin
		W := pbBeats.ClientWidth div 2;
		H := pbBeats.ClientHeight - 1;
		for i := 0 to 1 do
		begin
			Deck := MixerDeck[i+1].Deck;
			if (Deck = nil) or (Deck.Paused) then
				Col := BGRABlack
			else
				Col := Grays[Deck.BeatFadeCounter];
			pbBeats.Bitmap.FillRect(Bounds(i*W, 0, W, H), Col, dmSet);
		end;
		pbBeats.Repaint;
	end;
end;

procedure TMainForm.sDirsChange(Sender: TObject);
begin
	ListDirs.ScrollY := Trunc(sDirs.Position);
end;

procedure TMainForm.miFileRenameClick(Sender: TObject);
var
	Path, Fn, Ext, NewName: String;
	ListItem: ThListItem;
begin
	if SelectedListItem.Filename.IsEmpty then Exit;

	Path := ExtractFilePath(SelectedListItem.Filename);
	Ext  := ExtractFileExt(SelectedListItem.Filename);
	Fn   := ExtractFileNameWithoutExt(ExtractFileName(SelectedListItem.Filename));
	NewName := Fn;

	if not IsShiftDown then
		NewName := ToTitleCase(NewName);

	if (not AskString('Rename File', NewName)) or (NewName = Fn) then Exit;
	NewName := NewName + Ext;

	ListItem := FindFileListEntry(Fn + Ext);
	Fn := IncludeTrailingPathDelimiter(Path) + NewName;

	if not RenameFile(SelectedListItem.Filename, Fn) then
	begin
		ErrorMessage('Could not rename the file.');
		Exit;
	end;

	Path := GetBPMFile(ExtractFileName(SelectedListItem.Filename));
	if FileExists(Path) then
		RenameFile(Path, GetBPMFile(NewName));

	if ListItem <> nil then
	begin
		ListItem.Caption := NewName;
		FileList.Invalidate;
	end;

	SelectedListItem.Filename := Fn;
end;

procedure TMainForm.miFileDeleteClick(Sender: TObject);
var
	ListItem: ThListItem;
	SelectedFile: String;
begin
	ListItem := SelectedListItem.ListItem;
	if (ListItem = nil) or (ListItem.Tag = LI_ISDIRECTORY) then Exit;

	SelectedFile := SelectedListItem.Filename;
	if SelectedFile.IsEmpty then Exit;

	if MessageDlg('Delete File', 'Are you sure?', mtConfirmation, mbYesNo, '') = mrYes then
	begin
		if not SysUtils.DeleteFile(SelectedFile) then
		begin
			ErrorMessage('Could not delete the file.');
			Exit;
		end;

		SelectedFile := GetBPMFile(ExtractFileName(SelectedFile));
		if FileExists(SelectedFile) then
		begin
			if MessageDlg('Delete Metadata', 'Also delete the associated .BPM file?',
				mtConfirmation, mbYesNo, '') = mrYes then
					SysUtils.DeleteFile(SelectedFile);
		end;

		FileList.Items.Remove(ListItem);
		FileList.Invalidate;
		SelectedListItem.Filename := '';
	end;
end;

procedure TMainForm.miAboutClick(Sender: TObject);
var
	S: String;
begin
	S := AppVersionString;
	S := S + LineEnding + 'BASS version ' + AudioManager.BASSVersion;
	ShowMessage(S);
end;

procedure TMainForm.FileListMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
	MousePos: TPoint; var Handled: Boolean);
begin
	Handled := True;
	WheelDelta := Round(WheelDelta * WheelAcceleration.Process(WHEEL_FILELIST, WheelDelta, 1.2) / 120);
	FileList.ScrollBy(0, WheelDelta);
end;

procedure TMainForm.LeftPanelResize(Sender: TObject);
begin
	ListDirsScrolled(0, 0);
	AlignEmbeddedImage;
end;

procedure TMainForm.SetButtonDown(Button: TDecksButton; MenuItem: TMenuItem; Down: Boolean);
begin
	if Button = nil then Exit;
	Button.Down := Down;
	if Down then
		Button.StateNormal.Background.Style := bbsColor
	else
		Button.StateNormal.Background.Style := bbsGradient;
	if MenuItem <> nil then MenuItem.Checked := Down;
end;

procedure TMainForm.UpdateToggleButtons;
begin
	SetButtonDown(bToggleEffects, miEnableEffects, Config.Effects.Enabled);
	SetButtonDown(bToggleMixer, miEnableMixer, Config.Mixer.Enabled);
	SetButtonDown(bToggleLeftPane, {miEnableLeftPane}nil, Config.Window.DirList.Enabled);
	SetButtonDown(bToggleGraphLines, nil, Config.Deck.BeatGraph.ShowHorizontalLines);
	SetButtonDown(bToggleWaveDual, nil, Config.Deck.Waveform.ShowDual);
	SetButtonDown(bToggleTracklist, nil, Config.Window.Tracklist.Visible);

	LeftPanel.Visible := Config.Window.DirList.Enabled;
end;

procedure TMainForm.bToggleLeftPaneMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Button <> mbLeft then Exit;
	StartUpdate;
	Config.Window.DirList.Enabled := not Config.Window.DirList.Enabled;
	UpdateToggleButtons;
	EndUpdate;
end;

procedure TMainForm.bToggleEffectsMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
var
	B: Boolean;
	H: Integer;
	Deck: TDeck;
begin
	if Button <> mbLeft then Exit;
	StartUpdate;
	B := not Config.Effects.Enabled;
	Config.Effects.Enabled := B;
	UpdateToggleButtons;
	if DeckList.Count > 0 then
	begin
		H := 53; // !!! pnlEffects.Height;
		DeckPanel.Height := MainForm.DeckPanel.Height + IfThen(B, H, -H);
		for Deck in DeckList do
			TDeckFrame(Deck.Form).ShowPanel_Effects;
	end;
	EndUpdate;
end;

procedure TMainForm.bToggleMixerMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Button <> mbLeft then Exit;
	Config.Mixer.Enabled := not Config.Mixer.Enabled;
	UpdateToggleButtons;
	UpdateMixerVisibility;
end;

procedure TMainForm.bToggleGraphLinesMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
var
	Deck: TDeck;
begin
	if Button <> mbLeft then Exit;
	if Sender = bToggleGraphLines then
		Config.Deck.BeatGraph.ShowHorizontalLines := not Config.Deck.BeatGraph.ShowHorizontalLines
	else
	if Sender = bToggleWaveDual then
		Config.Deck.Waveform.ShowDual := not Config.Deck.Waveform.ShowDual
	else
	if Sender = bToggleTracklist then
	begin
		Config.Window.Tracklist.Visible := not Config.Window.Tracklist.Visible;
		FormTracklist.Visible := Config.Window.Tracklist.Visible;
	end;
	UpdateToggleButtons;
	for Deck in DeckList do
		TDeckFrame(Deck.Form).FormResize(Self);
end;

procedure TMainForm.EmbeddedImageMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
var
	DW, DH, W, H: Integer;
	Scale: Single;
	DR: TRect;
begin
	DW := FileList.ClientWidth;
	DH := FileList.ClientHeight;
	W := EmbeddedImage.Picture.Bitmap.Width;
	H := EmbeddedImage.Picture.Bitmap.Height;

	Scale := Min(DW / W, DH / H) / 2;
	W := Trunc(W * Scale);
	H := Trunc(H * Scale);

	DW := DW div 2;
	DH := DH div 2;
	DR := Types.Rect(DW-W, DH-H, DW+W, DH+H);

	FileList.Canvas.AntialiasingMode := amOn;
	FileList.Canvas.StretchDraw(DR, EmbeddedImage.Picture.Bitmap);
	FileList.Canvas.AntialiasingMode := amDontCare;
end;

procedure TMainForm.EmbeddedImageMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	FileList.Invalidate;
end;

procedure TMainForm.miEnableEffectsClick(Sender: TObject);
begin
	bToggleEffectsMouseDown(Self, mbLeft, [], 0, 0);
end;

procedure TMainForm.miEnableMixerClick(Sender: TObject);
begin
	bToggleMixerMouseDown(Self, mbLeft, [], 0, 0);
end;

procedure TMainForm.OnFocusableControlDescend(Sender: TObject);
var
	Ctrl: TControl;
	i, DeckIndex: Integer;
begin
	Ctrl := FocusableControls.GetItem;

	// stupid hack to find out which Deck instance we are browsing
	if Ctrl is TPanel then
	begin
		for DeckIndex := 0 to High(DeckInFocusableControls) do
		begin
			for i := 0 to 2 do
				if Ctrl = DeckInFocusableControls[DeckIndex, i].Data then
				begin
					FocusedDeck := DeckIndex+1;
					Break;
				end;
		end;
	end;
end;

function TMainForm.ClickButton(Ctrl: TControl; Pressed: Boolean): Boolean;
var
	Btn: TDecksButton;
begin
	Result := False;
	if Ctrl = nil then
		Ctrl := FocusableControls.GetItem; // get focused item

	if Ctrl is TDecksButton then
	begin
		Btn := TDecksButton(Ctrl);
		if not Btn.Enabled then Exit;

		if (Pressed) and (FocusedDeck > 0) and (Btn.Style = bbtDropDown) and
			(Btn.Name.StartsWith('bEffect')) then
		begin
			// special handling for effect enable buttons
			if Assigned(Btn.OnMouseDown) then
			begin
				Btn.OnMouseDown(Btn, mbRight, [], 0, 0);
				if Btn.Down then
				begin // activate first effect knob when enabling an effect
					Ctrl := TDeckFrame(DeckList[FocusedDeck-1].Form).SliderFxParam0;
					if Ctrl <> nil then
						FocusableControls.SelectControl(Ctrl);
				end;
			end;
		end
		else
		begin
			if Pressed then // press a button
			begin
				if Assigned(Btn.OnSetDown) then
					Btn.Down := not Btn.Down
				else
				if Assigned(Btn.OnButtonClick) then
					Btn.OnButtonClick(Ctrl)
				else
				if Assigned(Btn.OnClick) then
					Btn.OnClick(Ctrl)
				else
				if Assigned(Btn.OnMouseDown) then
					Btn.OnMouseDown(Ctrl, mbLeft, [], 0, 0);
			end
			else // release button press
				if Assigned(Btn.OnMouseUp) then
					Btn.OnMouseUp(Ctrl, mbLeft, [], 0, 0);
		end;
		Result := True;
	end
	else
	if (Pressed) and (Ctrl is ThListView) then
	begin
		// "click" on a list = invoke doubleclick handler
		if (Ctrl = FocusableControls.ActiveControl) and
			(Assigned((Ctrl as ThListView).OnDblClick)) then
		begin
			ThListView(Ctrl).OnDblClick(Ctrl);
			Result := True;
		end;
	end
	else
	if (Pressed) and (Ctrl is TShellTreeView) then
	begin
		if (Ctrl = FocusableControls.ActiveControl) and
			(Assigned((Ctrl as TShellTreeView).OnChange)) then
		with TShellTreeView(Ctrl) do
		begin
			OnChange(nil, Selected);
			Result := True;
		end;
	end;
end;

procedure TMainForm.OnFocusableControlEnter(Sender: TObject);
var
	Ctrl: TControl;
begin
	Ctrl := FocusableControls.ActiveControl;
	if Ctrl is TDecksButton then
		FocusableControls.Unlock
	else
	if Ctrl is TWinControl then
		Self.ActiveControl := TWinControl(Ctrl);
end;

(*procedure TMainForm.CueDeviceChange(Sender: TObject);
var
	i: Integer;
begin
	if Sender is TMenuItem then
	begin
		i := (Sender as TMenuItem).Tag;
		Config.Audio.CueDevice := i;
		//InitDevice(i, Config.Audio.CueDevice); TODO
	end;
end;*)

end.
