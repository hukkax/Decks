unit Form.Main;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
	Types, ShellCtrls, ComCtrls, Menus, FGL, {LMessages,} LCLIntf, LCLType,
	BGRABitmap, BGRABitmapTypes, BCLabel, BGRAVirtualScreen,
	hListView, hShellTree, DecksButton, hSlider, hKnob,
	Decks.Config, Decks.Audio, Decks.MIDI, Decks.Effects, Decks.Deck,
	Decks.SongInfo, Decks.TagScanner,
	Frame.Deck;

{$WARN 5024 off : Parameter "$1" not used}

const
	SupportedFormats = '.mp3 .ogg .wav .it .s3m .xm .mod .sid .nsf';

	COLOR_FILE_DEFAULT = $AAAAAA;
	COLOR_FILE_HASBPM  = $DDEEFF;
	COLOR_FILE_PLAYED  = $5EB078;

	COLUMN_FILENAME  = 0;
	COLUMN_BPM       = 1;
	COLUMN_DURATION  = 2;
	COLUMN_BITRATE   = 3;
	COLUMN_YEAR      = 4;
	COLUMN_GENRE     = 5;
	COLUMN_ARTIST    = 6;
	COLUMN_TITLE     = 7;
	COLUMN_COMMENT   = 8;

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
		bMainMenu: TDecksButton;
		miMIDIInput: TMenuItem;
		pbBeats: TBGRAVirtualScreen;
		PopupMenu: TPopupMenu;
		sEQ1L: ThKnob;
		sEQ1M: ThKnob;
		sEQ1H: ThKnob;
		sEQ2L: ThKnob;
		sEQ2M: ThKnob;
		sEQ2H: ThKnob;
		lBPM: TBCLabel;
		FileList: ThListView;
		ImageListIcons: TImageList;
		ListDirs: ThShellTree;
		miNewDeck: TMenuItem;
		miSep: TMenuItem;
		LeftPanel: TPanel;
		MixerPanel: TPanel;
		sBPM: ThGaugeBar;
		sFader: ThGaugeBar;
		sDirs: ThRangeBar;
		RightPanel: TPanel;
		DeckPanel: TPanel;
		PanelTop: TPanel;
		PopupFile: TPopupMenu;
		sFiles: ThRangeBar;
		shpBorder: TShape;
		SplitterV: TSplitter;
		SplitterH: TSplitter;
		Timer: TTimer;
		miFileActions: TMenuItem;
		miSep2: TMenuItem;
		miFileRename: TMenuItem;
		miFileDelete: TMenuItem;
		miEnableMixer: TMenuItem;
		MenuItem1: TMenuItem;
		MenuItem2: TMenuItem;
		miAbout: TMenuItem;
		EmbeddedImage: TImage;
		bToggleEffects: TDecksButton;
		bToggleMixer: TDecksButton;
		miEnableEffects: TMenuItem;
		bToggleLeftPane: TDecksButton;
		PopupListHeader: TPopupMenu;
		bToggleGraphLines: TDecksButton;
		bToggleWaveDual: TDecksButton;
		lCPU: TBCLabel;
		bToggleTracklist: TDecksButton;
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
		procedure ListDirsMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
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
	private
		PlayedFilenames: TStringList;
		IsShiftDown: Boolean;

		procedure ResizeFrames;
		function  FindFileListEntry(const Filename: String): ThListItem;
		function  ReadImageFromTags(const Filename: String): Boolean;
		procedure SaveTrackList;
		procedure SetButtonDown(Button: TDecksButton; MenuItem: TMenuItem; Down: Boolean);
		procedure UpdateToggleButtons;
		procedure AlignEmbeddedImage;
	public
		FileListIsActive: Boolean;
		EQControls: array[1..2, TEQBand] of ThKnob;

		procedure ShowFocusRect;
		procedure UpdateMixerVisibility;
		procedure ApplyMixer(ApplyEQ: Boolean = True);
		procedure SetMasterTempo(BPM: Single);

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

	AudioManager: TAudioManager;
	DeckList: TFPGObjectList<TDeck>;
	MixerDeck: array[1..2] of TMixerDeck;
	MIDI: TMIDI;
	MasterBPM: Single;
	MasterDeck: TDeckFrame;
	MasterDeckIndex: Integer;


	SelectedFile, CurrentDir: String;

	procedure SetMaster(Index: Byte);
	procedure SetMasterBPM(BPM: Single);

	procedure Log(const S: String);
	procedure ErrorMessage(const MsgText: String);
	procedure LoadButtonImages(Form: TWinControl; Path: String);


implementation

{$R *.lfm}

uses
	Math, FileUtil, LazFileUtils, StrUtils,
	MouseWheelAccelerator, TextInputDialog,
	BCTypes, TeeGenericTree,
	Form.Tracklist,
	BASS, AudioTag, basetag, file_Wave, file_mp3, file_ogg,
	Decks.Song, Decks.Beatgraph;

type
	TFocusableControl = TNode<TControl>;

	TFocusableControls = class
		Root,
		CurrentItem: TFocusableControl;
		CurrentLevel: Integer;
		ActiveControl: TControl;
		FocusRectangle: TShape;
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

var
	TagScanner: TTagScannerJob;
	FocusableControls: TFocusableControls;

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
				Butt.Glyph := TBitmap.Create;
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
			Form.bMaster.Color := Form.bMaster.Tag;
		end
		else
		begin
			Form.bMaster.Color := Form.lTime.Color;
		end;
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
		if not Application.Terminated then
			Execute(Params.Action, Params.Pressed, Params.Value);
	finally
		Dispose(PExecuteParams(Data));
	end;
end;

procedure TMainForm.Execute(Action: TDecksAction; Pressed: Boolean = True; Value: Integer = 0);
const
	EQBandFrom: array[MIXER_EQ_LOW..MIXER_EQ_HIGH] of TEQBand =
		(EQ_BAND_LOW, EQ_BAND_MID, EQ_BAND_HIGH);
	KnobSnap = 6;
var
	Deck: TDeck;
	Form: TDeckFrame;
	DeckNum: Byte;
	S: String;
begin
	if Action.Kind = NO_ACTION then Exit;

	if MIDI.Debug then
	begin
		WriteStr(S, Action.Kind);
		Caption := S + Format('(%d)=%d', [Action.Param,Value]);
	end;

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

		if (FileListIsActive) and (Action.Kind in [UI_SELECT_TOGGLE, UI_SELECT_OPEN]) then
			Action.Kind := DECK_LOAD;
		if (Deck = nil) and (Action.Kind <> DECK_LOAD) then Exit;
	end;

	case Action.Kind of

	DECK_PLAY:			if Pressed then Form.bPlayClick(Self);
	DECK_CUE:			if (Deck.Paused) or (Deck.Cueing) then Form.Cue(Pressed)
							else if Pressed then Form.JumpToCue;
	DECK_SYNC:			if Pressed then Form.SetSynced(not Deck.Synced);
	DECK_REVERSE:		Deck.SetReverse(Pressed, True);
	DECK_LOAD:			if Pressed then LoadDeck(DeckNum);
	DECK_AMP:			Form.SliderAmp.Position := Trunc(((Value) / 128) * 200);
	DECK_BEND:			if Pressed then Deck.BendStart(Value > 0, False)
							else Deck.BendStop;
	DECK_SEEK:			Form.SetCue(Point(Max(0, Form.GraphCue.X + Value), 0));

	MIXER_CROSSFADER:	sFader.Position := Round((Value / 127) * 1000);
	MIXER_EQ_KILL:		if Pressed then Deck.ToggleEQKill(EQ_BAND_LOW);
	MIXER_EQ_LOW..
	MIXER_EQ_HIGH:		if DeckNum > 0 then
						begin
							Value := Value - 64;
							if InRange(Value, -KnobSnap, +KnobSnap) then
								Value := 0;
							//else if Value < 0 then Inc(Value, KnobSnap)
							//else if Value > 0 then Dec(Value, KnobSnap);
							EQControls[DeckNum, EQBandFrom[Action.Kind]].Position :=
								Trunc((Value / 128) * 3000);
						end;
	DECK_FX_FILTER:		if DeckNum > 0 then
							Form.SliderFxParam0.FloatPosition :=
								((Value - 64) / 127) * 2;

	UI_LIST:			if Pressed then SetActiveList(Value > 0);
	UI_SELECT:			if Pressed then ListBrowse(Value);
	UI_SELECT_TOGGLE:	if Pressed then ListDirs.Selected.Expanded := not ListDirs.Selected.Expanded;
	UI_SELECT_OPEN:		if Pressed then ListDirsChange(ListDirs, ListDirs.Selected);

	end;
end;

procedure TMainForm.UpdateController(Deck: TDeck; Event: Integer);
var
	Num: Byte;
	B: Boolean;
begin
	if Deck = nil then Exit;
	if Deck = MixerDeck[1].Deck then
		Num := 1
	else
	if Deck = MixerDeck[2].Deck then
		Num := 2
	else
		Exit;

	B := (Num = 2);

	case Event of

		MODE_PLAY_START:
		begin
			if Deck.Cueing then
				MIDI.SetLed(DECK_CUE, B)  // Cue on
			else
			begin
				MIDI.SetLed(DECK_PLAY, B); // Play on
				MIDI.SetLed(DECK_CUE, B, False); // Cue off
			end;
		end;

		MODE_PLAY_WAITSYNC:
			MIDI.SetLed(DECK_CUE, B);


		MODE_PLAY_STOP, MODE_PLAY_PAUSE, MODE_PLAY_FAILURE:
		begin
			if Deck.Cueing then
				MIDI.SetLed(DECK_CUE, B, False)  // Cue
			else
				MIDI.SetLed(DECK_PLAY, B, False); // Play
		end;

		MODE_EQ_KILL_ON, MODE_EQ_KILL_OFF:
		begin
			EQControls[Num, EQ_BAND_LOW].BorderColor :=
				IfThen(Event = MODE_EQ_KILL_ON, MixerPanel.Color, $00727578);
			MIDI.SetLed(MIXER_EQ_KILL, B, Event = MODE_EQ_KILL_ON);
		end;

		MODE_SYNC_ON, MODE_SYNC_OFF:
		begin
			MIDI.SetLed(DECK_SYNC, B, Deck.Synced);
		end;

	end;
end;

procedure TMainForm.ListBrowse(Dir: Integer);
var
	i: Integer;
	Node: TTreeNode;
begin
	if FileListIsActive then
	begin
		i := FileList.ItemIndex + Dir;
		if (i >= 0) and (i < FileList.Items.Count) then
		begin
			FileList.ItemIndex := i;
			FileList.ScrollToView(FileList.Items[i]);
		end;
	end
	else
	begin
		ListDirs.Tag := 1;
		if ListDirs.Selected = nil then
			ListDirs.Select(ListDirs.Items.GetFirstVisibleNode);
		Node := ListDirs.Selected;
		if Dir > 0 then
			ListDirs.Selected := ListDirs.Selected.GetNext
		else
			ListDirs.Selected := ListDirs.Selected.GetPrev;
		if ListDirs.Selected = nil then
			ListDirs.Selected := Node
		else
			ListDirs.Selected.MakeVisible;
	end;
end;

constructor TFocusableControls.Create;
begin
	inherited Create;
	Root := TFocusableControl.Create;
	CurrentItem := nil;
	CurrentLevel := 0;
	FocusColor := clRed;
	FocusRectangle := TShape.Create(MainForm.PanelTop);
	FocusRectangle.Shape := stRectangle;
	FocusRectangle.Brush.Style := bsClear; // bsFDiagonal;
	//FocusRectangle.Brush.Color := clMaroon;
	FocusRectangle.Pen.Style := psSolid;
	FocusRectangle.Pen.Color := FocusColor;
	FocusRectangle.Pen.Width := 2;
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
	MainForm.ShowFocusRect;
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
	MainForm.ShowFocusRect;
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
	MainForm.ShowFocusRect;
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
	MainForm.ShowFocusRect;
end;

procedure TMainForm.ShowFocusRect;
var
	Ctrl: TControl;
	WCtrl: TWinControl;
	Level: Integer;
begin
	with FocusableControls do
	begin
		if (CurrentItem <> nil) and (CurrentItem.Data <> nil) then
		begin
			Ctrl := CurrentItem.Data;
			Level := CurrentItem.Level;
			with FocusRectangle do
			begin
				if (CurrentLevel = Level) and (PrevCtrl[Level] = Ctrl) then Exit;

				if (Ctrl is TPanel) or (Ctrl is ThListView) or (Ctrl is ThShellTree) then
				begin
					WCtrl := TWinControl(Ctrl);
					Parent := WCtrl;
					Align := alClient;
{					if (PrevCtrl[Level] <> nil) and (PrevCtrl[Level] is TWinControl) then
						TWinControl(PrevCtrl[Level]).Color := PrevColor[Level];
					PrevColor[Level] := WCtrl.Color;
					WCtrl.Color := FocusColor;}
					PrevCtrl[Level] := Ctrl;
				end
				else
				begin
					Align := alNone;
					Parent := Ctrl.Parent;
					BoundsRect := Bounds(Ctrl.Left-1, Ctrl.Top-1, Ctrl.Width+2, Ctrl.Height+2);
					Anchors := Ctrl.Anchors;
				end;
				CurrentLevel := Level;
				Visible := True;
			end;
		end;
	end;
end;

// ================================================================================================
// TMainForm
// ================================================================================================

procedure TMainForm.FormCreate(Sender: TObject);
var
	i: Integer;
	{$IFDEF USEMIDI}
	S: String;
	mi: TMenuItem;
	{$ENDIF}

//	FC, CC: TFocusableControl;
//	Ctrl: TControl;
begin
	DefaultFormatSettings.DecimalSeparator := '.';
	Config.Load;

	CurrentDir := Config.Directory.Audio;
	if not DirectoryExists(CurrentDir) then
		CurrentDir := Config.AppPath;

	Caption := AppVersionString.Replace('/', '-', [rfReplaceAll]);
	SelectedFile := '';
	MasterDeck := nil;
	PlayedFilenames := TStringList.Create;
	PlayedFilenames.OwnsObjects := True;
	AudioManager := TAudioManager.Create;
	DeckList := TFPGObjectList<TDeck>.Create(True);
	sBPMChange(Self);

	Width  := Trunc(Screen.DesktopWidth  * 0.75);
	Height := Trunc(Screen.DesktopHeight * 0.75);
	DeckPanel.Height := Trunc(ClientHeight * 0.3);

	// ==========================================
	// Init list controls
	//
	with FileList do
	begin
		Columns.Clear;
		AddColumn('Filename', -54).AlwaysShow := True;
		AddColumn(' BPM', 50).AlwaysShow := True;
		AddColumn('Duration', 60);
		AddColumn('Bitrate', 46);
		AddColumn('Year', 42);
		AddColumn('Genre', 100);
		AddColumn('Artist', -16);
		AddColumn('Title', -16);
		AddColumn('Comment', -16);
		for i := 0 to Columns.Count-1 do
			Columns[i].Tag := i;
		Font.Color := COLOR_FILE_DEFAULT;
		PopupList := PopupFile;
		PopupHeader := PopupListHeader;
		OnClickItem := FileListClickItem;
	end;

	ListDirs.OnScroll := ListDirsScrolled;
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
	shpBorder.Brush.Style := bsSolid; // it's white otherwise?
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
		with Root.Add(PanelTop) do
		begin
			Add(bToggleLeftPane);
			Add(bToggleEffects);
			Add(bToggleMixer);
			Add(bToggleGraphLines);
			Add(bToggleTracklist);
//			Add(bToggleWaveDual);
			Add(sBPM);
		end;
		Root.Add(ListDirs); // Root.Add(LeftPanel);
		with Root.Add(DeckPanel) do;
		with Root.Add(MixerPanel) do begin
			Add(sEQ1L); Add(sEQ1M); Add(sEQ1H);
			Add(sFader);
			Add(sEQ2L); Add(sEQ2M); Add(sEQ2H);
		end;
		Root.Add(FileList);
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
	ListDirsScrolled(0, 0);
	{$IFNDEF MSWINDOWS}
	FileList.Invalidate;
	MixerPanel.Invalidate;
	{$ENDIF}
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if Key = VK_SHIFT then IsShiftDown := True;

	if MasterDeck = nil then
	begin
		case Key of

			VK_LEFT:
			begin
				if IsShiftDown then
					FocusableControls.Ascend
				else
					FocusableControls.SelectPrevious;
				Key := 0;
			end;

			VK_RIGHT:
			begin
				if IsShiftDown then
					FocusableControls.Descend
				else
					FocusableControls.SelectNext;
				Key := 0;
			end;
		end;
		Exit;
	end;

	case Key of

		VK_DELETE:
			CloseDeck(MasterDeck);

		VK_LEFT:
		begin
			if MasterDeckIndex > 1 then
				SetMaster(MasterDeckIndex-1);
			Key := 0;
		end;

		VK_RIGHT:
		begin
			SetMaster(MasterDeckIndex+1);
			Key := 0;
		end;

	else
		if MasterDeck.ProcessKeyDown(Key, Shift) then
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
	if Key = VK_SHIFT then IsShiftDown := False;

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
		FileList.Color := ColActive;
		ListDirs.BackgroundColor := ColInactive;
	end
	else
	begin
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
	if Button <> mbMiddle then Exit;

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
				TagReader := ReadTags(SelectedFile);
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

procedure TMainForm.FileListSelectItem(Sender: TObject; Button: TMouseButton; Shift: TShiftState; Item: ThListItem);
var
	B: Boolean = False;
begin
	if Item <> nil then
	begin
		SelectedFile := CurrentDir + Item.Caption;
		if Item.Tag > 0 then
		begin
			Application.ProcessMessages;
			B := ReadImageFromTags(SelectedFile);
		end;
		if B then
			AlignEmbeddedImage
		else
		begin
			EmbeddedImage.Height := 0;
			EmbeddedImage.Picture.Clear;
		end;
	end;
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
		FormTracklist.SongList.Items.Add(Item);
		FormTracklist.SongList.ItemIndex := FormTracklist.SongList.Items.Count-1;
		if FormTracklist.Visible then
			FormTracklist.SongList.Invalidate;
		FormTracklist.SongList.ScrollToView(Item);
	end;
end;

procedure TMainForm.OnFileScanEvent(EventKind: TTagScannerEventKind; const Filename: String; Tags: PSongTags);
var
	Item: ThListItem;
	S: String;
	I: Integer;
begin
	case EventKind of
		tsFileScanStarted: FileList.ItemIndex := -1;

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
			if FileList.SortColumn = 0 then FileList.SortItems;

		tsTagScanStarted: ;

		tsTagsRead:
		begin
			Item := FindFileListEntry(Filename);
			if Item = nil then Exit;

			Item.SubItems.BeginUpdate;
			Item.SubItems.Clear;
			for I := COLUMN_BPM to COLUMN_COMMENT do
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
			Item.SubItems[COLUMN_GENRE-1]   := Tags.Genre;
			Item.SubItems[COLUMN_ARTIST-1]  := Tags.Artist;
			Item.SubItems[COLUMN_TITLE-1]   := Tags.Title;
			Item.SubItems[COLUMN_COMMENT-1] := Tags.Comment;
			Item.Tag := IfThen(Tags.HasImage, 1, 0);

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

	Info := GetSongInfo(Filename);
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

		if Assigned(TagScanner) then TagScanner.Terminate;

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

procedure TMainForm.ListDirsMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
//var
//	Node: TTreeNode;
begin
 //	Node := ListDirs.GetNodeAt(X, Y);
 //	if Node <> nil then
 //		Node.Expanded := not Node.Expanded;
end;

procedure TMainForm.miLoadFileClick(Sender: TObject);
begin
	LoadDeck((Sender as TMenuItem).Tag);
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
	HadNone := DeckList.Count = 0;
	Deck := FindDeck(Index);
	if Deck = nil then
		Deck := CreateDeck;
	if Deck.Load(SelectedFile) then
	begin
		if (HadNone) and (Config.Deck.FirstSetsMasterBPM) then
			SetMasterTempo(Deck.OrigBPM);
	end;
end;

procedure TMainForm.SetMasterTempo(BPM: Single);
begin
	sBPM.Position := Trunc(BPM * 1000);
end;

procedure TMainForm.PopupFilePopup(Sender: TObject);
var
	Deck: TDeck;
begin
	miSep.Visible := (DeckList.Count > 0);
	for Deck in DeckList do
		Deck.UpdateMenuItem;
end;

procedure TMainForm.DeckPanelResize(Sender: TObject);
begin
	ResizeFrames;
end;

procedure TMainForm.FileListDblClick(Sender: TObject);
var
	I: Integer;
begin
	if SelectedFile.IsEmpty then
	begin
		I := FileList.ItemIndex;
		if I >= 0 then
			FileListSelectItem(Self, mbLeft, [], FileList.Items[I]);
	end;
	if not SelectedFile.IsEmpty then
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

procedure TMainForm.FileListEnter(Sender: TObject);
begin
	SetActiveList(FileList.Focused);
end;

procedure TMainForm.FileListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_RETURN then FileListDblClick(Self);
end;

procedure TMainForm.ResizeFrames;
var
	Deck: TDeck;
	Form: TDeckFrame;
	C, W: Integer;
begin
	if (DeckList = nil) or (DeckList.Count < 1) then Exit;
	C := DeckList.Count;
	W := DeckPanel.ClientWidth div C;
	C := 0;
	for Deck in DeckList do
	begin
		Form := TDeckFrame(Deck.Form);
		Form.Width := W;
		Form.Height := DeckPanel.ClientHeight;
		Form.Left := C;
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
begin
	for i := 0 to DeckList.Count-1 do
	begin
		Deck := DeckList[i];
		Deck.Index := i+1;
		Deck.Form.Name := Deck.Form.Name + Deck.Index.ToString;
		if i in [0,1] then
			MixerDeck[i+1].Deck := Deck;
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
	TDeckFrame(Result.Form).Init(Result);

	if DeckList.Count < 2 then
		SetMaster(1);

	DeckLayoutChanged;

	sFaderChange(Self);
	ResizeFrames;

	mi := TMenuItem.Create(PopupFile);
	mi.OnClick := miLoadFileClick;
	PopupFile.Items.Insert(PopupFile.Items.Count - 4, mi);
	Result.MenuItem := mi;
	Result.UpdateMenuItem;

	EndFormUpdate;
	Timer.Enabled := True;
end;

procedure TMainForm.CloseDeck(DeckFrame: TDeckFrame);
var
	I: Integer;
	Deck: TDeck;
begin
	SetMaster(0);
	DeckFrame.Timer.Enabled := False;
	Deck := DeckFrame.Deck;

	for I := 1 to 2 do
		if MixerDeck[I].Deck = Deck then
			MixerDeck[I].Deck := nil;

	I := DeckList.IndexOf(Deck);
	if I >= 0 then
		DeckList.Delete(I); // destroys the frame, too
	MasterDeck := nil;

	DeckLayoutChanged;

	ResizeFrames;
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
	Deck: TDeck;
	Col: TBGRAPixel;
begin
	Timer.Tag := Timer.Tag + 1;
	if Timer.Tag >= 20 then
	begin
		Timer.Tag := 0;
		lCPU.Caption := Format('%.1f', [BASS_GetCPU]) + '%';
	end;

	if not Config.Mixer.Enabled then Exit;

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

procedure TMainForm.sDirsChange(Sender: TObject);
begin
	ListDirs.ScrollY := Trunc(sDirs.Position);
end;

procedure TMainForm.miFileRenameClick(Sender: TObject);
var
	Path, Fn, Ext, NewName: String;
	ListItem: ThListItem;
begin
	if SelectedFile.IsEmpty then Exit;

	Path := ExtractFilePath(SelectedFile);
	Ext  := ExtractFileExt(SelectedFile);
	Fn   := ExtractFileNameWithoutExt(ExtractFileName(SelectedFile));
	NewName := Fn;

	if not IsShiftDown then
		NewName := ToTitleCase(NewName);

	if (not AskString('Rename File', NewName)) or (NewName = Fn) then Exit;
	NewName := NewName + Ext;

	ListItem := FindFileListEntry(Fn + Ext);
	Fn := IncludeTrailingPathDelimiter(Path) + NewName;

	if not RenameFile(SelectedFile, Fn) then
	begin
		ErrorMessage('Could not rename the file.');
		Exit;
	end;

	Path := GetBPMFile(ExtractFileName(SelectedFile));
	if FileExists(Path) then
		RenameFile(Path, GetBPMFile(NewName));

	if ListItem <> nil then
	begin
		ListItem.Caption := NewName;
		FileList.Invalidate;
	end;

	SelectedFile := Fn;
end;

procedure TMainForm.miFileDeleteClick(Sender: TObject);
var
	ListItem: ThListItem;
begin
	if SelectedFile.IsEmpty then Exit;

	if MessageDlg('Delete File', 'Are you sure?', mtConfirmation, mbYesNo, '') = mrYes then
	begin
		if not SysUtils.DeleteFile(SelectedFile) then
		begin
			ErrorMessage('Could not delete the file.');
			Exit;
		end;

		ListItem := FindFileListEntry(ExtractFileName(SelectedFile));

		SelectedFile := GetBPMFile(ExtractFileName(SelectedFile));
		if FileExists(SelectedFile) then
		begin
			if MessageDlg('Delete Metadata', 'Also delete the associated .BPM file?',
				mtConfirmation, mbYesNo, '') = mrYes then
					SysUtils.DeleteFile(SelectedFile);
		end;

		if ListItem <> nil then
		begin
			FileList.Items.Remove(ListItem);
			FileList.Invalidate;
		end;

		SelectedFile := '';
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
		H := 62; // !!! pnlEffects.Height;
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
	W, H: Integer;
	DR: TRect;
begin
	W := FileList.ClientWidth  div 2;
	H := FileList.ClientHeight div 2;
	X := Min(W, H);
	DR := Rect(W-X, H-X, W+X, H+X);
	FileList.Canvas.AntialiasingMode := amOn;
	FileList.Canvas.StretchDraw(DR, EmbeddedImage.Picture.Bitmap);
	FileList.Canvas.AntialiasingMode := amDontCare;
end;

procedure TMainForm.EmbeddedImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	FileList.Invalidate;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
	UpdateMixerVisibility;
	UpdateToggleButtons;
end;

procedure TMainForm.miEnableEffectsClick(Sender: TObject);
begin
	bToggleEffectsMouseDown(Self, mbLeft, [], 0, 0);
end;

procedure TMainForm.miEnableMixerClick(Sender: TObject);
begin
	bToggleMixerMouseDown(Self, mbLeft, [], 0, 0);
end;

end.
