unit Form.Main;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
	Types, ShellCtrls, ComCtrls, Menus, FGL, LMessages, LCLIntf, LCLType,
	BGRABitmap, BGRABitmapTypes, BCLabel, BGRAVirtualScreen,
	hListView, hShellTree, DecksButton, hSlider, hKnob,
	Decks.Config, Decks.Audio, Decks.MIDI, Decks.Deck, Frame.Deck;

const
	SupportedFormats = '.mp3 .ogg .wav .mod .sid .nsf';

	COLOR_FILE_DEFAULT = $AAAAAA;
	COLOR_FILE_HASBPM  = $DDEEFF;
	COLOR_FILE_PLAYED  = $5EB078;

type
	TMainForm = class(TForm)
		DecksButton1: TDecksButton;
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
		procedure FileListSelectItem(Sender: TObject; Item: ThListItem);
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
		procedure miEnableMixerClick(Sender: TObject);
	private
		PlayedFilenames: TStringList;
		IsShiftDown: Boolean;

		procedure ResizeFrames;
		function  FindFileListEntry(const Filename: String): ThListItem;
	public
		FileListIsActive: Boolean;
		EQControls: array[1..2, TEQBand] of ThKnob;

		procedure UpdateMixerVisibility;
		procedure ApplyMixer(ApplyEQ: Boolean = True);

		procedure SetActiveList(RightList: Boolean);
		procedure ListBrowse(Dir: Integer);
		procedure ListFilesInDir(const Dir: String);
		procedure UpdateFileInfos;
		procedure UpdateFileInfo(Filename: String);
		procedure MarkSongAsPlayed(Filename: String);

//		procedure MIDIEvent(var Msg: TLMessage); message WM_MIDI;
		procedure ASyncExecute(Data: PtrInt);
		procedure Execute(Action: TDecksAction; Pressed: Boolean = True; Value: Integer = 0);
		procedure UpdateController(Deck: TDeck; Event: Integer);

		function  CreateDeck: TDeck;
		procedure CloseDeck(DeckFrame: TDeckFrame);
		function  FindDeck(Index: Integer): TDeck;
		function  FindDeckForm(Index: Integer): TDeckFrame;
		procedure LoadDeck(Index: Integer = 0);

		procedure OnFileItemAdded(Sender: TObject);
		procedure OnFileTagsRead(Sender: TObject);
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
	procedure LoadButtonImages(Form: TWinControl; Path: String);


implementation

{$R *.lfm}

uses
	Math, FileUtil, StrUtils,
	AudioTag, basetag, file_Wave, file_mp3, file_ogg,
	Decks.TagScanner,
	Decks.Song, Decks.SongInfo, Decks.Beatgraph;

var
	TagScanner: TTagScannerJob;

// ================================================================================================
// Utility
// ================================================================================================

procedure Log(const S: String);
begin
	{$IFDEF DEBUG}{$IFNDEF WINDOWS}WriteLn(S);{$ENDIF}{$ENDIF}
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
	DECK_SYNC:			; // TODO
	DECK_REVERSE:		Deck.SetReverse(Pressed, True);
	DECK_LOAD:			if Pressed then LoadDeck(DeckNum);
	DECK_AMP:			Form.SliderAmp.Position := Trunc(((Value) / 128) * 200);
	DECK_BEND:			if Pressed then Deck.BendStart(Value > 0, False)
							else Deck.BendStop;
	DECK_SEEK:			Form.SetCue(Point(Max(0, Form.GraphCue.X + Value), 0));

	MIXER_CROSSFADER:	sFader.Position := Round((Value / 127) * 1000);
	MIXER_EQ_KILL:		if Pressed then Deck.ToggleEQKill(EQ_BAND_LOW);
	MIXER_EQ_LOW..
	MIXER_EQ_HIGH:		if DeckNum > 0 then EQControls[DeckNum,
							EQBandFrom[Action.Kind]].Position :=
							Trunc(((Value - 64) / 128) * 3000);

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

// ================================================================================================
// TMainForm
// ================================================================================================

procedure TMainForm.FormCreate(Sender: TObject);
var
	i: Integer;
	S: String;
	mi: TMenuItem;
begin
	DefaultFormatSettings.DecimalSeparator := '.';
	Config.Load;

	CurrentDir := Config.Directory.Audio;
	if not DirectoryExists(CurrentDir) then
		CurrentDir := Config.AppPath;

	Caption := AppVersionString;
	SelectedFile := '';
	MasterDeck := nil;
	PlayedFilenames := TStringList.Create;
	AudioManager := TAudioManager.Create;
	DeckList := TFPGObjectList<TDeck>.Create(True);
	sBPMChange(Self);

	// ==========================================
	// Init list controls
	//
	with FileList do
	begin
		Font.Color := COLOR_FILE_DEFAULT;
		Columns.Clear;
		AddColumn('Filename', -54);
		AddColumn(' BPM', 50);
		AddColumn('Duration', 60);
		AddColumn('Bitrate', 46);
		AddColumn('Artist', -23);
		AddColumn('Title', -23);
	end;

	ListDirs.OnScroll := ListDirsScrolled;
	ListDirs.HotTrackColor := FileList.ColorHover;
	ListDirs.Path := CurrentDir;

	// ==========================================
	// Misc GUI fixings
	//
	Application.HintColor := FileList.Color;
	Screen.HintFont.Color := FileList.Font.Color;
	Screen.HintFont.Name := FileList.Font.Name;
	Screen.HintFont.Size := FileList.Font.Size;
	Application.ShowHint := False;
	Application.ShowHint := True;
	{$IFDEF LCLGTK2} // fix buggy colors
	shpBorder.Brush.Style := bsSolid; // it's white otherwise?
	{$ENDIF}

	// ==========================================
	// Init MIDI and list devices
	//
	{$IFDEF USEMIDI}
	MIDI.Init;

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
	{$ENDIF}

	EQControls[1, EQ_BAND_LOW]  := sEQ1L;
	EQControls[1, EQ_BAND_MID]  := sEQ1M;
	EQControls[1, EQ_BAND_HIGH] := sEQ1H;
	EQControls[2, EQ_BAND_LOW]  := sEQ2L;
	EQControls[2, EQ_BAND_MID]  := sEQ2M;
	EQControls[2, EQ_BAND_HIGH] := sEQ2H;

	miEnableMixer.Checked := Config.Mixer.Enabled;
	UpdateMixerVisibility;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	FileList.Tag := 0;
	OnResize := nil;
	ListDirs.OnCollapsed := nil;
	CanClose := True;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	Config.Save;

	{$IFDEF USEMIDI}
	MIDI.Uninit;
	{$ENDIF}
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

	if MasterDeck = nil then Exit;

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

procedure TMainForm.FileListSelectItem(Sender: TObject; Item: ThListItem);
begin
	if Item <> nil then
		SelectedFile := CurrentDir + Item.Caption;
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

procedure TMainForm.MarkSongAsPlayed(Filename: String);
var
	Item: ThListItem;
begin
	if PlayedFilenames.IndexOf(Filename) >= 0 then Exit;

	PlayedFilenames.Add(Filename);
	Item := FindFileListEntry(ExtractFileName(Filename));
	if Item <> nil then
	begin
		Item.Color := COLOR_FILE_PLAYED;
		FileList.Invalidate;
		Application.ProcessMessages;
	end;
end;

procedure TMainForm.OnFileItemAdded(Sender: TObject);
var
	Filename: String;
	Item: ThListItem;
begin
	Filename := ExtractFileName(TagScanner.CurrentFilename);
	Item := FileList.AddItem(Filename);
	if PlayedFilenames.IndexOf(Filename) >= 0 then
		Item.Color := COLOR_FILE_PLAYED;
	FileList.Invalidate;
	Application.ProcessMessages;
end;

procedure TMainForm.OnFileTagsRead(Sender: TObject);
var
	Item: ThListItem;
	Info: TSongInfo;
	Filename, S: String;
begin
	Filename := ExtractFileName(TagScanner.CurrentFilename);
	Item := FindFileListEntry(Filename);
	if Item <> nil then
	begin
		Item.SubItems.Clear;

		Info := GetSongInfo(Filename);
		if Info.BPM > 1 then
		begin
			S := Format('%.2f', [Info.BPM]);
			if Item.Color = clNone then
				Item.Color := COLOR_FILE_HASBPM;
		end
		else
			S := '';
		Item.SubItems.Add(S);

		if TagScanner.CurrentTags <> nil then
			for S in TagScanner.CurrentTags do
				Item.SubItems.Add(S);

		FileList.Invalidate;
		Application.ProcessMessages;
	end;
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
var
	S, Fn: String;
	Files: TStringList;
	Item: ThListItem;
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
		TagScanner.OnFileAdded := OnFileItemAdded;
		TagScanner.OnTagsRead  := OnFileTagsRead;
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
begin
	Deck := FindDeck(Index);
	if Deck = nil then
		Deck := CreateDeck;
	Deck.Load(SelectedFile);
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
			FileListSelectItem(Self, FileList.Items[I]);
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
	MixerPanel.Visible := Config.Mixer.Enabled;
	MixerPanel.Enabled := MixerPanel.Visible;
end;

procedure TMainForm.ApplyMixer(ApplyEQ: Boolean = True);
begin
	if not Config.Mixer.Enabled then Exit;

	MixerDeck[1].Apply(ApplyEQ);
	MixerDeck[2].Apply(ApplyEQ);
end;

function TMainForm.CreateDeck: TDeck;
var
	mi: TMenuItem;
begin
	Result := TDeck.Create;

	Result.Form := TDeckFrame.Create(DeckPanel);
	Result.Form.Deck := Result;
	Result.Form.Parent := DeckPanel;
	TDeckFrame(Result.Form).Init(Result);
	Result.Form.Name := Result.Form.Name + Result.Index.ToString;

	if DeckList.Count < 2 then
		SetMaster(1);

	if Result.Index in [1, 2] then
		MixerDeck[Result.Index].Deck := Result;

	if MixerDeck[1].Deck <> nil then
		MixerDeck[1].Deck.OtherDeck := MixerDeck[2].Deck;
	if MixerDeck[2].Deck <> nil then
		MixerDeck[2].Deck.OtherDeck := MixerDeck[1].Deck;

	sFaderChange(Self);
	ResizeFrames;

	mi := TMenuItem.Create(PopupFile);
	mi.OnClick := miLoadFileClick;
	PopupFile.Items.Insert(PopupFile.Items.Count - 4, mi);
	Result.MenuItem := mi;
	Result.UpdateMenuItem;

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
	W, H: Integer;
	Deck: TDeck;
begin
	if not Config.Mixer.Enabled then Exit;

	W := pbBeats.ClientWidth div 2;
	H := pbBeats.ClientHeight - 1;

	Deck := MixerDeck[1].Deck;
	if (Deck <> nil) and (Deck.Loaded) then
		pbBeats.Bitmap.FillRect(Bounds(0, 0, W, H),
			Grays[Deck.BeatFadeCounter], dmSet);

	Deck := MixerDeck[2].Deck;
	if (Deck <> nil) and (Deck.Loaded) then
		pbBeats.Bitmap.FillRect(Bounds(W, 0, W, H),
			Grays[Deck.BeatFadeCounter], dmSet);

	pbBeats.Refresh;
end;

procedure TMainForm.sDirsChange(Sender: TObject);
begin
	ListDirs.ScrollY := Trunc(sDirs.Position);
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
	Item.SubItems[0] := S;

	FileList.Invalidate;
	Application.ProcessMessages;
end;

procedure ErrorMessage(const MsgText: String);
begin
	MessageDlg('Error', MsgText, mtError, [mbOK], '');
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
	begin
		NewName := Trim(NewName);
		NewName := NewName.Replace('_', ' ', [rfReplaceAll]);
		while NewName.Contains('  ') do
			NewName := NewName.Replace('  ', ' ', [rfReplaceAll]);
		NewName := AnsiProperCase(NewName, [' ', '-', '(', '[']);
	end;

	NewName := InputBox('Rename File', 'Type a new filename:', NewName);
	if NewName = Fn then Exit;
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
		if not DeleteFile(SelectedFile) then
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
					DeleteFile(SelectedFile);
		end;

		if ListItem <> nil then
		begin
			FileList.Items.Remove(ListItem);
			FileList.Invalidate;
		end;

		SelectedFile := '';
	end;
end;

procedure TMainForm.miEnableMixerClick(Sender: TObject);
begin
	Config.Mixer.Enabled := not Config.Mixer.Enabled;
	miEnableMixer.Checked := Config.Mixer.Enabled;
	UpdateMixerVisibility;
end;

end.
