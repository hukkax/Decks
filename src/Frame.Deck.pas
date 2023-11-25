unit Frame.Deck;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, StdCtrls,
	ExtCtrls, Buttons, LCLType, LCLIntf, LMessages, Menus, ComCtrls,
	IniFiles, BCTypes, FGL,
	BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
	Decks.Config, Decks.Audio, Decks.Deck, Decks.Beatgraph, Decks.SongInfo, Decks.Effects, Decks.MIDI,
	BASS, BASSmix, BASSloud,
	hKnob, hSlider, DecksButton, DecksValueLabel, DecksPanel;

const
	BPMStep = 0.01;

	WM_ZONE = LM_USER + 2010;

{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

type
	TDragInfoKind = ( DRAG_NONE, DRAG_WAVE, DRAG_BEAT );

	TDragInfo = record
		Dragging: TDragInfoKind;
		Offset:   Integer;
	end;

	TGUIEffect = class
		Effect: TBaseEffect;
		Button: TDecksButton;
		LabelWidth: Word;
		IsStored: Boolean;

		destructor Destroy; override;
	end;

	TGUIEffectParam = record
		Knob:       ThKnob;
		NameLabel:  TLabel;
		ValueLabel: TLabel;
	end;

	TBeatDrag = record
	const
		RectSize = 8;
	public
		Pos:  QWord;
		Rect: TRect;
		Zone: TZone;
	end;

	TEffectsList = TFPGObjectList<TGUIEffect>;

	TDeckFrame = class(TBaseDeckFrame)
		miAudioDevices: TMenuItem;
		miDeckClose: TMenuItem;
		N1: TMenuItem;
		PopupMenu: TPopupMenu;
		Timer: TTimer;
		PopupZone: TPopupMenu;
		miZoneKind0: TMenuItem;
		miZoneKind1: TMenuItem;
		miZoneKind2: TMenuItem;
		miZoneKind3: TMenuItem;
		miZoneKind4: TMenuItem;
		PopupEffectPresets: TPopupMenu;
		miSetMasterTempo: TMenuItem;
		miAudioSeparator: TMenuItem;
		pnlEffects: TDecksPanel;
		bLoopBeat1: TDecksButton;
		bLoopBeat2: TDecksButton;
		bLoopBar1: TDecksButton;
		bLoopBar2: TDecksButton;
		bLoopBar4: TDecksButton;
		bLoopSong: TDecksButton;
		bLoopZone: TDecksButton;
		SliderFxParam0: ThKnob;
		SliderFxParam1: ThKnob;
		SliderFxParam2: ThKnob;
		SliderFxParam3: ThKnob;
		SliderFxParam4: ThKnob;
		SliderFxParam5: ThKnob;
		bEffect0: TDecksButton;
		bEffect1: TDecksButton;
		bEffect2: TDecksButton;
		bEffect3: TDecksButton;
		bEffect4: TDecksButton;
		bEffect5: TDecksButton;
		bEffect6: TDecksButton;
		bEffect7: TDecksButton;
		bMaster: TDecksPanel;
		lTime: TLabel;
		bDeckMenu: TDecksButton;
		pnlGraph: TDecksPanel;
		pb: TBGRAVirtualScreen;
		pbZones: TBGRAVirtualScreen;
		pbRuler: TBGRAVirtualScreen;
		SliderGraphX: TDecksRangeBar;
		pnlControls: TDecksPanel;
		bPlay: TDecksButton;
		bBendUp: TDecksButton;
		bBendDown: TDecksButton;
		SliderAmp: ThKnob;
		bSync: TDecksButton;
		bReverse: TDecksButton;
		pbVU: TBGRAVirtualScreen;
		pbWave: TBGRAVirtualScreen;
		lBPM: TDecksValueLabel;
		bStart: TDecksButton;
		bStore: TDecksButton;
		bZoneAdd: TDecksButton;
		bZoneDel: TDecksButton;
		miShowFile: TMenuItem;
		bStoreFx: TDecksButton;
		miAudioSubDevices: TMenuItem;
		pnlWaveform: TDecksPanel;
		lTimeTotal: TLabel;
		lMeasure: TLabel;
		bLoopBeat0: TDecksButton;
		bPitchLock: TDecksButton;
		pnlPads: TDecksPanel;
		bPage1: TDecksButton;
		bPage2: TDecksButton;
		bPage3: TDecksButton;
		bPage4: TDecksButton;
		bPad1: TDecksButton;
		bPad2: TDecksButton;
		bPad3: TDecksButton;
		bPad5: TDecksButton;
		bPad6: TDecksButton;
		bPad7: TDecksButton;
		bPad4: TDecksButton;
		bPad8: TDecksButton;
		procedure bBendUpMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure bBendUpMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure bMasterMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure bReverseMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure bReverseMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure bStoreClick(Sender: TObject);
		procedure bStartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure bSyncMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure cmbDevicesChange(Sender: TObject);
		procedure cmbSubDevicesChange(Sender: TObject);
		procedure bPlayClick(Sender: TObject);
		procedure bPlayMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure bPlayMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure FormResize(Sender: TObject);
		procedure lTimeClick(Sender: TObject);
		procedure miDeckCloseClick(Sender: TObject);
		procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure pbMouseLeave(Sender: TObject);
		procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure pbMouseWheel(Sender: TObject; Shift: TShiftState;
			WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
		procedure pbRulerMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure pbRulerRedraw(Sender: TObject; Bitmap: TBGRABitmap);
		procedure pbWaveMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure pbWaveMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure pbWaveMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure pbWaveMouseWheel(Sender: TObject; Shift: TShiftState;
			WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
		procedure pbWaveRedraw(Sender: TObject; Bitmap: TBGRABitmap);
		procedure pbZonesDblClick(Sender: TObject);
		procedure pbZonesMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure pbZonesRedraw(Sender: TObject; Bitmap: TBGRABitmap);
		procedure SliderGraphXChange(Sender: TObject);
		procedure SliderTempoChange(Sender: TObject);
		procedure TimerTimer(Sender: TObject);
		procedure pbZonesMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
			MousePos: TPoint; var Handled: Boolean);
		procedure miZoneKind0Click(Sender: TObject);
		procedure SliderFxParam0Change(Sender: TObject);
		procedure bEffect0Click(Sender: TObject);
		procedure miApplyPresetClick(Sender: TObject);
		procedure bEffect0MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure SliderFxParam0MouseEnter(Sender: TObject);
		procedure SliderFxParam0MouseLeave(Sender: TObject);
		procedure miSetMasterTempoClick(Sender: TObject);
		procedure pbRulerDblClick(Sender: TObject);
		procedure lBPMChange(Sender: TObject);
		procedure bZoneAddButtonClick(Sender: TObject);
		procedure bZoneDelButtonClick(Sender: TObject);
		procedure pnlEffectsResize(Sender: TObject);
		procedure miShowFileClick(Sender: TObject);
		procedure bLoopBeat1SetDown(Sender: TObject);
		procedure bStoreFxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure bStoreFxSetDown(Sender: TObject);
		procedure bPitchLockSetDown(Sender: TObject);
	private
		DragWave: TDragInfo;
		GraphDragging: Boolean;
		SampleZoom: Integer;
		WaveformStep: Word;
		Zoner,
		Ruler: TBGRABitmap;
		ShowRemainingTime: Boolean;
		ZoneTextStyle: TTextStyle;
		BeatDrag: TBeatDrag;
		FlashTimer: Integer;
		Flash, NeedFlash: Boolean;

		procedure InitDevice(Dev: Integer);
		procedure InitSubDevice(SubDev: Byte);
		procedure ZoneChangedMessage(var Msg: TLMessage); message WM_ZONE;
		procedure ZoneChanged(Zone: Integer; FromCallback, MixTime: Boolean);
		procedure GetPlayPosition; inline;
		procedure SetSlider(var Slider: TDecksGaugeBar; Position: Integer); overload;
		procedure SetSlider(var Slider: ThKnob; Position: Integer); overload;
		procedure SetSlider(var Slider: TDecksValueLabel; Position: Double); overload;
		procedure ZoomSample(Dir: Integer);
		procedure ZoomGraph(Dir: Integer);
		procedure UpdatePlayButton(Kind: TAppMode);
		procedure AddGUIEffect(FxObject: TBaseEffect; BtnObject: TDecksButton; LblWidth: Word = 0);
		procedure InitGUIEffectParam(Index: Byte; AKnob: ThKnob);
		function  GetEffectKnob(Sender: TObject): ThKnob;
		procedure ResizeEffectButtons;
		procedure OnLoadProgress(Percentage: Integer);
		procedure ApplyEffects;
		procedure ShowEffectValue(Knob: ThKnob);
		procedure UnloopAll;
	public
		GraphHover,
		GraphCue:   TPoint;
		CuePos: QWord; // in graph sample coords
		CurrentZone: Word;
		CurrentDevice: Integer;
		PadsPage: Word;
		IsShiftDown,
		CanCreateNewZone: Boolean;

		Effects: TEffectsList;
		GUIEffectParams: array[0..5] of TGUIEffectParam;
		SelectedEffect: Byte;
		SelectedEffectParam: Byte;

		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

		function  GetLoopButton(Kind: TDecksActionKind; LoopLength: Integer): TDecksButton;

		procedure LoadDeckInfo(const Filename: String; Ini: TIniFile);
		procedure SaveDeckInfo(const Filename: String; Ini: TIniFile);

		procedure OnZoneChanged(Zone: Integer; MixTime: Boolean);
		procedure OnDeckEvent(Kind: TAppMode; Flag: Boolean);
		procedure OnGraphIteration;
		procedure ConfigChanged(const Item: String);

		procedure DoInit; override;
		procedure BeginFormUpdate;
		procedure EndFormUpdate;

		function  ProcessKeyUp(Key: Word; Shift: TShiftState): Boolean;
		function  ProcessKeyDown(Key: Word; Shift: TShiftState): Boolean;
		function  ProcessKeyPress(Key: Char): Boolean;

		procedure SetZoneKind(Zone: Word; Kind: TZoneKind);
		procedure SetSynced(B: Boolean);
		procedure Sync;

		procedure ShowPosition;
		procedure SyncToOtherDeck(Immediate: Boolean);
		procedure Cue(DoCue: Boolean);
		procedure SetCue(P: TPoint); overload;
		procedure SetCue(P: QWord); overload;
		procedure HotCue(Mode: TDecksActionKind; Index: Integer; Pressed: Boolean);
		procedure AfterPosJump(Data: PtrInt);
		procedure JumpToPos(Pos: QWord; Reset: Boolean = False; ProcessAfter: Boolean = True);
		procedure JumpToCue(FromCallback: Boolean = False);
		procedure JumpToZone(Zone: Word);
		procedure JumpToBar(Bar: Word);

		function  DoDrawWaveform(Pos: QWord; Bar: Cardinal; Brightness: Single; Palette: PBGRAPixel): QWord;
		procedure DrawWaveform;
		procedure DrawZones(Recalc: Boolean = True);
		procedure DrawRuler(Recalc: Boolean = True);
		procedure DrawGraph;
		procedure RedrawGraph(Recalc: Boolean = False; IgnoreGUIBPM: Boolean = False);
		procedure UpdateCaption;

		procedure ShowPanel_Effects;
		procedure SelectEffect(EffectNum: DWord);
		procedure SelectEffectParam(ParamNum: DWord);
		procedure EnableEffect(EffectIndex: Byte; Toggle: Boolean = False);
		procedure SetEffectParam(ParamNum: Integer; Value: Single);

		procedure SetKnob(const Knob: ThKnob; Value: Integer);
	end;

var
	DeckFrame: TDeckFrame;

implementation

{$R *.lfm}

uses
	Math, FileUtil, BCButton,
	MouseWheelAccelerator,
	Form.Main,
	Decks.Song;

{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

procedure Audio_Callback_Play(handle: HSYNC; channel, data: DWord; user: Pointer);
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
	DeckFrame: TDeckFrame;
	Deck: TDeck;
begin
	DeckFrame := TDeckFrame(user);
	if (DeckFrame = nil) then Exit;
	Deck := DeckFrame.Deck;
	if Deck <> nil then
	begin
		if Deck.Paused then
			Deck.Play
		else
			DeckFrame.JumpToCue(True);
		Deck.QueuedSync := 0;
	end;
end;

{ TDeckFrame }

procedure TDeckFrame.ConfigChanged(const Item: String);
begin
	if Item = 'cuepostfader' then
	begin
		BASS_Loudness_SetChannel(Deck.Loudness, Deck.GetCueStream);
	end;
end;

procedure TDeckFrame.UpdatePlayButton(Kind: TAppMode);
var
	B: Boolean = False;
begin
	case Kind of

		MODE_PLAY_START:
		begin
			B := Deck.Cueing;
			bPlay.StateNormal.Border.LightWidth := IfThen(B, 1, 2);
			bPlay.StateNormal.Border.LightColor := $0035D95F;
			if not B then
				MainForm.MarkSongAsPlayed(Deck.Filename);
			Log('MODE_PLAY_START');
		end;

		MODE_PLAY_STOP,
		MODE_PLAY_PAUSE:
		begin
			bPlay.StateNormal.Border.LightWidth := 0;
			bPlay.Down := False;
		end;

		MODE_PLAY_FAILURE:
		begin
			Log('MODE_PLAY_FAILURE');
			bPlay.Color := clRed;
			bPlay.Down := False;
			ErrorMessage('Failed to start playback!');
		end;

		else
			Exit;
	end;

	MainForm.UpdateController(Deck, Kind, B);
end;

procedure TDeckFrame.OnDeckEvent(Kind: TAppMode; Flag: Boolean);
var
	S: String;
	i: Integer;
	Fx: TGUIEffect;
begin
	{$IFDEF USEMIDI}
	if Deck <> nil then
		MIDI.SendMidiMessages(Kind, Flag, Deck.Index);
	{$ENDIF}

	case Kind of

		MODE_LOAD_START:
		begin
			Log('MODE_LOAD_START');
			BASS_SetDevice(CurrentDevice);
			CurrentZone := 0;
			UnloopAll;
			Deck.Graph.Zones.Clear;
			Deck.Graph.Scroll.X := 0;
			Deck.Graph.Zoom := 1;
			Deck.Info.LUFS := 0.0;
			Deck.Info.Amp := 1.0;
			Deck.Info.BPM := 0.0;
			Timer.Enabled := False;
			bMaster.Caption := Deck.Filename;
			Invalidate;
			Application.ProcessMessages;
			Enabled := False;
		end;

		MODE_LOAD_GRAPH:
		begin
			Log('MODE_LOAD_GRAPH:');
			Deck.Graph.Generate;
			Deck.Graph.BitmapSize := Point(pb.ClientWidth, pb.ClientHeight);
			Enabled := True;
		end;

		MODE_LOAD_SUCCESS:
		begin
			Log('MODE_LOAD_SUCCESS');
			if not Deck.GetInfo then
				Deck.Graph.Clear;
			if Deck.Graph.Zones.Count < 1 then
				Deck.Graph.AddZone(0, IfThen(Deck.Info.BPM >= 60, Deck.Info.BPM, MasterBPM), True);
			UpdateCaption;
			Deck.CalculateLUFS;
			SetSlider(lBPM, Deck.Graph.Zones.First.BPM);
			SliderAmp.GetExtraIndicator(0).Position := 200 - Trunc(Deck.Info.NormalizedAmp * 100);
			SetSlider(SliderAmp, Trunc(Deck.Info.Amp * 100));
			Deck.Graph.Amplify;
			Deck.Graph.ZonesLoaded;
			Deck.GetAvgBPM;
			for i := Low(Deck.HotCues) to High(Deck.HotCues) do
				HotCue(DECK_HOTCUE_CLEAR, i, True);
			SetCue(TPoint.Zero);
			SliderTempoChange(nil);
			JumpToCue;
			SliderTempoChange(nil); // fixme: need to call this twice
			ZoneChanged(0, False, False);
			MainForm.UpdateFileInfo(Deck);
			Deck.SaveInfoFile(Deck.Info.BPM);
		end;

		MODE_LOAD_FAILURE:
		begin
			Log('MODE_LOAD_FAILURE');
			case BASS_ErrorGetCode of
				BASS_ERROR_FILEOPEN:	S := 'Error opening file';
				BASS_ERROR_FILEFORM:	S := 'File format not recognised/supported';
				BASS_ERROR_NOTAUDIO:	S := 'Not an audio file';
				BASS_ERROR_CODEC:		S := 'Codec not available/supported';
				BASS_ERROR_FORMAT:		S := 'Sample format not supported';
				BASS_ERROR_MEM:			S := 'Insufficient memory';
				BASS_ERROR_UNKNOWN:		S := 'Unknown error';
				else S := '';
			end;
			Log(S);
			//ErrorMessage('Load error: ' + S);
			bMaster.Caption := S;
			bMaster.Background.Color := clMaroon;
		end;

		MODE_LOAD_FINISH:
		begin
			Log('MODE_LOAD_FINISH');
			RedrawGraph(True);
			SetCue(TPoint.Zero);
			DrawWaveform;
			SliderGraphX.Position := 0;
			lTime.Tag := -1;
			lTimeTotal.Caption := FormatTime(StreamLengthInSeconds(Deck.OrigStream));
			Invalidate;
			InitSubDevice(Config.Audio.SubDevice[Deck.Index]);
			Timer.Enabled := True;
			MainForm.ApplyMixer;
			ApplyEffects;
		end;

		MODE_PLAY_START, MODE_PLAY_STOP, MODE_PLAY_PAUSE, MODE_PLAY_FAILURE:
			UpdatePlayButton(Kind);

{		MODE_BEND_START:
			MainForm.Caption := Format('Bend %d', [Deck.PlayFreq]);

		MODE_BEND_STOP:
			MainForm.Caption := Format('Normal %d', [Deck.PlayFreq]);}

		MODE_TEMPOCHANGE:
		if Timer.Enabled then
		begin
//			MainForm.Caption := Format('Tempo %d', [Deck.PlayFreq]);
			if Effects <> nil then
				for Fx in Effects do
					if (Fx <> nil) and (Fx.Effect <> nil) then
					begin
						Fx.Effect.Deck := Deck;
						Fx.Effect.Apply;
					end;
		end;

		MODE_EQ_KILL,
		MODE_SYNC,
		MODE_FX_ENABLE,
		MODE_LOOP, MODE_LOOP_SONG, MODE_LOOP_ZONE:
			MainForm.UpdateController(Deck, Kind, Flag);

	end;
end;

procedure TDeckFrame.InitDevice(Dev: Integer);
var
	OK: Boolean;
	S: String;
	mi: TMenuItem;
begin
	Deck.Graph.BitmapSize := Point(pb.ClientWidth * Deck.Graph.WantedZoom, pb.ClientHeight);

//	Dev := cmbDevices.ItemIndex; // 0-based
	OK := AudioManager.InitDevice(MainForm.Handle, Dev);
	Inc(Dev); // to 1-based

	if not OK then
	begin
		S := '';
		case BASS_ErrorGetCode of
			BASS_ERROR_ALREADY: OK := True;
			BASS_ERROR_DX:		S := 'DirectX (or ALSA on Linux) is not installed.';
			BASS_ERROR_DEVICE:	S := 'Invalid device.';
			BASS_ERROR_DRIVER:	S := 'There is no available device driver.';
			BASS_ERROR_BUSY:	S := 'Device busy.';
			BASS_ERROR_FORMAT:	S := 'Format not supported by device.';
			BASS_ERROR_MEM:		S := 'Insufficient memory.';
			BASS_ERROR_UNKNOWN:	S := 'Unknown problem!';
		end;
		if S <> '' then
			ShowMessage('Init failed: ' + S);
	end;

	if OK then
	begin
		CurrentDevice := Dev;
		if Deck.Stream <> 0 then
			BASS_ChannelSetDevice(Deck.Stream, Dev);
		BASS_SetDevice(Dev);

		miAudioSubDevices.Clear;

		if Config.Mixer.CueMode = CUE_NONE then
		begin
			for Dev := 0 to High(SpeakerAssignmentInfo) do
			begin
				mi := TMenuItem.Create(miAudioSubDevices);
				mi.Caption := SpeakerAssignmentInfo[Dev].Caption;
				mi.GroupIndex := 1;
				mi.AutoCheck := True;
				mi.RadioItem := True;
				mi.Tag := Dev;
				mi.Checked := (mi.Tag = Config.Audio.SubDevice[Deck.Index]);
				mi.OnClick := cmbSubDevicesChange;
				miAudioSubDevices.Add(mi);
			end;

			InitSubDevice(Config.Audio.SubDevice[Deck.Index]);
		end
		else
			InitSubDevice(0);
	end;
end;


procedure TDeckFrame.InitSubDevice(SubDev: Byte);
var
	SF: DWord;
	CM: Byte;
begin
	if Deck.Stream = 0 then Exit;

	CM := Config.Mixer.CueMode;

	if CM = CUE_NONE then
	begin
		SF := SpeakerAssignmentInfo[Config.Audio.SubDevice[Deck.Index]].SpeakerFlags;
		BASS_Mixer_ChannelFlags(Deck.OrigStream, 0, SF);
		Config.Audio.SubDevice[Deck.Index] := SubDev;
		AudioManager.Devices[Config.Audio.Device[Deck.Index]].Speakers := SubDev;
	end;

	Deck.UpdateCueOutput;

	if CM = CUE_NONE then
	begin
		SF := SpeakerAssignmentInfo[SubDev].SpeakerFlags;
		BASS_Mixer_ChannelFlags(Deck.OrigStream, SF, SF);
	end;
end;

procedure TDeckFrame.SetSlider(var Slider: TDecksGaugeBar; Position: Integer);
begin
	Slider.OnChange := nil;
	Slider.Position := Position;
	Slider.OnChange := SliderTempoChange;
end;

procedure TDeckFrame.SetSlider(var Slider: ThKnob; Position: Integer);
begin
	Slider.OnChange := nil;
	Slider.Position := Position;
	Slider.OnChange := SliderTempoChange;
end;

procedure TDeckFrame.SetSlider(var Slider: TDecksValueLabel; Position: Double);
begin
	Slider.OnChange := nil;
	Slider.Value := Position;
	Slider.OnChange := lBPMChange;
end;

function TDeckFrame.GetLoopButton(Kind: TDecksActionKind; LoopLength: Integer): TDecksButton;
begin
	Result := nil;
	case Kind of
		DECK_LOOP_SONG: Result := bLoopSong;
		DECK_LOOP_ZONE: Result := bLoopZone;
		DECK_LOOP:
		case LoopLength of
			0:      Result := bLoopBeat0;
			1:      Result := bLoopBeat1;
			2..3:   Result := bLoopBeat2;
			4..7:   Result := bLoopBar1;
			8..15:  Result := bLoopBar2;
			16..31: Result := bLoopBar4;
		end;
	end;
end;

procedure TDeckFrame.ZoneChangedMessage(var Msg: TLMessage);
begin
	ZoneChanged(Msg.lParam, False, False);
end;

procedure TDeckFrame.OnZoneChanged(Zone: Integer; MixTime: Boolean);
begin
	//PostMessage(Self.Handle, WM_ZONE, Zone, Zone);
	ZoneChanged(Zone, True, MixTime);
end;

procedure TDeckFrame.ZoneChanged(Zone: Integer; FromCallback, MixTime: Boolean);
var
	Z: TZone;
	DoLoopZone: Boolean;
begin
	{$IFDEF DEBUG}
	Log(Format('ZoneChanged %d  CB=%s  MT=%s', [Zone, FromCallback.ToString, MixTime.ToString]));
	{$ENDIF}

	if Zone = ZONECHANGE_STOPPED then
	begin
		Deck.Stop;
		//SetCue(TPoint.Zero); // crashes on Qt5
		GraphCue := TPoint.Zero;
		CuePos := Deck.Graph.GraphToPos(GraphCue, True);
		SliderGraphX.Position := 0;
		Exit;
	end;

	if Deck.Graph.Zones.Count < 1 then Exit;

	if Zone = ZONECHANGE_GETPOS then
	begin
		GetPlayPosition;
		Zone := Deck.Graph.GetZoneIndexAt(Deck.Graph.SongToGraphBytes(Deck.PlayPosition));
		Deck.PlayingZone := Zone;
	end;

	Z := Deck.Graph.Zones[Zone];

	if FromCallback then
	begin
		//if Zone = Deck.PlayingZone then Exit;
		Deck.PlayingZone := Zone;
		if MixTime then
		begin
			DoLoopZone := (Z.Kind = zkLoop) or
				((Deck.LoopInfo_Zone.Enabled) and (Deck.LoopInfo_Zone.Zone >= 0));
			if not DoLoopZone then
				Deck.UnloopZone;
			case Z.Kind of
				// no special processing
				zkNormal: ;
				// jump to bar # defined in Data
				zkJump:	JumpToPos(Z.Data);
				// jump straight to next zone
				zkSkip:	JumpToZone(Zone + 1);
				// marks the end of song
				zkEnd:	ZoneChanged(ZONECHANGE_STOPPED, False, True);
				// loop current zone indefinitely
				zkLoop:	;
			end;
			if DoLoopZone then
				Deck.LoopZone(Zone);
			Deck.SetBPM(MasterBPM);
		end
		else
			PostMessage(Self.Handle, WM_ZONE, Zone, Zone);
	end
	else
	if not MixTime then
	begin
		if CurrentZone = Zone then
		begin
			//SetSlider(SliderTempo, Trunc(Z.BPM));
			//SetSlider(SliderTempoFrac, Trunc(Frac(Z.BPM) * 1000));
			SetSlider(lBPM, Z.BPM);
		end;
		Deck.SetBPM(MasterBPM);
		DrawZones(True);
	end;
end;

procedure TDeckFrame.DoInit;
var
	Dev: TAudioDevice;
	mi: TMenuItem;
	i: Integer;
begin
	for i := Low(Deck.HotCues) to High(Deck.HotCues) do
		Deck.HotCues[i] := Default(THotCue);

	for i := Low(Deck.Cue) to High(Deck.Cue) do
		Deck.Cue[i] := 0;

	ShowRemainingTime := Config.Deck.ShowRemainingTime;

	Deck.Graph.WantedZoom := 1;
	SampleZoom := 1;

	Deck.OnLoadInfo := LoadDeckInfo;
	Deck.OnSaveInfo := SaveDeckInfo;

	Deck.Graph.OnGraphIteration := OnGraphIteration;
	Deck.Graph.OnZoneChange := OnZoneChanged;
	Deck.Graph.OnLoadProgress := OnLoadProgress;
	Deck.OnModeChange := OnDeckEvent;
	Deck.Index := DeckList.Add(Deck) + 1;

	lTime.Tag := -1;
	bMaster.Tag := bMaster.Background.Color;
	bMaster.Background.Color := lTime.Color;
	GraphHover := Point(-1, -1);

	pnlWaveform.Height := Max(40, Config.Deck.Waveform.Height);

	if Config.Mixer.CueMode = CUE_NONE then
	begin
		for Dev in AudioManager.Devices do
		begin
			mi := TMenuItem.Create(miAudioDevices);
			mi.Caption := Dev.Name;
			mi.Tag := Dev.Index - 1;
			mi.GroupIndex := 1;
			mi.AutoCheck := True;
			mi.RadioItem := True;
			mi.Checked := (mi.Tag = Config.Audio.Device[Deck.Index]);
			mi.OnClick := cmbDevicesChange;
			miAudioDevices.Add(mi);
		end;

		InitDevice(Config.Audio.Device[Deck.Index]);
	end
	else
	begin
		miAudioDevices.Visible := False;
		miAudioSubDevices.Visible := False;
		miAudioSeparator.Visible := False;

		InitDevice(Config.Audio.CueDevice);
	end;

	Show;
	ShowPosition;

	//SelectEffect(6); // select and enable the Filter effect by default (ugly)
	//bEffect0MouseDown(bEffect6, mbRight, [], 0, 0);

	Timer.Enabled := True;
end;

procedure TDeckFrame.SliderTempoChange(Sender: TObject);
begin
	// fix choppiness of slider movement
	if Assigned(Sender) then (Sender as TControl).Repaint;

	//SliderTempo.OnChange := nil;
	Deck.Graph.ZonesChanged;
	RedrawGraph(True);
	DrawWaveform;
	Deck.SetBPM(MasterBPM);
	//SliderTempo.OnChange := SliderTempoChange;
end;

procedure TDeckFrame.GetPlayPosition;
begin
	Deck.PlayPosition := Deck.GetPlayPosition;
end;

// update vu meters and various position indicators
//
procedure TDeckFrame.ShowPosition;
var
	time_e, time_r, tm, ts, Y, W, H: Integer;
	se, sr: String;
	Vol: DWord = 0;
	lof, hif, VolF: Single;
begin
	if not Deck.Loaded then
	begin
		{if lTime.Tag >= 0 then Exit;
		time_e := 0;}
		Exit;
	end
	else
		if Deck.Graph.Zones.Count < 1 then Exit;

	GetPlayPosition;

	if Deck.Paused then
		bPlay.StateNormal.Border.Color := Grays[0]
	else
	begin
		bPlay.StateNormal.Border.Color := Grays[Deck.BeatFadeCounter];

		// hack to jump into loop if we missed the loop endpoint while setting it up
		if (Deck.LoopInfo_Misc.Enabled) and (Deck.PlayPosition >= Deck.LoopInfo_Misc.EndPos) then
		begin
			Deck.PlayPosition := (Deck.PlayPosition - Deck.LoopInfo_Misc.EndPos) mod
				(Deck.LoopInfo_Misc.EndPos - Deck.LoopInfo_Misc.StartPos);
			Deck.PlayPosition := Deck.PlayPosition + Deck.LoopInfo_Misc.StartPos;
			JumpToPos(Deck.PlayPosition, True, False);
		end;
	end;

	time_e := Max(0, Round(BASS_ChannelBytes2Seconds(Deck.OrigStream, Deck.PlayPosition)));

	Inc(FlashTimer);
	if FlashTimer > Config.Deck.WarnSpeed then
	begin
		FlashTimer := 0;
		Flash := not Flash;
		if (Flash) and (NeedFlash) then
			lTime.Color := clRed
		else
			lTime.Color := bDeckMenu.StateNormal.Background.Color;
	end;

	if time_e <> lTime.Tag then
	begin
		time_r := Round(BASS_ChannelBytes2Seconds(Deck.OrigStream, Deck.ByteLength - Deck.PlayPosition));

		tm := time_e div 60;
		ts := time_e - (tm * 60);
		se := Format('%d:%.2d',  [tm, ts]);  // elapsed
		tm := time_r div 60;
		ts := time_r - (tm * 60);
		sr := Format('-%d:%.2d', [tm, ts]);  // remaining
		NeedFlash := (time_r <= Config.Deck.WarnTime);

		if ShowRemainingTime then
		begin
			lTime.Caption := sr;
			lTime.Hint := se;
		end
		else
		begin
			lTime.Caption := se;
			lTime.Hint := sr;
		end;

		lTime.Tag := time_e;
	end;

	with pbVU do
	begin
		W := ClientWidth; H := ClientHeight;
		Bitmap.FillRect(Bounds(0, 0, W, H), BGRABlack, dmSet);

		if not Deck.Paused then
		begin
			if BASS_Loudness_GetLevel(Deck.Loudness, MakeLong(BASS_LOUDNESS_CURRENT, 50), VolF{%H-}) then
			begin
				hif := Abs(Config.Audio.TargetLUFS) - 4;
				lof := -35.0;

				if (VolF.IsInfinity) or (VolF < lof) then
					VolF := lof;
				//VolF := log10(VolF) * 20; // translate it to dB
				//bmaster.caption := VolF.ToString;
				VolF := VolF + Abs(lof); // -35..-14 -> 0..21

				VolF := VolF / (Abs(lof) - hif);
				VolF := VolF * Deck.Info.Amp;

				if VolF > 0.0 then
				begin
					// vertical
					Y := H - Min(Trunc(H * VolF), H);
					Bitmap.FillRect(Bounds(0, Y, W, H), BGRAWhite, dmSet);
				end;
				MIDI.SetVUMeter(Deck.Index, VolF);
			end
			else
			begin
				Vol := BASS_ChannelGetLevel(Deck.Stream);
				if Vol > 0 then
				begin
					// vertical
					W := W div 2;
					Y := H - Trunc((H * ((Vol and $FFFF) / 32767)));
					Bitmap.FillRect(Bounds(0, Y, W, H), BGRAWhite, dmSet);

					Y := H - Trunc((H * ((Vol shr 16) / 32767)));
					Bitmap.FillRect(Bounds(W, Y, W+2, H), BGRAWhite, dmSet);

					// horizontal
					{H := H div 2;
					X := Trunc((W * ((Vol and $FFFF) / 32767)));
					Bitmap.FillRect(Bounds(0, 0, X, H), BGRAWhite, dmSet);

					X := Trunc((W * ((Vol shr 16) / 32767)));
					Bitmap.FillRect(Bounds(0, H, X, H+H), BGRAWhite, dmSet);}
				end;
				MIDI.SetVUMeter(Deck.Index, (Vol and $FFFF) / 32768);
			end;

		end;

		Repaint;
	end;

	DrawGraph;
end;

procedure TDeckFrame.TimerTimer(Sender: TObject);
begin
	if not Enabled then Exit;

	if Deck.Graph.QueueDraw then
		Deck.Graph.Draw(pb.ClientWidth * Deck.Graph.WantedZoom, pb.ClientHeight);

	if Deck.PitchBend.Active then
		Deck.BendUpdate;

	Deck.BeatFadeCounter := Max(0, Deck.BeatFadeCounter - 40);

	ShowPosition;
end;

procedure TDeckFrame.bBendUpMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	Deck.BendStart((Sender = bBendUp), (Button <> mbLeft));
end;

procedure TDeckFrame.bBendUpMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	Deck.BendStop;
end;

procedure TDeckFrame.bMasterMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	SetMaster(Deck.Index);
end;

procedure TDeckFrame.bReverseMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if not Deck.Paused then
		Deck.SetReverse(True, Deck.Synced or (Button = mbLeft));
end;

procedure TDeckFrame.bReverseMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
var
	B: Boolean;
begin
	B := Button = mbLeft;
	Deck.SetReverse(False, B);
	if (B) or (Deck.Synced) then
		if Deck.IsOtherDeckPlaying then
			SyncToOtherDeck(True);
end;

procedure TDeckFrame.LoadDeckInfo(const Filename: String; Ini: TIniFile);
var
	Effect: TGUIEffect;
	Param: TEffectParam;
	B: Boolean;
	Sect: String;
begin
	for Effect in Effects do
	begin
		Sect := LowerCase('fx.' + Effect.Effect.Name);
		B := Ini.SectionExists(Sect);
		Effect.IsStored := B;
		if B then
		begin
			Effect.Effect.Enabled := Ini.ReadBool(Sect, 'Enabled', Effect.Effect.Enabled);
			for Param in Effect.Effect.Params do
				Param.Value := Ini.ReadFloat(Sect, Param.Name, Param.Value);
		end;
		EnableEffect(Effects.IndexOf(Effect));
	end;
end;

procedure TDeckFrame.SaveDeckInfo(const Filename: String; Ini: TIniFile);
var
	Effect: TGUIEffect;
	Param: TEffectParam;
	Sect: String;
begin
	for Effect in Effects do
	begin
		Sect := LowerCase('fx.' + Effect.Effect.Name);
		if not Effect.IsStored then
			Ini.EraseSection(Sect)
		else
		begin
			Ini.WriteBool(Sect, 'Enabled', Effect.Effect.Enabled);
			for Param in Effect.Effect.Params do
				Ini.WriteFloat(Sect, Param.Name, Param.Value)
		end;
	end;
end;

procedure TDeckFrame.bStoreClick(Sender: TObject);
begin
	Deck.SaveInfoFile(Deck.GetAvgBPM);
	Deck.Info.BPM := Deck.AvgBPM;
	MainForm.UpdateFileInfo(Deck.Filename);
end;

procedure TDeckFrame.bStartMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	case Button of
		// Move cue to song start
		mbLeft:   begin SetCue(TPoint.Zero); Exit; end;
		// Set song start position to cue
		mbRight:  Deck.Graph.StartPos := Deck.Graph.GraphToSongBytes(CuePos);
		// Reset song start position
		mbMiddle: Deck.Graph.StartPos := 0;
	end;

	Deck.Graph.ZonesLoaded;
	RedrawGraph;
end;

procedure TDeckFrame.ZoomSample(Dir: Integer);
begin
	if Dir = 0 then
		SampleZoom := 2
	else
	if Dir > 0 then
		SampleZoom := Min(3, SampleZoom+1)
	else
	if Dir < 0 then
		SampleZoom := Max(1, SampleZoom-1);

	DrawWaveform;
end;

procedure TDeckFrame.ZoomGraph(Dir: Integer);
var
	P: QWord;
begin
	P := CuePos;

	if Dir = 0 then
		Deck.Graph.WantedZoom := 1
	else
	if Dir > 0 then
		Deck.Graph.WantedZoom := Min(10, Deck.Graph.WantedZoom+1)
	else
	if Dir < 0 then
		Deck.Graph.WantedZoom := Max(1,  Deck.Graph.WantedZoom-1);

	SliderGraphX.Position := 0;
	Deck.Graph.ZoomChanged := True;
	RedrawGraph;

	if Dir <> 0 then
		SetCue(P);
end;

procedure TDeckFrame.cmbDevicesChange(Sender: TObject);
var
	i: Integer;
begin
	if Sender is TMenuItem then
	begin
		i := (Sender as TMenuItem).Tag;
		Config.Audio.Device[Deck.Index] := i;
		InitDevice(i);
	end;
end;

procedure TDeckFrame.cmbSubDevicesChange(Sender: TObject);
var
	i: Integer;
begin
	if Sender is TMenuItem then
	begin
		i := (Sender as TMenuItem).Tag;
		InitSubDevice(i);
	end;
end;

procedure TDeckFrame.SetSynced(B: Boolean);
begin
	Deck.Synced := B;
	bSync.StateNormal.Border.LightWidth := IfThen(B, 1, 0);
	MainForm.UpdateController(Deck, MODE_SYNC, B);
end;

procedure TDeckFrame.Sync;
begin
	SyncToOtherDeck(True);
end;

procedure TDeckFrame.bSyncMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	case Button of
		mbLeft:
			SetSynced(not Deck.Synced);
		mbRight:
			Sync;
	end;
end;

procedure TDeckFrame.bPlayClick(Sender: TObject);
var
	i: Integer;
	B: Boolean;
begin
	if not Enabled then Exit;

	B := Deck.Cueing;

	for i := Low(Deck.HotCues) to High(Deck.HotCues) do
		if Deck.HotCues[i].Temporary then
		begin
			Deck.HotCues[i].Temporary := False;
			B := True;
		end;

	if B then
	begin
		Deck.Cueing := False;
		UpdatePlayButton(MODE_PLAY_START);
	end
	else
	begin
		BASS_SetDevice(CurrentDevice);

		// jump to cue if song has been played through
		GetPlayPosition;
		if Deck.PlayPosition >= Deck.ByteLength then
			JumpToCue;

		Deck.SetBPM(MasterBPM);
		if (Deck.Synced) and (Deck.Paused) then
			SyncToOtherDeck(False)
		else
			Deck.Pause;
	end;
end;

procedure TDeckFrame.bPlayMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if not Enabled then Exit;

	case Button of
		mbRight:
			Cue(True);
		mbMiddle:
			if (Deck.OtherDeck <> nil) and (not Deck.OtherDeck.Paused) then
				SyncToOtherDeck(False)
			else
				Deck.Pause;
	end;
end;

procedure TDeckFrame.bPlayMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if not Enabled then Exit;

	case Button of
		mbRight:  Cue(False);
	end;
end;

procedure TDeckFrame.SyncToOtherDeck(Immediate: Boolean);
var
	OtherDeck: TDeck;

	function GetOtherDeckPos: TPoint; inline;
	begin
		Result := OtherDeck.Graph.PosToGraph(OtherDeck.GetPlayPosition(False), False);
	end;

var
	PT, CT: TPoint;
	P: QWord;
	B: Cardinal;
begin
	if not Enabled then Exit;

	OtherDeck := Deck.GetOtherOrCurrentDeck(Immediate);
	if OtherDeck = nil then Exit;

	if (OtherDeck.Paused) and (Deck.Paused) then
	begin
		Deck.Play;
		Exit;
	end;

	// detect if play was pressed during the first beat of a bar, if so,
	// start synced playback immediately instead of waiting for the next bar to begin
	// TODO: configuration
	if (not Immediate) and (not OtherDeck.Paused) then
	begin
		PT := {%H-}GetOtherDeckPos;
		if PT.Y <= OtherDeck.Graph.Height div 4 then
			Immediate := True;
	end;

	if Immediate then
	begin
		CT := Deck.Graph.PosToGraph(Deck.GetPlayPosition(True), False);
		BASS_SetDevice(CurrentDevice);
		PT := GetOtherDeckPos;
		CT.Y := PT.Y;
		P := Deck.Graph.GraphToPos(CT);

		BASS_Mixer_ChannelSetPosition(Deck.OrigStream, P,
			BASS_POS_BYTE or BASS_POS_MIXER_RESET);
		if Deck.Paused then Deck.Play;

		ZoneChanged(ZONECHANGE_GETPOS, False, False);
		ShowPosition;
	end
	else
	begin
		if (OtherDeck.Paused) then
			OtherDeck := Self.Deck;

		if Deck.QueuedSync = 0 then
		begin
			if OtherDeck.LoopInfo_Misc.Enabled then
				P := OtherDeck.LoopInfo_Misc.StartPos
			else
			begin
				B := Min(OtherDeck.GetCurrentBar+1, High(OtherDeck.Graph.Bars));
				P := OtherDeck.Graph.Bars[B].Pos;
				P := OtherDeck.Graph.GraphToSongBytes(P);
			end;
			if Deck.Paused then JumpToCue;

			bPlay.StateNormal.Border.LightColor := $0022AAFF;
			bPlay.StateNormal.Border.LightWidth := 2;
			MainForm.UpdateController(Deck, MODE_PLAY_WAITSYNC, True);

			Deck.QueuedSync := BASS_ChannelSetSync(OtherDeck.OrigStream,
				BASS_SYNC_POS or BASS_SYNC_MIXTIME or BASS_SYNC_ONETIME, P,
				@Audio_Callback_Play, Self);
		end
		else
		begin
			BASS_ChannelRemoveSync(OtherDeck.OrigStream, Deck.QueuedSync);
			Deck.QueuedSync := 0;
			bPlay.StateNormal.Border.LightWidth := 0;
			if Deck.Paused then
				MainForm.UpdateController(Deck, MODE_PLAY_PAUSE, True)
			else
				MainForm.UpdateController(Deck, MODE_PLAY_START, True);
		end;
	end;
end;

procedure TDeckFrame.Cue(DoCue: Boolean);
begin
	if not Enabled then Exit;

	if DoCue then
	begin
		if (not Deck.Cueing) and (Deck.Paused) then
		begin
			JumpToCue;
			BASS_SetDevice(CurrentDevice);
			Deck.Cueing := True;
			Deck.Play;
		end;
	end
	else
	begin
		if Deck.Cueing then
		begin
			BASS_SetDevice(CurrentDevice);
			Deck.Stop;
			Deck.Cueing := False;
			JumpToCue;
		end;
	end;
end;

procedure TDeckFrame.FormResize(Sender: TObject);
begin
	Invalidate;
	if not Enabled then Exit;
	RedrawGraph(True);
	DrawWaveform;
end;

procedure TDeckFrame.lTimeClick(Sender: TObject);
begin
	if not Enabled then Exit;

	Config.Deck.ShowRemainingTime := not Config.Deck.ShowRemainingTime;
	ShowRemainingTime := Config.Deck.ShowRemainingTime;

	lTime.Tag := 0;
	ShowPosition;
end;

procedure TDeckFrame.miDeckCloseClick(Sender: TObject);
begin
	MainForm.CloseDeck(Self);
end;

(*
procedure TDeckFrame.cmbDevicesDrawItem(Control: TWinControl; Index: Integer;
	ARect: TRect; State: TOwnerDrawState);
var
	CB: TComboBox;
	R: TRect;
	S: String;
begin
	CB := Control as TComboBox;
	R := ARect;

	CB.Canvas.FillRect(R);
	//ImageList1.Draw(LBox.Canvas, R.Left, R.Top, Index);
	S := CB.Items[Index];
	//TextHeight := CB.Canvas.TextHeight(S);
	//TextLeftPos := R.Left + ImageList1.Width + IMAGE_TEXT_SPACE;
	//TextTopPos  := R.Top + R.Height div 2 - TextHeight div 2;
	CB.Canvas.TextOut(R.Left,
		R.Top + ((R.Height - CB.Canvas.TextHeight(S)) div 2),
		S);
end;
*)

procedure TDeckFrame.InitGUIEffectParam(Index: Byte; AKnob: ThKnob);
const
	W = 15;
var
	Lbl: TLabel;
begin
//	AKnob.Height := AKnob.Height - 3;

	Lbl := TLabel.Create(AKnob.Parent);
	Lbl.AutoSize := False;
	Lbl.Transparent := True;
	Lbl.SetBounds(AKnob.Left-W, 1, AKnob.Width-1+(W*2), 24);
	Lbl.Alignment := taCenter;
	Lbl.Font.Color := clWhite;
	Lbl.Parent := AKnob.Parent;

	with GUIEffectParams[Index] do
	begin
		Knob := AKnob;
		Knob.Sensitivity := 200;
		ValueLabel := Lbl;
		NameLabel  := Lbl;
		Knob.PositionLabel := Lbl;
		Knob.OnMouseEnter := SliderFxParam0MouseEnter;
		Knob.OnMouseLeave := SliderFxParam0MouseLeave;
	end;
end;

procedure TDeckFrame.AddGUIEffect(FxObject: TBaseEffect; BtnObject: TDecksButton; LblWidth: Word = 0);
var
	Fx: TGUIEffect;
begin
	if FxObject = nil then Exit;

	FxObject.Deck := Deck;
	Fx := TGUIEffect.Create;
	Fx.Effect := FxObject;
	Fx.Button := BtnObject;
	if LblWidth = 0 then LblWidth := 70;
	Fx.LabelWidth := LblWidth;
	BtnObject.Tag := Effects.Count;
	BtnObject.OnClick := nil;
	BtnObject.OnButtonClick := bEffect0Click;
	BtnObject.OnMouseDown := bEffect0MouseDown;
	BtnObject.DropDownStyle := bdsCommon;
	Fx.Button.Caption := FxObject.Name;
	Effects.Add(Fx);
end;

destructor TGUIEffect.Destroy;
begin
	if Effect <> nil then
		Effect.Free;
	inherited Destroy;
end;

constructor TDeckFrame.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	CurrentZone := 0;
	PadsPage := 1;

	with ZoneTextStyle do
	begin
		Alignment := taLeftJustify;
		Layout := tlCenter;
		SingleLine := True;
		Clipping := True;
		ExpandTabs := False;
		ShowPrefix := False;
		Wordbreak := False;
		Opaque := True;
		SystemFont := False;
		RightToLeft := False;
		EndEllipsis := True;
	end;
	CanCreateNewZone := True;

	Ruler := TBGRABitmap.Create;
	Zoner := TBGRABitmap.Create;
	Zoner.FontAntialias := True;
	Zoner.FontQuality := fqSystemClearType;
	Zoner.FontFullHeight := 16;

	//ConvertButtons(Self);
	LoadButtonImages(Self, Config.GetThemePath + 'images');

	// Create effects and bind to UI
	//
	Effects := TEffectsList.Create(True);
	SelectedEffect := 255;

	AddGUIEffect(TFxEcho.Create,       bEffect0);
	AddGUIEffect(TFxChorus.Create,     bEffect1);
	AddGUIEffect(TFxPanner.Create,     bEffect2);
	AddGUIEffect(TFxAutoWah.Create,    bEffect3);

	AddGUIEffect(TFxChopper.Create,    bEffect4);
	AddGUIEffect(TFxPhaser.Create,     bEffect5);
	AddGUIEffect(TFxReverb.Create,     bEffect6);
	AddGUIEffect(TFxCompressor.Create, bEffect7, 100);
//	AddGUIEffect(TFXPitchShift.Create, bEffect7, 90);

	//AddGUIEffect(nil, bEffect7); // loop controls
	//
	// Assign gui knobs to effect parameters
	InitGUIEffectParam(0, SliderFxParam0); InitGUIEffectParam(1, SliderFxParam1);
	InitGUIEffectParam(2, SliderFxParam2); InitGUIEffectParam(3, SliderFxParam3);
	InitGUIEffectParam(4, SliderFxParam4); InitGUIEffectParam(5, SliderFxParam5);

	SelectEffect(0);
	ShowPanel_Effects;

	Enabled := False;
end;

destructor TDeckFrame.Destroy;
begin
	Ruler.Free;
	Zoner.Free;
	Effects.Free;
	//Deck.Free; //owned by MainForm.DeckList

	inherited Destroy;
end;

function TDeckFrame.ProcessKeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
	Result := True;
	if not Enabled then Exit;

	case Key of
		VK_CONTROL:
			Cue(False);
		VK_SHIFT:
			IsShiftDown := False;
		VK_ADD, VK_SUBTRACT,
		VK_UP,  VK_DOWN:
			Deck.BendStop;
	else
		Result := False;
	end;
end;

function TDeckFrame.ProcessKeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
	Result := True;
	if not Enabled then Exit;

	case Key of

		VK_SPACE:		bPlay.OnClick(Self);

		VK_CONTROL:		Cue(True);

		VK_MENU: ;

		VK_SHIFT:
			if not IsShiftDown then
			begin
				CanCreateNewZone := True;
				IsShiftDown := True;
			end;

		VK_ADD,
		VK_UP:			Deck.BendStart(True,  (ssShift in Shift));

		VK_SUBTRACT,
		VK_DOWN:		Deck.BendStart(False, (ssShift in Shift));

		VK_S:
		begin
			if not IsShiftDown then
				bStoreClick(Self)
			else
				bStartMouseDown(Self, mbRight, Shift, 0, 0);
		end;

	else
		Result := False;
	end;
end;

function TDeckFrame.ProcessKeyPress(Key: Char): Boolean;
begin
	Result := True;
	if not Enabled then Exit;
	case Key of
		'+': bZoneAddButtonClick(Self);
		'-': bZoneDelButtonClick(Self);
		else
			Result := False;
	end;
end;

// input in graph sample coords
procedure TDeckFrame.SetCue(P: QWord);
begin
	if not Enabled then Exit;
	CuePos := P;
	GraphCue := Deck.Graph.PosToGraph(P, True);
	DrawWaveform;
	DrawGraph;
	DrawRuler;
end;

// input in graph pixel coords
procedure TDeckFrame.SetCue(P: TPoint);
begin
	if not Enabled then Exit;
	GraphCue := P;
	CuePos := Deck.Graph.GraphToPos(P, True);
	DrawWaveform;
	DrawGraph;
	DrawRuler;
end;

procedure TDeckFrame.HotCue(Mode: TDecksActionKind; Index: Integer; Pressed: Boolean);
var
	HotCue: ^THotCue;
label
	SetLeds;
begin
	if (Index < Low(Deck.HotCues)) or (Index > High(Deck.HotCues)) then
		Index := Low(Deck.HotCues);

	HotCue := @Deck.HotCues[Index];

	case Mode of
		DECK_HOTCUE_CLEAR:
		begin
			if Pressed then
			begin
				HotCue.Temporary := False;
				HotCue.Enabled := False;
				//MIDI.SetLed(DECK_HOTCUE, Deck.Index, False, Index);
				//MIDI.SetLed(DECK_HOTCUE_TEMP, Deck.Index, False, Index);
			end;
			goto SetLeds;
			//Exit;
		end;
		DECK_HOTCUE_SET:
			if not Pressed then Exit;
		else
			if not HotCue.Enabled then Mode := DECK_HOTCUE_SET;
	end;

	if Mode = DECK_HOTCUE_SET then
	begin
		HotCue.Pos := Deck.Graph.GraphToSongBytes(CuePos);
		HotCue.Temporary := False;
		HotCue.Enabled := True;
		//MIDI.SetLed(DECK_HOTCUE, Deck.Index, True, Index);
		//MIDI.SetLed(DECK_HOTCUE_TEMP, Deck.Index, True, Index);
	end
	else
	if HotCue.Enabled then
	begin
		if (not HotCue.Temporary) and (not Pressed) then Exit;

		if not HotCue.Temporary then // start playing hotcue
		begin
			if Deck.Paused then
			begin
				HotCue.TriggerPos := $FEEDBEEF;
				SetCue(Deck.Graph.SongToGraphBytes(HotCue.Pos));
				bPlay.SetMouseDown(mbRight, True);
			end
			else
			begin
				HotCue.TriggerPos := Deck.GetPlayPosition(False, False);
				JumpToPos(HotCue.Pos, True);
			end;
			HotCue.Temporary := Mode = DECK_HOTCUE_TEMP;
		end
		else // stop playing hotcue and jump back to trigger pos if temporary flag set
		begin
			if HotCue.TriggerPos = $FEEDBEEF then
				bPlay.SetMouseDown(mbRight, False);
			JumpToPos(HotCue.TriggerPos + Abs(Deck.GetPlayPosition(False, False) - HotCue.Pos), True);
			HotCue.Temporary := False;
			HotCue.TriggerPos := 0;
		end;
	end;

SetLeds:
	//for Index := Low(Deck.HotCues) to High(Deck.HotCues) do
		MIDI.SendMidiMessages(MODE_HOTCUE, Deck.HotCues[Index].Enabled, Deck.Index, Index);
end;

procedure TDeckFrame.AfterPosJump(Data: PtrInt);
var
	Zone: Integer;
begin
	if not Enabled then Exit;
	if not Deck.Paused then
		UpdatePlayButton(MODE_PLAY_START);
	Zone := Deck.Graph.GetZoneIndexAt(CuePos);
	Deck.UnloopZone;
	Deck.PlayingZone := Zone;
	ZoneChanged(Zone, True, True);
	PostMessage(Self.Handle, WM_ZONE, Zone, Zone);
	if Deck.Graph.Zones[Zone].Kind = zkLoop then
		Deck.LoopZone(Zone); // reapply loop, dumb
	ShowPosition;
end;

procedure TDeckFrame.JumpToPos(Pos: QWord; Reset: Boolean = False; ProcessAfter: Boolean = True);
var
	Flags: DWord = BASS_POS_BYTE;
begin
	if not Enabled then Exit;
	if Reset then
		Flags := Flags or BASS_POS_MIXER_RESET;
	BASS_Mixer_ChannelSetPosition(Deck.OrigStream, Pos, Flags);
	Deck.PlayPosition := Pos;
	if ProcessAfter then
		AfterPosJump(0);
end;

procedure TDeckFrame.JumpToCue(FromCallback: Boolean = False);
var
	Pos: QWord;
begin
	if not Enabled then Exit;
	Pos := Deck.Graph.GraphToSongBytes(CuePos);
	if not FromCallback then
		JumpToPos(Pos, True)
	else
	begin
		BASS_Mixer_ChannelSetPosition(Deck.OrigStream, Pos, 0);
		Application.QueueAsyncCall(AfterPosJump, 0);
	end;
end;

procedure TDeckFrame.JumpToZone(Zone: Word);
begin
	if not Enabled then Exit;
	Deck.UnloopZone;
	JumpToPos(Deck.Graph.GraphToSongBytes(Deck.Graph.Zones[Zone].Pos));
end;

procedure TDeckFrame.JumpToBar(Bar: Word);
begin
	if Enabled then
		JumpToPos(Deck.Graph.GraphToSongBytes(Deck.Graph.Bars[Bar].Pos));
end;

procedure TDeckFrame.SetKnob(const Knob: ThKnob; Value: Integer);
var
	Ghost: TKnobIndicator;
begin
	if Knob = nil then Exit;

	Ghost := Knob.GetExtraIndicator(-1);

	if Abs(Abs(Knob.Position) - Abs(Value)) <= Knob.PageSize then
	begin
		if Ghost.Visible then
		begin
			Ghost.Visible := False;
			Knob.Refresh;
		end
		else
			Knob.Position := Value;
	end
	else
	begin
		Ghost.Position := Value;
		Ghost.Visible  := True;
	end;
end;

procedure TDeckFrame.pbMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Enabled then
	case Button of

		mbLeft:
		begin
			GraphDragging := True;
			pbMouseMove(Sender, Shift, X, Y);
			SetMaster(Deck.Index);
		end;

		mbRight:
			if (Deck.Synced) and (not Deck.Paused) then
				SyncToOtherDeck(False)
			else
				JumpToCue;

		mbMiddle:
			SyncToOtherDeck(False);

	end;
end;

procedure TDeckFrame.pbMouseLeave(Sender: TObject);
begin
	if not Enabled then Exit;
	GraphHover := Point(-1, -1);
	DrawGraph;
end;

procedure TDeckFrame.pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
	if not Enabled then Exit;
	X := Min(Max(X, 0), pb.ClientWidth-1);
	Y := Min(Max(Y, 0), pb.ClientHeight-1);
	GraphHover := Point(X + Deck.Graph.Scroll.X, Y);
	if GraphDragging then
		SetCue(GraphHover)
	else
		DrawGraph;
end;

procedure TDeckFrame.pbMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	GraphDragging := False;
end;

procedure TDeckFrame.pbMouseWheel(Sender: TObject; Shift: TShiftState;
	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
	B, I: Integer;
	Step: Single;
	Delta: Single;
	Z: TZone;
begin
	Handled := True;
	if not Enabled then Exit;

	// shift+wheel to add/modify zone at cursor pos
	if Shift = [ssShift] then
	begin
		Delta := WheelAcceleration.Process(WHEEL_PB, WheelDelta);

		B := Deck.Graph.GraphToBar(GraphHover.x);

		I := Deck.Graph.Bars[B].Zone;
		Z := Deck.Graph.Zones[I];
		if Z = nil then Exit;

		if (CanCreateNewZone) and (Z.barindex <> B) then
		begin
			Z := Deck.Graph.AddZone(B);
			if Z <> nil then
			begin
				I := Deck.Graph.Zones.IndexOf(Z);
				CurrentZone := I;
			end;
		end;

		Step := BPMStep * Delta;
		if WheelDelta < 0 then
			Z.BPM := Z.BPM + Step
		else
		if WheelDelta > 0 then
			Z.BPM := Z.BPM - Step;

		ZoneChanged(I, False, False);
		RedrawGraph(True);

		CanCreateNewZone := False;
	end
	else
	// alt+wheel to adjust song start position
	if Shift = [ssAlt] then
	begin
		Delta := WheelAcceleration.Process(WHEEL_PB, WheelDelta, 2.0);

		I := Trunc(Deck.Graph.GetBarLength(False, 0) / pb.ClientHeight / 10 * Delta); // shift by 1 graph pixel

		if WheelDelta > 0 then
			Deck.Graph.StartPos += I
		else
		if WheelDelta < 0 then
			Deck.Graph.StartPos := Max(0, Deck.Graph.StartPos - I);

		Deck.Graph.ZonesLoaded;
		RedrawGraph(True);
		SetCue(GraphCue);
	end
	else
	// zoom graph with wheel
	begin
		if WheelDelta > 0 then
			ZoomGraph(+1)
		else
		if WheelDelta < 0 then
			ZoomGraph(-1);
	end;
end;

procedure TDeckFrame.pbRulerMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if not Enabled then Exit;
	X := Min(Max(X, 0), pbRuler.ClientWidth-1) + Deck.Graph.Scroll.X;
	if Button = mbRight then X := X - (X mod (4*Deck.Graph.Zoom));
	GraphCue := Point(X, 0);
	SetCue(GraphCue);
end;

procedure TDeckFrame.pbRulerDblClick(Sender: TObject);
begin
	if Enabled then
	begin
		if (Deck.Synced) and (not Deck.Paused) then
			SyncToOtherDeck(False)
		else
			JumpToCue;
	end;
end;

procedure TDeckFrame.lBPMChange(Sender: TObject);
begin
	if Deck = nil then Exit;

	Deck.Graph.ZonesChanged;
	RedrawGraph(True);
	DrawWaveform;
	Deck.SetBPM(MasterBPM);
end;

procedure TDeckFrame.bZoneAddButtonClick(Sender: TObject);
var
	Z: TZone;
begin
	Z := Deck.Graph.AddZone(Deck.Graph.GraphToBar(GraphCue.X));
	if Z <> nil then
	begin
		CurrentZone := Deck.Graph.Zones.IndexOf(Z);
		RedrawGraph(True);
	end;
end;

procedure TDeckFrame.bZoneDelButtonClick(Sender: TObject);
begin
	if Deck.Graph.RemoveZone(CurrentZone) then
	begin
		if CurrentZone > 0 then Dec(CurrentZone);
		ZoneChanged(CurrentZone, False, False);
		RedrawGraph(True);
	end;
end;

procedure TDeckFrame.pnlEffectsResize(Sender: TObject);
begin
	ResizeEffectButtons;
end;

procedure TDeckFrame.miShowFileClick(Sender: TObject);
begin
	MainForm.SelectFileInFileList(Deck.Filename, True);
end;

procedure TDeckFrame.bLoopBeat1SetDown(Sender: TObject);
var
	Btn: TDecksButton;
	K: TAppMode;
begin
	if (Sender = nil) or not (Sender is TDecksButton) then Exit;

	Btn := TDecksButton(Sender);
	Btn.StateNormal.Border.LightWidth := IfThen(Btn.Down, 1, 0);

	case Btn.Tag of
	    -2: K := MODE_LOOP_ZONE;
		-1: K := MODE_LOOP_SONG;
		else K := MODE_LOOP;
	end;
	//LoopControls[DECK_LOOP] := (Sender as TDecksButton);

	Deck.SetLoop(K, Btn.Tag, Btn.Down);
end;

procedure TDeckFrame.bStoreFxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	B: Boolean;
begin
	B := not Effects[SelectedEffect].IsStored;
	Effects[SelectedEffect].IsStored := B;
	bStoreFx.Down := B;
end;

procedure TDeckFrame.bStoreFxSetDown(Sender: TObject);
begin
	bStoreFx.StateNormal.Border.LightWidth := IfThen(bStoreFx.Down, 1, 0);
end;

procedure TDeckFrame.bPitchLockSetDown(Sender: TObject);
begin
	bPitchLock.StateNormal.Border.LightWidth := IfThen(bPitchLock.Down, 1, 0);
	Deck.PitchLock := bPitchLock.Down;
	Deck.SetBPM(MasterBPM);
end;

procedure TDeckFrame.pbRulerRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
	DrawRuler;
end;

procedure TDeckFrame.pbWaveMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
{var
	Z: TZone;}
begin
	if Enabled then
	case Button of
		mbLeft:
		begin
			SetMaster(Deck.Index);
			SetCaptureControl(pbWave);
			DragWave.Offset := X;
			if PtInRect(Point(X, Y), BeatDrag.Rect) then
			begin
				DragWave.Dragging := DRAG_BEAT;
				{Z := Deck.Graph.AddZone(Deck.Graph.PosToBar(BeatDrag.Pos, True), 0);
				if Z <> nil then
					RedrawGraph(True);}
			end
			else
				DragWave.Dragging := DRAG_WAVE;
		end;

		mbRight:
		begin
			if (Deck.Synced) and (not Deck.Paused) then
				SyncToOtherDeck(False)
			else
				JumpToCue;
		end;
	end;
end;

procedure TDeckFrame.pbWaveMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	DragWave.Dragging := DRAG_NONE;
	SetCaptureControl(nil);
end;

procedure TDeckFrame.pbWaveMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	Z: TZone;
	Ch: Single;
begin
	if Deck = nil then Exit;

	if Enabled then
	case DragWave.Dragging of

		DRAG_WAVE:
		begin
			SetCue(Max(0, CuePos {%H-}- ((DragWave.Offset - X) * WaveformStep)));
			DragWave.Offset := X;
		end;

		DRAG_BEAT:
		begin
			Z := BeatDrag.Zone;

			Ch := (X - DragWave.Offset);
			Ch := 1 - ((Ch / pbWave.ClientWidth) / 100);
			Z.BPM := Z.BPM * Ch;

			// update visuals
			DragWave.Offset := X;
			Deck.Graph.ZonesChanged;
			RedrawGraph(True, True);
			DrawWaveform;
			Deck.SetBPM(MasterBPM);
		end;

		else
			if PtInRect(Point(X, Y), BeatDrag.Rect) then
				pbWave.Cursor := crArrow
			else
				pbWave.Cursor := crSizeWE;
	end;
end;

procedure TDeckFrame.pbWaveMouseWheel(Sender: TObject; Shift: TShiftState;
	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	Handled := True;
	if not Enabled then Exit;

	if WheelDelta > 0 then
		ZoomSample(+1)
	else
	if WheelDelta < 0 then
		ZoomSample(-1);
end;

procedure TDeckFrame.pbWaveRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
	DrawWaveform;
end;

procedure TDeckFrame.pbZonesDblClick(Sender: TObject);
begin
	SetCue(Deck.Graph.Zones[CurrentZone].Pos);
end;

procedure TDeckFrame.pbZonesMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
var
	P: TPoint;
begin
	if not Enabled then Exit;

	SetMaster(Deck.Index);
	Y := Deck.Graph.GraphToBar(X + Deck.Graph.Scroll.X);
	CurrentZone := Deck.Graph.Bars[Y].Zone;
	ZoneChanged(CurrentZone, False, False);

	case Button of
		mbMiddle:
			JumpToZone(CurrentZone);

		mbRight:
		begin
			Y := Ord(Deck.Graph.Zones[CurrentZone].Kind);
			PopupZone.Items[Y].Checked := True;
			P := ClientToScreen(Point(X, pbZones.Top + pbZones.Height));
			PopupZone.PopUp(P.X, P.Y);
		end;
	end;
end;

procedure TDeckFrame.pbZonesRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
	DrawZones;
end;

procedure TDeckFrame.SliderGraphXChange(Sender: TObject);
begin
	Deck.Graph.Scroll.X := Trunc(SliderGraphX.Position);

	DrawGraph;
	DrawRuler(False);
	DrawZones(False);
end;

procedure TDeckFrame.OnGraphIteration;
begin
	DrawGraph;
	Application.ProcessMessages;
end;

// ================================================================================================
// Drawing
// ================================================================================================

procedure TDeckFrame.DrawZones(Recalc: Boolean = True);
var
	Z, H, X1, X2: Integer;
	Col: TColor;
	R: TRect;
	Zone: TZone;
begin
	if not Enabled then Exit;

	if Recalc then
	begin
		H := pbZones.ClientHeight;
		Zoner.SetSize(Deck.Graph.Width, H);
		Zoner.FontHeight := Trunc((H-2) * 0.8);
		X2 := Zoner.Width-1;

		Zoner.Fill(BGRABlack);

		if Deck.Loaded then
		for Z := Deck.Graph.Zones.Count-1 downto 0 do
		begin
			Zone := Deck.Graph.Zones[Z];
			//X1 := Deck.Graph.PosToGraph(Zone.Pos).X;
			X1 := Deck.Graph.BarToGraph(Zone.barindex);

			R := Rect(X1, 0, X2, H-1);

			if Z = CurrentZone then
			begin
				Zoner.FillRect(R, BGRA($FF,$AA,$00));
				Col := clBlack;
			end
			else
			begin
				case Zone.Kind of
					zkNormal:	Col := clGray;
					zkLoop:		Col := clNavy;
					zkJump:		Col := clTeal;
					zkSkip:		Col := clMaroon;
					zkEnd:		Col := clBlack;
					else		Col := clGray;
				end;
				Zoner.FillRect(R, ColorToBGRA(Col));
				Col := clWhite;
			end;

			if Z = Deck.PlayingZone then
			begin
				Zoner.HorizLine(R.Left+1, R.Bottom-2, R.Right-1, BGRAWhite);
				Zoner.HorizLine(R.Left+1, R.Bottom-1, R.Right-1, BGRAWhite);
				//Zoner.RaiseRectTS(R, +30);
			end;

			Zoner.TextRect(R, X1+2, 2, Format('%.3f', [Zone.BPM]), ZoneTextStyle, Col);

			X2 := X1 - 1;
		end;
	end;

	pbZones.Bitmap.PutImage(-Deck.Graph.Scroll.X, 0, Zoner, dmSet);
	pbZones.Invalidate;
end;

procedure TDeckFrame.DrawRuler(Recalc: Boolean = True);
const
	Col: array[0..3] of TColor = (clWhite, clSilver, clGray, clSilver);
var
	C, A, X, Z, H, Gap: Integer;
	XX, CX: Cardinal;
	Cl: TBGRAPixel;
begin
	if not Enabled then Exit;

	if Recalc then
	begin
		H := pbRuler.ClientHeight;
		Ruler.SetSize(Deck.Graph.Width, H);
		Ruler.Fill(BGRABlack);

		if Deck.Loaded then
		begin
			Z := Deck.Graph.Zoom;
			X := 0;
			C := 0;
			Gap := IfThen(Z > 2, 1, 0);
			XX := 0;
			CX := GraphCue.X div Z;
			repeat
				for A := 0 to 3 do
				begin
					if XX = CX then
						Cl := BGRA(30, 255, 20)
					else
						Cl := ColorToBGRA(Col[C mod 4]);
					Ruler.FillRect(X, 1, X+Z-Gap, H-1, Cl);
					Inc(X, Z);
					Inc(XX);
				end;
				Inc(C); // color index
			until X >= Ruler.Width;
		end;
	end;

	pbRuler.Bitmap.PutImage(-Deck.Graph.Scroll.X, 0, Ruler, dmSet);
	pbRuler.Invalidate;
end;

function TDeckFrame.DoDrawWaveform(Pos: QWord; Bar: Cardinal; Brightness: Single; Palette: PBGRAPixel): QWord;
var
	X, W: Cardinal;
	HY, Y: Integer;
	FAdd, FPos: Double;
	BL, L, Sam, TPos, BeatPos: QWord;
	Sample: PByte;
	IsFirstBeat: Boolean;
	BeatCol: array[Boolean] of TBGRAPixel;
begin
	BL := Deck.Graph.GetBarLength(True, Bar);
	L := BL * 2 div Trunc(Power(2, SampleZoom));
	if L < 10 then Exit(0);

	W := pbWave.ClientWidth;
	FAdd := (L / W);
	WaveformStep := Round(FAdd);

	Pos := Min(Pos, Deck.Graph.length_audio - L);
	Result := Pos;

	BeatPos := Deck.Graph.GetNextBeat(Pos, IsFirstBeat);
	BL := Trunc(BL / 4);

	if FAdd >= 0.1 then
	begin
		BeatCol[True]  := BGRA(55, 255, 255);
		BeatCol[False] := BGRA(120, 255, 60);

		HY := pbWave.Bitmap.Height div 2 - 1;
		FPos := Pos;
		for X := 0 to W-1 do
		begin
			// collect samples for this horizontal position in waveform display
			TPos := Trunc(FPos);
			Sam := 0;
			Sample := @Deck.Graph.AudioData[TPos];
			for Y := 1 to WaveformStep do
			begin
				Sam += Sample^;
				Inc(Sample);
			end;

			// draw beat indicator if a beat is located in this position
			if TPos >= BeatPos then
			begin
				pbWave.Bitmap.VertLine(X, 0, pbWave.Bitmap.Height-1, BeatCol[IsFirstBeat]);
				// draw the drag box for the first beat in a bar
				if IsFirstBeat then
				begin
					if DragWave.Dragging = DRAG_NONE then
					begin
						BeatDrag.Pos := TPos;
						BeatDrag.Zone := Deck.Graph.GetZoneAt(Max(0, BeatPos - Deck.Graph.GetBeatLength(False)));
					end;
					BeatDrag.Rect := Bounds(X - (BeatDrag.RectSize div 2), 0, BeatDrag.RectSize, BeatDrag.RectSize);
					pbWave.Bitmap.FillRect(BeatDrag.Rect, BeatCol[True]);
				end;
				BeatPos := Deck.Graph.GetNextBeat(BeatPos+1, IsFirstBeat);
			end;

			// draw the sample data
			FPos += FAdd;
			Sam := Min(255, Trunc(Sam / FAdd {* Brightness}));
			Y := Trunc(Sam / 256 * HY);
			pbWave.Bitmap.VertLine(X, HY-Y, HY+Y, Palette[Sam]);
		end;
	end;
end;

procedure TDeckFrame.DrawWaveform;
var
	X: Cardinal;
begin
	if not Enabled then Exit;

	pbWave.Bitmap.Fill(BGRABlack);

	X := Deck.Graph.GraphToBar(GraphCue.X);
	CuePos := DoDrawWaveform(CuePos, X, Deck.Graph.Brightness, @Grays[0]);

	if Config.Deck.Waveform.ShowDual then
	begin
		X := Deck.Graph.GetZoneIndexAt(CuePos);
		X := Deck.Graph.Zones[X].barindex + Deck.Graph.Zones[X].amount_bars - 1;
		DoDrawWaveform(Deck.Graph.GraphToPos(Point(Deck.Graph.BarToGraph(X), GraphCue.Y), True),
			X, Deck.Graph.Brightness, @Grays2[0]);
	end;

	pbWave.Repaint;
end;

procedure TDeckFrame.DrawGraph;
const
	COLOR_HOVER = clAqua;
	CUESIZE_Y = 1;
var
	X, Y, W, H, Z, ScrollX: Integer;
	CueSize: Byte;
	R: TRect;
	P: TPoint;
	C: TBGRAPixel;
begin
	if not Enabled then Exit;

	ScrollX := Deck.Graph.Scroll.X;
	W := pb.ClientWidth-1;
	H := pb.ClientHeight-1;
	Z := Deck.Graph.Zoom;
	case Z of
		0:    Exit;
		1..4: CueSize := 2;
		5:    CueSize := 1;
		else  CueSize := 0;
	end;

	// draw graph base
	//
	pb.Bitmap.Fill(BGRABlack);
	pb.Bitmap.PutImage(-ScrollX, 0, Deck.Graph.Bitmap, dmSet);

	// cue position indicator
	//
	X := GraphCue.X;
	Y := GraphCue.Y;
	if X > 0 then
		X := (X div Z * Z) - ScrollX;
	R.TopLeft := Point(X - CueSize, Y - CUESIZE_Y);
	R.BottomRight := Point(X+Z + CueSize, Y + CUESIZE_Y + 1);
	pb.Bitmap.FillRect(R, BGRA(130, 255, 30, 200), dmLinearBlend);

	// loop range indicator
	//
	if Deck.LoopInfo_Misc.Enabled then
	begin
		Deck.Graph.ColorizeArea(pb.Bitmap,
			Deck.LoopInfo_Misc.StartPos, Deck.LoopInfo_Misc.EndPos,
			BGRA(255, 255, 255, 130)); //BGRA(100,200,0,200)
	end;

	// playback position indicator
	//
	P := Deck.Graph.PosToGraph(Deck.PlayPosition, False);

	Y := Trunc(P.Y / H * 4); // quadrant
	if Y <> Deck.BeatPreviousQuadrant then
	begin
		Deck.BeatFadeCounter := 255;
		Deck.BeatPreviousQuadrant := Y;
	end;

	X := P.X - ScrollX;

	lMeasure.Caption := IntToStr(X div Deck.Graph.Zoom + 1 {Deck.Graph.PosToBar(PlayPosition, False)}) + '.' + IntToStr(Y+1);

	Y := P.Y;
	R.TopLeft := Point(X, 0);
	R.BottomRight := Point(X+Z, H);
	pb.Bitmap.FillRect(R, BGRA(255, 100, 30, 130), dmLinearBlend);
	pb.Bitmap.HorizLine(X, Y, X+Z-1, ColorToBGRA(clYellow));

	if Config.Deck.BeatGraph.ShowHorizontalLines then
	begin
		for X := 0 to 3 do
		begin
			Y := X * (pb.Bitmap.Height div 4);
			pb.Bitmap.HorizLine(0, Y, pb.Bitmap.Width, BGRA(30,250,30,255));
		end;
	end;

	// mouseover coords indicator
	//
	if GraphHover.X >= 0 then
	begin
		X := GraphHover.X;
		Y := GraphHover.Y;
		C := ColorToBGRA(COLOR_HOVER);
		pb.Bitmap.HorizLine(0, Y, W, C);
		if Z < 2 then
			pb.Bitmap.HorizLine(X - ScrollX, 0, H, C)
		else
		begin
			X := (X div Z * Z) - ScrollX;
			R.TopLeft := Point(X, 0);
			R.BottomRight := Point(X+Z, H);
			C.alpha := 130;
			pb.Bitmap.FillRect(R, C, dmLinearBlend);
		end;
	end;

	pb.Repaint;
end;

procedure TDeckFrame.RedrawGraph(Recalc: Boolean = False; IgnoreGUIBPM: Boolean = False);
var
	BPM: Single;
begin
	if not Enabled then Exit;

	if not IgnoreGUIBPM then
	begin
		BPM := lBPM.Value; //SliderTempo.Position + (SliderTempoFrac.Position / 1000);
		if Deck.Graph.Zones.Count > 0 then
		begin
			Deck.Graph.Zones[CurrentZone].BPM := BPM;
			if Recalc then
				Deck.Graph.NeedRecalc := True;
		end;
	end;

	Recalc := Deck.Graph.NeedRecalc;

	if Recalc then
	begin
		Deck.GetAvgBPM;
		Deck.BPM := MasterBPM;
		Deck.Info.Amp := SliderAmp.Position / 100;
//		Deck.Graph.Brightness := Deck.Graph.CalcBrightness * Deck.Info.Amp;
		Deck.SetVolume(-1); // update volume
	end;

	Deck.Graph.Draw(pb.ClientWidth-1, pb.ClientHeight-1);
	DrawRuler;
	DrawZones;
	DrawGraph;

	if Recalc then
		Deck.ReapplyLoops;

	with SliderGraphX do
	begin
		OnChange := nil;
		Range := Deck.Graph.Width;
		Window := pb.ClientWidth;
		OnChange := SliderGraphXChange;
	end;
end;

procedure TDeckFrame.OnLoadProgress(Percentage: Integer);
var
	X, W, H: Integer;
begin
	with pbZones do
	begin
		W := ClientWidth; H := ClientHeight;
		Bitmap.FillRect(Bounds(0, 0, W, H), BGRABlack, dmSet);
		if (Percentage >= 0) and (Percentage <= 100) then
		begin
			X := Trunc(W / 100 * Percentage);
			Bitmap.FillRect(Bounds(0, 0, X, H), Grays[Trunc(255 / 100 * Percentage)], dmSet);
		end;
		Repaint;
	end;
	Application.ProcessMessages;
end;

procedure TDeckFrame.UpdateCaption;
begin
	bDeckMenu.Caption := '☰ ' + Deck.Index.ToString;
	if not Deck.Tags.Title.IsEmpty then
	begin
		if not Deck.Tags.Artist.IsEmpty then
			bMaster.Caption := Deck.Tags.Artist + ' - ' + Deck.Tags.Title
		else
			bMaster.Caption := Deck.Tags.Title;
	end
	else
		bMaster.Caption := ExtractFileName(Deck.Filename);
	bMaster.Hint := Deck.Filename;
end;

procedure TDeckFrame.UnloopAll;
begin
	if Deck <> nil then
		Deck.UnloopAll;
	bLoopSong.Down := False;
	bLoopZone.Down := False;
	bLoopBeat1.Down := False;
	bLoopBeat2.Down := False;
	bLoopBar1.Down  := False;
	bLoopBar2.Down := False;
	bLoopBar4.Down := False;
end;

procedure TDeckFrame.pbZonesMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
	MousePos: TPoint; var Handled: Boolean);
var
	Z: TZone;
	Delta: Single;
	Step: Single;
begin
	Handled := True;
	if not Enabled then Exit;

	if Shift = [ssShift] then
	begin
		Delta := WheelAcceleration.Process(WHEEL_PBZONES, WheelDelta);

		Z := Deck.Graph.Zones[CurrentZone];

		Step := BPMStep * Delta;
		if WheelDelta < 0 then
			Z.BPM := Z.BPM + Step
		else
		if WheelDelta > 0 then
			Z.BPM := Z.BPM - Step;

		ZoneChanged(CurrentZone, False, False);
		RedrawGraph(True);
	end;
end;

{
procedure TDeckFrame.SliderTempoFracMouseWheelDown(Sender: TObject; Shift: TShiftState;
	MousePos: TPoint; var Handled: Boolean);
begin
	if SliderTempoFrac.Position <= SliderTempoFrac.Min then
	begin
		SetSlider(SliderTempoFrac, 990);
		SliderTempo.Position := SliderTempo.Position - 1;
		Handled := True;
	end;
end;

procedure TDeckFrame.SliderTempoFracMouseWheelUp(Sender: TObject; Shift: TShiftState;
	MousePos: TPoint; var Handled: Boolean);
begin
	if SliderTempoFrac.Position >= 990 then
	begin
		SetSlider(SliderTempoFrac, SliderTempoFrac.Min);
		SliderTempo.Position := SliderTempo.Position + 1;
		Handled := True;
	end;
end;
}
procedure TDeckFrame.SetZoneKind(Zone: Word; Kind: TZoneKind);
var
	Z: TZone;
begin
	if not Enabled then Exit;
	if Zone >= Deck.Graph.Zones.Count then Exit;

	Z := Deck.Graph.Zones[Zone];
	Z.Kind := Kind;

	if Kind = zkJump then
		Z.Data := Deck.Graph.GraphToPos(GraphCue)
	else
		Z.Data := 0;

	RedrawGraph;

	if Kind = zkLoop then
		Deck.LoopZone(Zone)
	else
		Deck.UnloopZone;
end;

procedure TDeckFrame.miZoneKind0Click(Sender: TObject);
begin
	SetZoneKind(CurrentZone, TZoneKind((Sender as TMenuItem).Tag));
end;

procedure TDeckFrame.BeginFormUpdate;
begin
	MainForm.StartUpdate;
end;

procedure TDeckFrame.EndFormUpdate;
begin
	MainForm.EndUpdate;
end;

procedure TDeckFrame.ShowPanel_Effects;
var
	B: Boolean;
begin
	B := MainForm.bToggleEffects.Down;
	pnlEffects.Top := Height-2;// pnlControls.Top - pnlEffects.Height;
	pnlEffects.Visible := B;
end;

procedure TDeckFrame.ApplyEffects;
var
	Fx: TGUIEffect;
begin
	Deck.ApplyFilter;
	if (Effects <> nil) and (Deck.OrigStream <> 0) then
		for Fx in Effects do
			if Fx.Effect <> nil then
			begin
				Fx.Effect.Stream := Deck.OrigStream;
				Fx.Effect.BPM := @Deck.AvgBPM;
				Fx.Effect.Deck := Self.Deck;
			end;
end;

procedure TDeckFrame.SetEffectParam(ParamNum: Integer; Value: Single);
var
	Fx: TBaseEffect;
	Knob: ThKnob;
	H, i: Integer;
begin
	if (Value < 0.0) or (Value > 1.0) then Exit;

	Fx := Effects[SelectedEffect].Effect;
	if Fx = nil then Exit;

	i := ParamNum;
	if (i < 0) or (i >= Fx.Params.Count) then
		i := SelectedEffectParam;

	Knob := GUIEffectParams[i].Knob;
	if Knob <> nil then
		Knob.Position := Trunc(((Knob.Max - Knob.Min) * Value) + Knob.Min);
end;

procedure TDeckFrame.SelectEffectParam(ParamNum: DWord);
var
	Fx: TBaseEffect;
	Knob: ThKnob;
	H, i: Integer;
	ParNum: Integer;
begin
	Fx := Effects[SelectedEffect].Effect;
	if Fx = nil then Exit;

	H := Fx.Params.Count-1;

	if ParamNum = VAL_BROWSE_NEXT then
		ParNum := SelectedEffectParam + 1
	else
	if ParamNum = VAL_BROWSE_PREV then
		ParNum := SelectedEffectParam - 1
	else
		ParNum := ParamNum and $FF;

	if ParNum < 0 then ParNum := H else
	if ParNum > H then ParNum := 0;
	SelectedEffectParam := ParNum;

	for i := 0 to High(GUIEffectParams) do
	begin
		Knob := GUIEffectParams[i].Knob;
		if Knob = nil then Continue;

		if i = SelectedEffectParam then
			Knob.Knob.BorderColor := clWhite
		else
			Knob.Knob.BorderColor := clGray;
		Knob.Repaint;
	end;
end;

procedure TDeckFrame.SelectEffect(EffectNum: DWord);
var
	Fx: TBaseEffect;
	Param: TEffectParam;
	GUIParam: TGUIEffectParam;
	Knob: ThKnob;
	Button: TDecksButton;
	MI: TMenuItem;
	Num, i, M: Integer;
	B: Boolean;
begin
	if SelectedEffect = EffectNum then Exit;

	if EffectNum = VAL_BROWSE_NEXT then
		Num := SelectedEffect + 1
	else
	if EffectNum = VAL_BROWSE_PREV then
		Num := SelectedEffect - 1
	else
		Num := EffectNum;

	if Num >= Effects.Count then Num := 0
	else
	if Num < 0 then Num := Effects.Count-1;

	BeginFormUpdate;
	DisableAutoSizing;

	SelectedEffect := Num;

	for i := 0 to Effects.Count-1 do
	begin
		B := (i = Num);
		Button := Effects[i].Button;
		if B then
		begin
			Button.StateNormal.Background.Color := clTeal;
			if Button.HelpContext = 0 then
			begin
				Button.DropDownMenu := PopupEffectPresets;
				//Button.Style := bbtDropDown;
				Button.DropDownArrow := True;
			end;
		end
		else
		begin
			Button.StateNormal.Background.Color := $005F5C5A;
			Button.DropDownMenu := nil;
			Button.Style := bbtButton;
			Button.DropDownArrow := False;
		end;
	end;

	PopupEffectPresets.Items.Clear;

	bStoreFx.Down := Effects[SelectedEffect].IsStored;

	Fx := Effects[SelectedEffect].Effect;
	B := Fx <> nil;
	//pnlEffectKnobs.Visible := B;
	//pnlEffectLoop.Visible := not B;

	if B then
	begin
		Button := Effects[SelectedEffect].Button;
		Button.StateNormal.Border.LightWidth := IfThen(Fx.Enabled, 2, 0);

		for i := 0 to High(GUIEffectParams) do
		begin
			GUIParam := GUIEffectParams[i];

			Knob := GUIParam.Knob;
			B := (i < Fx.Params.Count);
			Knob.OnChange := nil;
			Knob.Visible := B;

			if B then
			begin
				Param := Fx.Params[i];
				M := Param.Multiplier;
				Knob.Max := Trunc(Param.Max * M);
				Knob.Min := Trunc(Param.Min * M);
				Knob.Multiplier := M;
				Knob.FloatPosition := Param.Value;
				Knob.Hint := Param.Name;
				Knob.OnChange := SliderFxParam0Change;
				if GUIParam.NameLabel <> nil then
				begin
					GUIParam.NameLabel.Caption := Param.Name;
					GUIParam.NameLabel.Hint := Param.Caption;
					GUIParam.NameLabel.ShowHint := True;
				end;
			end;
			if GUIParam.NameLabel <> nil then
				GUIParam.NameLabel.Visible := B;
		end;

		for i := 0 to Fx.Presets.Count-1 do
		begin
			MI := TMenuItem.Create(PopupEffectPresets);
			MI.Caption := Fx.Presets[i].Name;
			MI.Tag := i;
			MI.OnClick := miApplyPresetClick;
			PopupEffectPresets.Items.Add(MI);
		end;
	end;

	SelectEffectParam(0);
	ResizeEffectButtons;
	EnableAutoSizing;
	EndFormUpdate;
	OnDeckEvent(MODE_FX_ENABLE, Effects[SelectedEffect].Effect.Enabled);
end;

procedure TDeckFrame.ResizeEffectButtons;
var
	i, X, W: Integer;
	Effect: TGUIEffect;
	Button: TDecksButton;
	Knob: ThKnob;
	GUIParam: TGUIEffectParam;
begin
	BeginFormUpdate;
	DisableAutoSizing;

	W := ({bLoopSong.Left}pnlEffects.Width div 2 - 4) div {Effects.Count}4;

	for i := 0 to Effects.Count-1 do
	begin
		if (i = 0) or (i = 4) then X := 5;
		Effect := Effects[i];
		Button := Effect.Button;
		if Button = nil then Continue;
		Button.SetBounds(X, Button.Top, W, Button.Height);
		if Effect.Effect <> nil then // adjust captions to fit
		begin
			if W <= Effect.LabelWidth then
				Button.Caption := Effect.Effect.ShortName
			else
				Button.Caption := Effect.Effect.Name;
		end;
		Inc(X, W);
	end;

	for i := 0 to High(GUIEffectParams) do
	begin
		GUIParam := GUIEffectParams[i];
		Knob := GUIParam.Knob;
		if not Knob.Visible then Continue;

		Knob.Left := Trunc(i * Knob.Width * 1.3) + (pnlEffects.Width div 2) + 5;
		if GUIParam.NameLabel <> nil then
			GUIParam.NameLabel.SetBounds(Knob.Left-20, {Knob.Top + Knob.Height + 1} 0, Knob.Width+40, 18+10);
	end;

	EnableAutoSizing;
	EndFormUpdate;
end;

procedure TDeckFrame.ShowEffectValue(Knob: ThKnob);
begin
	if Knob <> nil then
		Knob.PositionLabel.Caption := Effects[SelectedEffect].Effect.Params[Knob.Tag].Text;
end;

function TDeckFrame.GetEffectKnob(Sender: TObject): ThKnob;
begin
	if not (Sender is ThKnob) then
		Result := nil
	else
		Result := ThKnob(Sender);
end;

procedure TDeckFrame.SliderFxParam0Change(Sender: TObject);
var
	Knob: ThKnob;
	Param: TEffectParam;
begin
	Knob := GetEffectKnob(Sender);
	if Knob <> nil then
	begin
		Knob.GetExtraIndicator(-1).Visible := False;
		Param := Effects[SelectedEffect].Effect.Params[Knob.Tag];
		Param.Value := Knob.FloatPosition;
		ShowEffectValue(Knob);
	end;
end;

procedure TDeckFrame.SliderFxParam0MouseEnter(Sender: TObject);
begin
	ShowEffectValue(GetEffectKnob(Sender));
end;

procedure TDeckFrame.SliderFxParam0MouseLeave(Sender: TObject);
var
	Knob: ThKnob;
begin
	Knob := GetEffectKnob(Sender);
	if Knob <> nil then
		Knob.PositionLabel.Caption := Effects[SelectedEffect].Effect.Params[Knob.Tag].Name;
end;

procedure TDeckFrame.bEffect0Click(Sender: TObject);
begin
	SelectEffect((Sender as TControl).Tag);
end;

procedure TDeckFrame.miApplyPresetClick(Sender: TObject);
var
	MI: TMenuItem;
	Fx: TBaseEffect;
	Knob: ThKnob;
	i: Integer;
begin
	if not (Sender is TMenuItem) then Exit;
	MI := (Sender as TMenuItem);

	Fx := Effects[SelectedEffect].Effect;
	if Fx = nil then Exit;

	Fx.ApplyPreset(Fx.Presets[MI.Tag]);

	for i := 0 to Min(Fx.Params.Count-1, High(GUIEffectParams)) do
	begin
		Knob := GUIEffectParams[i].Knob;
		Knob.FloatPosition := Fx.Params[i].Value;
		SliderFxParam0MouseLeave(Knob);
	end;
end;

procedure TDeckFrame.EnableEffect(EffectIndex: Byte; Toggle: Boolean = False);
var
	Fx: TBaseEffect;
	B: Boolean;
begin
	if (EffectIndex >= Effects.Count) then Exit;
	Fx := Effects[EffectIndex].Effect;
	if Fx = nil then Exit;

	if Toggle then
		B := not Fx.Enabled
	else
		B := Fx.Enabled;
	with Effects[EffectIndex].Button do
	begin
		StateNormal.Border.LightWidth := IfThen(B, 2, 0);
		Down := B;
	end;
	Fx.Stream := Deck.OrigStream;
	Fx.Enabled := B;
	OnDeckEvent(MODE_FX_ENABLE, B);
end;

procedure TDeckFrame.bEffect0MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbRight then
		if (Sender is TDecksButton) then
			EnableEffect((Sender as TDecksButton).Tag, True);
end;

procedure TDeckFrame.miSetMasterTempoClick(Sender: TObject);
begin
	MainForm.SetMasterTempo(Deck.AvgBPM);
end;


end.

