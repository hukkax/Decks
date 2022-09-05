unit Frame.Deck;

{$mode Delphi}
{$INLINE ON}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
	Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, StdCtrls,
	ExtCtrls, EditBtn, Buttons, LCLType, LCLIntf, LMessages, Menus, ComCtrls,
	BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
	Decks.Audio, Decks.Deck, Decks.Beatgraph, Decks.SongInfo, Decks.Effects,
	BASS, BASSmix,
	hKnob, hSlider, DecksButton, BCTypes, FGL;

const
	BPMStep = 0.01;

	WM_ZONE = LM_USER + 2010;

type
	TDragInfo = record
		Dragging: Boolean;
		Offset:   Integer;
	end;

	TGUIEffect = class
		Effect: TBaseEffect;
		Button: TDecksButton;

		destructor Destroy; override;
	end;

	TGUIEffectParam = record
		Knob:       ThKnob;
		NameLabel:  TLabel;
		ValueLabel: TLabel;
	end;

	TEffectsList = TFPGObjectList<TGUIEffect>;

	TDeckFrame = class(TBaseDeckFrame)
		bLoopBar: TDecksButton;
		bLoopBar2: TDecksButton;
		bLoopBar4: TDecksButton;
		bLoopBeat: TDecksButton;
		bLoopBeat2: TDecksButton;
		bLoopSong: TDecksButton;
		bLoopZone: TDecksButton;
		bSync: TDecksButton;
		bStore: TDecksButton;
		bStart: TDecksButton;
		bMaster: TLabel;
		bPlay: TDecksButton;
		bBendUp: TDecksButton;
		bBendDown: TDecksButton;
		bReverse: TDecksButton;
		bDeckMenu: TDecksButton;
		miAudioDevices: TMenuItem;
		miDeckClose: TMenuItem;
		N1: TMenuItem;
		pb: TBGRAVirtualScreen;
		pbRuler: TBGRAVirtualScreen;
		pbWave: TBGRAVirtualScreen;
		pbZones: TBGRAVirtualScreen;
		PopupMenu: TPopupMenu;
		SliderAmp: ThKnob;
		lTime: TLabel;
		shpGraph: TShape;
		SliderGraphX: ThRangeBar;
		SliderTempo: ThGaugeBar;
		SliderTempoFrac: ThGaugeBar;
		Timer: TTimer;
		PopupZone: TPopupMenu;
		miZoneKind0: TMenuItem;
		miZoneKind1: TMenuItem;
		miZoneKind2: TMenuItem;
		miZoneKind3: TMenuItem;
		miZoneKind4: TMenuItem;
		pbVU: TBGRAVirtualScreen;
		pnlControls: TPanel;
		pnlGraph: TPanel;
		pnlEffects: TPanel;
		bEffect0: TDecksButton;
		bEffect1: TDecksButton;
		bEffect2: TDecksButton;
		bEffect3: TDecksButton;
		bEffect4: TDecksButton;
		SliderFxParam0: ThKnob;
		SliderFxParam1: ThKnob;
		SliderFxParam2: ThKnob;
		SliderFxParam3: ThKnob;
		SliderFxParam4: ThKnob;
		SliderFxParam5: ThKnob;
		bEffect5: TDecksButton;
		pnlEffectKnobs: TPanel;
		PopupEffectPresets: TPopupMenu;
		miSetMasterTempo: TMenuItem;
		MenuItem2: TMenuItem;
		procedure bBendUpMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure bBendUpMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure bLoopBeatClick(Sender: TObject);
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
		procedure SliderTempoFracMouseWheelDown(Sender: TObject; Shift: TShiftState;
			MousePos: TPoint; var Handled: Boolean);
		procedure SliderTempoFracMouseWheelUp(Sender: TObject; Shift: TShiftState;
			MousePos: TPoint; var Handled: Boolean);
		procedure miZoneKind0Click(Sender: TObject);
		procedure SliderFxParam0Change(Sender: TObject);
		procedure bEffect0Click(Sender: TObject);
		procedure miApplyPresetClick(Sender: TObject);
		procedure bEffect0MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure SliderFxParam0MouseEnter(Sender: TObject);
		procedure SliderFxParam0MouseLeave(Sender: TObject);
		procedure pnlEffectsResize(Sender: TObject);
		procedure miSetMasterTempoClick(Sender: TObject);
	private
		DragWave: TDragInfo;
		GraphDragging: Boolean;
		SampleZoom: Integer;
		WaveformStep: Word;
		Zoner,
		Ruler: TBGRABitmap;
		ShowRemainingTime: Boolean;
		ZoneTextStyle: TTextStyle;

		procedure InitDevice(Dev: Integer);
		procedure ZoneChangedMessage(var Msg: TLMessage); message WM_ZONE;
		procedure ZoneChanged(Zone: Integer; FromCallback, MixTime: Boolean);
		procedure GetPlayPosition; inline;
		procedure SetSlider(var Slider: ThGaugeBar; Position: Integer); overload;
		procedure SetSlider(var Slider: ThKnob; Position: Integer); overload;
		procedure ZoomSample(Dir: Integer);
		procedure ZoomGraph(Dir: Integer);
		procedure UpdatePlayButton(Kind: Integer);
		procedure SelectEffect(EffectNum: Integer);
		procedure AddGUIEffect(FxObject: TBaseEffect; BtnObject: TDecksButton);
		procedure InitGUIEffectParam(Index: Byte; AKnob: ThKnob);
		function  GetEffectKnob(Sender: TObject): ThKnob;
		procedure ResizeEffectButtons;
		procedure OnLoadProgress(Percentage: Integer);
	public
		GraphHover,
		GraphCue:   TPoint;
		CuePos, // in graph sample coords
		PlayPosition: QWord;
		CurrentZone: Word;
		CurrentDevice: Integer;
		IsShiftDown,
		CanCreateNewZone: Boolean;

		Effects: TEffectsList;
		GUIEffectParams: array[0..5] of TGUIEffectParam;
		SelectedEffect: Byte;

		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

		procedure OnZoneChanged(Zone: Integer; MixTime: Boolean);
		procedure OnDeckEvent(Kind: Integer);
		procedure OnGraphIteration;

		procedure DoInit; override;
		procedure BeginFormUpdate;
		procedure EndFormUpdate;

		function  ProcessKeyUp(var Key: Word; Shift: TShiftState): Boolean;
		function  ProcessKeyDown(var Key: Word; Shift: TShiftState): Boolean;
		function  ProcessKeyPress(var Key: Char): Boolean;

		procedure SetZoneKind(Zone: Word; Kind: TZoneKind);

		procedure ShowPosition;
		procedure SyncToOtherDeck(Immediate: Boolean);
		procedure Cue(DoCue: Boolean);
		procedure SetCue(P: TPoint); overload;
		procedure SetCue(P: QWord); overload;
		procedure AfterPosJump(Data: PtrInt);
		procedure JumpToPos(Pos: QWord; Reset: Boolean = False);
		procedure JumpToCue(FromCallback: Boolean = False);
		procedure JumpToZone(Zone: Word);
		procedure JumpToBar(Bar: Word);

		procedure DrawWaveform;
		procedure DrawZones(Recalc: Boolean = True);
		procedure DrawRuler(Recalc: Boolean = True);
		procedure DrawGraph;
		procedure RedrawGraph;

		procedure ShowPanel_Effects;
	end;

var
	DeckFrame: TDeckFrame;

implementation

{$R *.lfm}

uses
	Math, FileUtil, BCButton,
	MouseWheelAccelerator,
	Form.Main,
	Decks.Config, Decks.Song;

procedure Audio_Callback_Play(handle: HSYNC; channel, data: DWord; user: Pointer);
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
	DeckFrame: TDeckFrame;
begin
	DeckFrame := TDeckFrame(user);
	if (DeckFrame = nil) or (DeckFrame.Deck = nil) then Exit;

	if DeckFrame.Deck.Paused then
		DeckFrame.Deck.Play
	else
		DeckFrame.JumpToCue(True);
end;

{ TDeckFrame }

procedure TDeckFrame.InitDevice(Dev: Integer);
var
//	Dev: Integer;
	OK: Boolean;
	S: String;
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
	end;
end;

procedure TDeckFrame.SetSlider(var Slider: ThGaugeBar; Position: Integer);
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

procedure TDeckFrame.ZoneChangedMessage(var Msg: TLMessage);
begin
	ZoneChanged(Msg.lParam, True, False);
end;

procedure TDeckFrame.OnZoneChanged(Zone: Integer; MixTime: Boolean);
begin
	//PostMessage(Self.Handle, WM_ZONE, Zone, Zone);
	ZoneChanged(Zone, True, MixTime);
end;

procedure TDeckFrame.ZoneChanged(Zone: Integer; FromCallback, MixTime: Boolean);
var
	Z: TZone;
	Buf: Single;
begin
	if Zone = ZONECHANGE_STOPPED then
	begin
		Deck.Stop;
//		SetCue(TPoint.Zero); // crashes on Qt5
		GraphCue := TPoint.Zero;
		CuePos := Deck.Graph.GraphToPos(GraphCue, True);
		SliderGraphX.Position := 0;
		Exit;
	end;

	if Deck.Graph.Zones.Count < 1 then Exit;

	if Zone = ZONECHANGE_GETPOS then
	begin
		GetPlayPosition;
		Zone := Deck.Graph.GetZoneIndexAt(Deck.Graph.SongToGraphBytes(PlayPosition));
		Deck.PlayingZone := Zone;
	end;

	Z := Deck.Graph.Zones[Zone];

	if FromCallback then
	begin
		if Zone = Deck.PlayingZone then Exit;
		Deck.PlayingZone := Zone;
		if MixTime then
		begin
			if Z.Kind <> zkLoop then
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
				zkLoop:	Deck.LoopZone(Zone);
			end;
		end;
	end
	else
	if not MixTime then
	begin
		SetSlider(SliderTempo, Trunc(Z.BPM));
		SetSlider(SliderTempoFrac, Trunc(Frac(Z.BPM) * 1000));
	end;

	Deck.SetBPM(MasterBPM);
	if not MixTime then
		DrawZones(True);
end;

procedure TDeckFrame.DoInit;
var
	Dev: TAudioDevice;
	mi: TMenuItem;
begin
	Deck.Graph.WantedZoom := 1;
	SampleZoom := 2;

	Deck.Graph.OnGraphIteration := OnGraphIteration;
	Deck.Graph.OnZoneChange := OnZoneChanged;
	Deck.Graph.OnLoadProgress := OnLoadProgress;
	Deck.OnModeChange := OnDeckEvent;
	Deck.Index := DeckList.Add(Deck) + 1;

	Tag := Deck.Index;
	lTime.Tag := -1;
	GraphHover := Point(-1, -1);

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

	Show;
	ShowPosition;

	Timer.Enabled := True;
end;

procedure TDeckFrame.SliderTempoChange(Sender: TObject);
begin
	// fix choppiness of slider movement
	if Assigned(Sender) then
		(Sender as TControl).Repaint;

	Deck.SetBPM(MasterBPM);
	Deck.Graph.ZonesChanged;
	RedrawGraph;
	DrawWaveform;
end;

procedure TDeckFrame.GetPlayPosition;
begin
	PlayPosition := Deck.GetPlayPosition;
end;

procedure TDeckFrame.ShowPosition;
var
	time_e, time_r, tm, ts, X, Y, W, H: Integer;
	se, sr: String;
	Vol: DWord = 0;
begin
	if not Deck.Loaded then
	begin
		if lTime.Tag >= 0 then Exit;
		time_e := 0;
	end
	else
	begin
		if Deck.Graph.Zones.Count < 1 then Exit;
		GetPlayPosition;
		time_e := Max(0, Round(BASS_ChannelBytes2Seconds(Deck.OrigStream, PlayPosition)));
	end;

	if time_e <> lTime.Tag then
	begin
		time_r := Round(BASS_ChannelBytes2Seconds(Deck.OrigStream, Deck.ByteLength - PlayPosition));

		tm := time_e div 60;
		ts := time_e - (tm * 60);
		se := Format('%d:%.2d', [tm, ts]);
		tm := time_r div 60;
		ts := time_r - (tm * 60);
		sr := Format('-%d:%.2d', [tm, ts]);

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

	bPlay.StateNormal.Border.Color := Grays[Deck.BeatFadeCounter];

	with pbVU do
	begin
		W := ClientWidth; H := ClientHeight;
		Bitmap.FillRect(Bounds(0, 0, W, H), BGRABlack, dmSet);
		if not Deck.Paused then
			Vol := BASS_ChannelGetLevel(Deck.Stream);
		if Vol > 0 then
		begin
			{ // vertical
			W := 2;
			Y := H - Trunc((H * ((Vol and $FFFF) / 32767)));
			Bitmap.FillRect(Bounds(0, Y, W, H), BGRAWhite, dmSet);

			Y := H - Trunc((H * ((Vol shr 16) / 32767)));
			Bitmap.FillRect(Bounds(W, Y, W+2, H), BGRAWhite, dmSet);
			}
			// horizontal
			H := H div 2;
			X := Trunc((W * ((Vol and $FFFF) / 32767)));
			Bitmap.FillRect(Bounds(0, 0, X, H), BGRAWhite, dmSet);

			X := Trunc((W * ((Vol shr 16) / 32767)));
			Bitmap.FillRect(Bounds(0, H, X, H+H), BGRAWhite, dmSet);
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

procedure TDeckFrame.bLoopBeatClick(Sender: TObject);
var
	K, L: Integer;
	Btn: TDecksButton;
begin
	Btn := (Sender as TDecksButton);
	L := Btn.Tag;
	Btn.Down := not Btn.Down;
	Btn.StateNormal.Border.LightWidth := IfThen(Btn.Down, 1, 0);

	case Btn.Tag of
		1..3: K := LOOP_BEATS;
		4..999: begin K := LOOP_BARS; L := Btn.Tag div 4; end;
		   0: begin K := LOOP_ZONE; L := 1; end;
		  -1: begin K := LOOP_SONG; L := 1; end;
		else
			Exit;
	end;

	if Btn.Down then
		Deck.SetLoop(K, L)
	else
		Deck.SetLoop(K, LOOP_OFF);
end;

procedure TDeckFrame.bMasterMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	SetMaster(Tag);
end;

procedure TDeckFrame.bReverseMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	Deck.SetReverse(True, Button = mbLeft);
end;

procedure TDeckFrame.bReverseMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
var
	B: Boolean;
begin
	B := Button = mbLeft;
	Deck.SetReverse(False, True);
	if B then SyncToOtherDeck(True);
end;

procedure TDeckFrame.bStoreClick(Sender: TObject);
begin
	Deck.SaveInfoFile;
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

procedure TDeckFrame.bSyncMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	case Button of
		mbLeft:
		begin
			Deck.Synced := not Deck.Synced;
			bSync.StateNormal.Border.LightWidth := IfThen(Deck.Synced, 1, 0);
		end;

		mbRight:
			SyncToOtherDeck(True);
	end;
end;

procedure TDeckFrame.bPlayClick(Sender: TObject);
begin
	if not Enabled then Exit;

	if Deck.Cueing then
	begin
		Deck.Cueing := False;
		UpdatePlayButton(MODE_PLAY_START);
		Exit;
	end;
	BASS_SetDevice(CurrentDevice);

	// jump to cue if song has been played through
	GetPlayPosition;
	if PlayPosition >= BASS_ChannelGetLength(Deck.OrigStream, BASS_POS_BYTE) then
		JumpToCue;

	if (Deck.Synced) and (Deck.Paused) then
		SyncToOtherDeck(False)
	else
		Deck.Pause;
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
	P: QWord;
	B: Cardinal;
	PT, CT: TPoint;
	OtherDeck: TDeck;
begin
	if not Enabled then Exit;

	OtherDeck := Deck.GetOtherOrCurrentDeck;
	if OtherDeck = nil then Exit;

	if Immediate then
	begin
		CT := Deck.Graph.PosToGraph(Deck.GetPlayPosition(True), False);
		BASS_SetDevice(CurrentDevice);
		PT := OtherDeck.Graph.PosToGraph(OtherDeck.GetPlayPosition(False), False);
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
		B := OtherDeck.GetCurrentBar+1;
		P := OtherDeck.Graph.Bars[B].Pos;
		P := OtherDeck.Graph.GraphToSongBytes(P);
		if Deck.Paused then JumpToCue;
		bPlay.StateNormal.Border.LightColor := $0022AAFF;
		bPlay.StateNormal.Border.LightWidth := 2;
		BASS_ChannelSetSync(OtherDeck.OrigStream,
			BASS_SYNC_POS or BASS_SYNC_MIXTIME or BASS_SYNC_ONETIME, P,
			@Audio_Callback_Play, Self);
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
	if not Enabled then Exit;
	RedrawGraph;
	DrawWaveform;
end;

procedure TDeckFrame.lTimeClick(Sender: TObject);
begin
	if not Enabled then Exit;
	ShowRemainingTime := not ShowRemainingTime;
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
var
	Lbl: TLabel;
begin
//	AKnob.Height := AKnob.Height - 3;

	Lbl := TLabel.Create(AKnob.Parent);
	Lbl.AutoSize := False;
	Lbl.Transparent := True;
	Lbl.SetBounds(AKnob.Left-15, 0, AKnob.Width+30-1, 20);
	Lbl.Alignment := taCenter;
	Lbl.Font.Color := clWhite;
	Lbl.Parent := AKnob.Parent;

	with GUIEffectParams[Index] do
	begin
		Knob := AKnob;
		ValueLabel := Lbl;
		NameLabel  := Lbl;
		Knob.PositionLabel := Lbl;
		Knob.OnMouseEnter := SliderFxParam0MouseEnter;
		Knob.OnMouseLeave := SliderFxParam0MouseLeave;
	end;
end;

procedure TDeckFrame.AddGUIEffect(FxObject: TBaseEffect; BtnObject: TDecksButton);
var
	Fx: TGUIEffect;
begin
	Fx := TGUIEffect.Create;
	Fx.Effect := FxObject;
	Fx.Button := BtnObject;
	BtnObject.OnClick := nil;
	BtnObject.OnButtonClick := bEffect0Click;
	BtnObject.OnMouseDown := bEffect0MouseDown;
	BtnObject.DropDownStyle := bdsCommon;
	if FxObject <> nil then
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

	Effects := TEffectsList.Create(True);
	SelectedEffect := 255;

	AddGUIEffect(TFxEcho.Create,       bEffect0);
	AddGUIEffect(TFxReverb.Create,     bEffect1);
	AddGUIEffect(TFxPhaser.Create,     bEffect2);
	AddGUIEffect(TFxChorus.Create,     bEffect3);
//	AddGUIEffect(TFxDistortion.Create, bEffect4);
	AddGUIEffect(TFxCompressor.Create, bEffect4);
	AddGUIEffect(nil, bEffect5);

	InitGUIEffectParam(0, SliderFxParam0);
	InitGUIEffectParam(1, SliderFxParam1);
	InitGUIEffectParam(2, SliderFxParam2);
	InitGUIEffectParam(3, SliderFxParam3);
	InitGUIEffectParam(4, SliderFxParam4);
	InitGUIEffectParam(5, SliderFxParam5);

	SelectEffect(Effects.Count-1);
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

function TDeckFrame.ProcessKeyUp(var Key: Word; Shift: TShiftState): Boolean;
begin
	Result := True;
	if not Enabled then Exit;

	case Key of

		VK_CONTROL:
			bPlay.OnMouseUp(Self, mbRight, Shift, 0, 0);

		VK_SHIFT:
			IsShiftDown := False;

		VK_ADD, VK_SUBTRACT,
		VK_UP,  VK_DOWN:
			Deck.BendStop;
	else
		Result := False;
	end;
end;

function TDeckFrame.ProcessKeyDown(var Key: Word; Shift: TShiftState): Boolean;
begin
	Result := True;
	if not Enabled then Exit;

	case Key of

		VK_SPACE:		bPlay.OnClick(Self);

		VK_CONTROL:		bPlay.OnMouseDown(Self, mbRight, Shift, 0, 0);

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

function TDeckFrame.ProcessKeyPress(var Key: Char): Boolean;
var
	Z: TZone;
begin
	Result := True;
	if not Enabled then Exit;
	case Key of

		'+':
		begin
			Z := Deck.Graph.AddZone(Deck.Graph.GraphToBar(GraphCue.x));
			if Z <> nil then
			begin
				CurrentZone := Deck.Graph.Zones.IndexOf(Z);
				RedrawGraph;
			end;
		end;

		'-':
		if Deck.Graph.RemoveZone(CurrentZone) then
		begin
			if CurrentZone > 0 then Dec(CurrentZone);
			RedrawGraph;
		end;

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
end;

// input in graph pixel coords
procedure TDeckFrame.SetCue(P: TPoint);
begin
	if not Enabled then Exit;
	GraphCue := P;
	CuePos := Deck.Graph.GraphToPos(P, True);
	DrawWaveform;
	DrawGraph;
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
	if Deck.Graph.Zones[Zone].Kind = zkLoop then
		Deck.LoopZone(Zone); // reapply loop, dumb
	ShowPosition;
end;

procedure TDeckFrame.JumpToPos(Pos: QWord; Reset: Boolean = False);
var
	Flags: DWord = BASS_POS_BYTE;
begin
	if not Enabled then Exit;
	if Reset then
		Flags := Flags or BASS_POS_MIXER_RESET;
	BASS_Mixer_ChannelSetPosition(Deck.OrigStream, Pos, Flags);
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

procedure TDeckFrame.pbMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Enabled then
	case Button of

		mbLeft:
		begin
			GraphDragging := True;
			pbMouseMove(Sender, Shift, X, Y);
			SetMaster(Tag);
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
		RedrawGraph;

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
		RedrawGraph;
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

procedure TDeckFrame.pbRulerRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
	DrawRuler;
end;

procedure TDeckFrame.pbWaveMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Enabled then
	case Button of
		mbLeft:
		begin
			SetMaster(Tag);
			SetCaptureControl(pbWave);
			DragWave.Dragging := True;
			DragWave.Offset := X;
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
	DragWave.Dragging := False;
	SetCaptureControl(nil);
end;

procedure TDeckFrame.pbWaveMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
	if (not Enabled) or (not DragWave.Dragging) then Exit;

	SetCue(Max(0, CuePos - ((DragWave.Offset - X) * WaveformStep)));
	DragWave.Offset := X;
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

	SetMaster(Tag);
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
				end;
				Zoner.FillRect(R, ColorToBGRA(Col));
				Col := clWhite;
			end;

			if Z = Deck.PlayingZone then
			begin
				Zoner.HorizLine(R.Left+2, R.Bottom-2, R.Right-2, BGRA(0, 100, 70));
				//Zoner.RaiseRectTS(R, +30);
			end;

			Zoner.TextRect(R, X1+2, 2, Format('%.3f', [Zone.BPM]), ZoneTextStyle, Col);

			X2 := X1 - 1;
		end;
	end;

	pbZones.Bitmap.PutImage(-Deck.Graph.Scroll.X, 0, Zoner, dmSet);
	pbZones.Repaint;
end;

procedure TDeckFrame.DrawRuler(Recalc: Boolean = True);
const
	Col: array[0..3] of TColor = (clWhite, clSilver, clGray, clSilver);
var
	C, A, X, Z, H, Gap: Integer;
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

			repeat
				for A := 0 to 3 do
				begin
					Ruler.FillRect(X, 1, X+Z-Gap, H-1, ColorToBGRA(Col[C mod 4]));
					Inc(X, Z);
				end;
				Inc(C); // color index
			until X >= Ruler.Width;
		end;
	end;

	pbRuler.Bitmap.PutImage(-Deck.Graph.Scroll.X, 0, Ruler, dmSet);
	pbRuler.Repaint;
end;

procedure TDeckFrame.DrawWaveform;
var
	X, L, W: Cardinal;
	Sam, FAdd, Y: Integer;
	Sam2: QWord;
	Sample: PByte;
begin
	if not Enabled then Exit;

	L := Deck.Graph.GetBarLength(True , Deck.Graph.GraphToBar(GraphCue.X) )
		* 2 div Trunc(Power(2, SampleZoom));
	if L < 10 then Exit;

	W := pbWave.ClientWidth;
	FAdd := Round(L / W);
	WaveformStep := FAdd;

	CuePos := Min(CuePos, Deck.Graph.length_audio - L);

	Sample := @Deck.Graph.AudioData[CuePos];

	pbWave.Bitmap.Fill(BGRABlack);

	if FAdd > 0 then
	for X := 0 to W-1 do
	begin
		Sam2 := 0;
//		Sam := 0;
		for Y := 1 to FAdd do
		begin
//			if Sample^ > Sam then Sam := Sample^;
			Sam2 += Sample^;
			Inc(Sample);
		end;
		Sam := Min(255, Trunc(Sam2 / FAdd * Deck.Graph.Brightness));
//		Sam := Min(255, Trunc(Sam * Deck.Graph.Brightness));
		Y := Sam div 8;
		pbWave.Bitmap.VertLine(X, 31-Y, 31+Y, Grays[Sam]);
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
	P, PE: TPoint;
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

	// playback position indicator
	//
	P := Deck.Graph.PosToGraph(PlayPosition, False);

	Y := Trunc(P.Y / H * 4); // quadrant
	if Y <> Deck.BeatPreviousQuadrant then
	begin
		Deck.BeatFadeCounter := 255;
		Deck.BeatPreviousQuadrant := Y;
	end;

	X := P.X - ScrollX;
	Y := P.Y;
	R.TopLeft := Point(X, 0);
	R.BottomRight := Point(X+Z, H);
	pb.Bitmap.FillRect(R, BGRA(255, 100, 30, 130), dmLinearBlend);
	pb.Bitmap.HorizLine(X, Y, X+Z-1, ColorToBGRA(clYellow));

	// loop range indicator
	//
	if Deck.LoopInfo_Misc.Enabled then
	begin
		P  := Deck.Graph.PosToGraph(Deck.LoopInfo_Misc.StartPos - Deck.Graph.StartPos, False);
		PE := Deck.Graph.PosToGraph(Deck.LoopInfo_Misc.EndPos   - Deck.Graph.StartPos, False);
		P.X := P.X - ScrollX;
		PE.X := PE.X - ScrollX;
		R.TopLeft := Point(P.X, P.Y);
		if PE.Y > 0 then
			R.BottomRight := Point(PE.X+Z, PE.Y)
		else
			R.BottomRight := Point(P.X+Z, H);
		pb.Bitmap.FillRect(R, BGRA(255, 255, 255, 130), dmLinearBlend);
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

procedure TDeckFrame.RedrawGraph;
var
	BPM: Single;
begin
	if not Enabled then Exit;

	BPM := SliderTempo.Position + (SliderTempoFrac.Position / 1000);

	if CurrentZone = 0 then
		Deck.OrigBPM := BPM;
	if Deck.Graph.Zones.Count > 0 then
	begin
		Deck.Graph.Zones[CurrentZone].BPM := BPM;
		Deck.Graph.NeedRecalc := True;
	end;

	Deck.BPM := MasterBPM;
	Deck.Info.Amp := SliderAmp.Position / 100;
	Deck.Graph.Brightness := Deck.Graph.CalcBrightness * Deck.Info.Amp;
	Deck.SetVolume(-1); // update volume

	Deck.Graph.Draw(pb.ClientWidth-1, pb.ClientHeight-1);
	DrawRuler;
	DrawZones;
	DrawGraph;

	with SliderGraphX do
	begin
		OnChange := nil;
		Range := Deck.Graph.Width;
		Window := pb.ClientWidth;
		OnChange := SliderGraphXChange;
	end;
end;

procedure TDeckFrame.UpdatePlayButton(Kind: Integer);
begin
	case Kind of

		MODE_PLAY_START:
		begin
			bPlay.StateNormal.Border.LightWidth := IfThen(Deck.Cueing, 1, 2);
			bPlay.StateNormal.Border.LightColor := $0035D95F;
			if not Deck.Cueing then MainForm.MarkSongAsPlayed(Deck.Filename);
		end;

		MODE_PLAY_STOP,
		MODE_PLAY_PAUSE:
		begin
			bPlay.StateNormal.Border.LightWidth := 0;
			bPlay.Down := False;
		end;

		MODE_PLAY_FAILURE:
		begin
			bPlay.Color := clRed;
			bPlay.Down := False;
			ShowMessage('Failed to start playback!');
		end;

	end;
	MainForm.UpdateController(Deck, Kind);
end;

procedure TDeckFrame.OnLoadProgress(Percentage: Integer);
var
	X, W, H: Integer;
begin
	with pbVU do
	begin
		W := ClientWidth; H := ClientHeight;
		Bitmap.FillRect(Bounds(0, 0, W, H), BGRABlack, dmSet);
		if Percentage > 0 then
		begin
			X := Trunc(W / 100 * Percentage);
			Bitmap.FillRect(Bounds(0, 0, X, H), Grays[Trunc(255 / 100 * Percentage)], dmSet);
		end;
		Repaint;
	end;
	Application.ProcessMessages;
end;

procedure TDeckFrame.OnDeckEvent(Kind: Integer);
var
	S: String;
begin
	case Kind of

		MODE_LOAD_START:
		begin
			BASS_SetDevice(CurrentDevice);
			CurrentZone := 0;
			Deck.Graph.Zones.Clear;
			Deck.Graph.Scroll.X := 0;
			Deck.Graph.Zoom := 1;
			Timer.Enabled := False;
			bMaster.Caption := 'Loading';
			//Screen.Cursor := crHourGlass;
			Invalidate;
			Application.ProcessMessages;
			Enabled := False;
		end;

		MODE_LOAD_FINISH:
		begin
			Enabled := True;

			RedrawGraph;
			SetCue(TPoint.Zero);
			DrawWaveform;
			SliderGraphX.Position := 0;

			//Screen.Cursor := crDefault;
			lTime.Tag := -1;
			Invalidate;
			Timer.Enabled := True;

			MainForm.ApplyMixer;
		end;

		MODE_LOAD_GRAPH:
		begin
			Deck.Graph.Generate;
			SetSlider(SliderAmp, 100);
			Deck.Graph.BitmapSize := Point(pb.ClientWidth, pb.ClientHeight);
		end;

		MODE_LOAD_SUCCESS:
		begin
			S := ExtractFileName(Deck.Filename);
			bMaster.Caption := Format('%d: %s', [Deck.Index, S]);

			Deck.Info.BPM := MasterBPM;
			if Deck.GetInfo then
			begin
			end
			else
			begin
				Deck.Graph.Clear;
				Deck.Graph.AddZone(0, MasterBPM, True);
			end;

			if Deck.Info.BPM > 1 then
			begin
				SetSlider(SliderTempo, Trunc(Deck.Info.BPM));
				SetSlider(SliderTempoFrac, Trunc(Frac(Deck.Info.BPM) * 1000));
			end;

			SetSlider(SliderAmp, Trunc(Deck.Info.Amp * 100));

			Deck.Graph.ZonesLoaded;
			SetCue(TPoint.Zero);
			SliderTempoChange(nil);
			JumpToCue;
			SliderTempoChange(nil); // fixme: need to call this twice
			ZoneChanged(0, False, False);
		end;

		MODE_LOAD_FAILURE:
		begin
			Caption := '';
			S := '';
			case BASS_ErrorGetCode of
				BASS_ERROR_FILEOPEN:	S := 'The file could not be opened.';
				BASS_ERROR_FILEFORM:	S := 'File format not recognised/supported.';
				BASS_ERROR_NOTAUDIO:	S := 'The file does not contain audio.';
				BASS_ERROR_CODEC:		S := 'Codec not available/supported.';
				BASS_ERROR_FORMAT:		S := 'Sample format not supported.';
				BASS_ERROR_MEM:			S := 'Insufficient memory.';
				BASS_ERROR_UNKNOWN:		S := 'Mystery problem!';
			end;
			ShowMessage('Failed to open file: ' + S);
		end;

		MODE_PLAY_START, MODE_PLAY_STOP, MODE_PLAY_PAUSE, MODE_PLAY_FAILURE:
			UpdatePlayButton(Kind);

		//MODE_TEMPOCHANGE: ShowPosition;

		MODE_EQ_KILL_ON, MODE_EQ_KILL_OFF:
			MainForm.UpdateController(Deck, Kind);

	end;
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
		RedrawGraph;
	end;
end;

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

procedure TDeckFrame.ShowPanel_Effects;
var
	B: Boolean;
begin
	B := MainForm.bToggleEffects.Down;
	pnlEffects.Top := pnlControls.Top - pnlEffects.Height;
	pnlEffects.Visible := B;
end;

procedure TDeckFrame.BeginFormUpdate;
begin
	MainForm.StartUpdate;
end;

procedure TDeckFrame.EndFormUpdate;
begin
	MainForm.EndUpdate;
end;

procedure TDeckFrame.SelectEffect(EffectNum: Integer);
var
	Fx: TBaseEffect;
	Param: TEffectParam;
	GUIParam: TGUIEffectParam;
	Knob: ThKnob;
	Button: TDecksButton;
	MI: TMenuItem;
	i, M: Integer;
	B: Boolean;
begin
	if SelectedEffect = EffectNum then Exit;
	BeginFormUpdate;

	SelectedEffect := EffectNum;

	for i := 0 to Effects.Count-1 do
	begin
		B := (i = EffectNum);
		Button := Effects[i].Button;
		if B then
		begin
			Button.StateNormal.Background.Color := clPurple;
			if Button.HelpContext = 0 then
			begin
				Button.DropDownMenu := PopupEffectPresets;
				Button.Style := bbtDropDown;
			end;
		end
		else
		begin
			Button.StateNormal.Background.Color := clTeal;
			Button.DropDownMenu := nil;
			Button.Style := bbtButton;
		end;
	end;

	PopupEffectPresets.Items.Clear;

	Fx := Effects[SelectedEffect].Effect;
	B := Fx <> nil;
	pnlEffectKnobs.Visible := B;
	if B then
	begin
		Button := Effects[SelectedEffect].Button;
		Button.StateNormal.Border.LightWidth := IfThen(Fx.Enabled, 2, 0);
		//pnlEffectKnobs.Tag := 0;

		for i := 0 to High(GUIEffectParams) do
		begin
			GUIParam := GUIEffectParams[i];

			Knob := GUIParam.Knob;
			B := (i < Fx.Params.Count);
			Knob.OnChange := nil;
			Knob.Visible := B;
			if B then
			begin
				//pnlEffectKnobs.Tag := pnlEffectKnobs.Tag + 48;
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

	ResizeEffectButtons;
	EndFormUpdate;
end;

procedure TDeckFrame.ResizeEffectButtons;
var
	i, X, W: Integer;
	Button: TDecksButton;
begin
	BeginFormUpdate;

	X := 8;
	W := Min(180, (pnlEffects.ClientWidth - 16 - 300) div 3);

	for i := 0 to Effects.Count-1 do
	begin
		Button := Effects[i].Button;
		Button.Left := X;
		Button.Width := W;
		if (i mod 2) = 1 then Inc(X, W);
	end;

	Inc(X, 8);
	pnlEffectKnobs.SetBounds(X, 3, pnlEffects.ClientWidth - X, pnlEffects.ClientHeight-4);

	EndFormUpdate;
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
		Param := Effects[SelectedEffect].Effect.Params[Knob.Tag];
		Param.Value := Knob.FloatPosition;
	end;
end;

procedure TDeckFrame.SliderFxParam0MouseEnter(Sender: TObject);
var
	Knob: ThKnob;
begin
	Knob := GetEffectKnob(Sender);
	if Knob <> nil then
	begin
		Knob.ShowPosition;
		//GUIEffectParams[Knob.Tag].NameLabel.Caption :=
		//Knob.PositionLabel.Caption := '%f';
			//Format('%f', [Knob.FloatPosition]);
	end;
end;

procedure TDeckFrame.SliderFxParam0MouseLeave(Sender: TObject);
var
	Knob: ThKnob;
begin
	Knob := GetEffectKnob(Sender);
	if Knob <> nil then
	begin
		//GUIEffectParams[Knob.Tag].NameLabel.Caption :=
		Knob.PositionLabel.Caption :=
			Effects[SelectedEffect].Effect.Params[Knob.Tag].Name;
	end;
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

procedure TDeckFrame.bEffect0MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
var
	Fx: TBaseEffect;
	B: Boolean;
begin
	if Button = mbRight then
	begin
		if not (Sender is TDecksButton) then Exit;
		X := (Sender as TDecksButton).Tag;
		Fx := Effects[X].Effect;
		if Fx = nil then Exit;
		B := not Fx.Enabled;
		if B then
			Fx.Stream := Deck.Stream;
		Effects[X].Button.StateNormal.Border.LightWidth := IfThen(B, 2, 0);
		Fx.Enabled := B;
	end;
end;

procedure TDeckFrame.pnlEffectsResize(Sender: TObject);
begin
	ResizeEffectButtons;
end;

procedure TDeckFrame.miSetMasterTempoClick(Sender: TObject);
begin
	MainForm.SetMasterTempo(Deck.OrigBPM);
end;


end.

