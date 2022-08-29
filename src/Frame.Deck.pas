unit Frame.Deck;

{$mode Delphi}
{$INLINE ON}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
	Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, StdCtrls,
	ExtCtrls, EditBtn, Buttons, LCLType, LCLIntf, LMessages, Menus,
	BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
	Decks.Audio, Decks.Deck, Decks.Beatgraph, Decks.SongInfo, BASS, BASSmix,
	hKnob, hSlider, DecksButton, BCTypes;

const
	BPMStep = 0.01;

	WM_ZONE = LM_USER + 2010;

type
	TDragInfo = record
		Dragging: Boolean;
		Offset:   Integer;
	end;

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
		DecksButton1: TDecksButton;
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
		shpBorder: TShape;
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
	public
		GraphHover,
		GraphCue:   TPoint;
		CuePos, // in graph sample coords
		PlayPosition: QWord;
		CurrentZone: Word;
		CurrentDevice: Integer;
		IsShiftDown,
		CanCreateNewZone: Boolean;

		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

		procedure OnZoneChanged(Zone: Integer; MixTime: Boolean);
		procedure OnDeckEvent(Kind: Integer);
		procedure OnGraphIteration;

		procedure DoInit; override;

		function  ProcessKeyUp(var Key: Word; Shift: TShiftState): Boolean;
		function  ProcessKeyDown(var Key: Word; Shift: TShiftState): Boolean;
		function  ProcessKeyPress(var Key: Char): Boolean;

		procedure SetZoneKind(Zone: Word; Kind: TZoneKind);

		procedure ShowPosition;
		procedure SyncToOtherDeck(Immediate: Boolean);
		procedure Cue(DoCue: Boolean);
		procedure SetCue(P: TPoint); overload;
		procedure SetCue(P: QWord); overload;
		procedure JumpToPos(Pos: QWord; Reset: Boolean = False);
		procedure JumpToCue;
		procedure JumpToZone(Zone: Word);
		procedure JumpToBar(Bar: Word);

		procedure DrawWaveform;
		procedure DrawZones(Recalc: Boolean = True);
		procedure DrawRuler(Recalc: Boolean = True);
		procedure DrawGraph;
		procedure RedrawGraph;
	end;

var
	DeckFrame: TDeckFrame;

implementation

{$R *.lfm}

uses
	Math, FileUtil,
	MouseWheelAccelerator,
	Form.Main,
	Decks.Config, Decks.Song;

procedure Audio_Callback_Play(handle: HSYNC; channel, data: DWord; user: Pointer);
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
	TDeck(user^).Play;
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
			{if (Deck.OtherDeck <> nil) and (Deck.OtherDeck.Queued) then
			begin
				BASS_ChannelGetAttribute(Deck.Stream, BASS_ATTRIB_BUFFER, Buf);
				BASS_Mixer_ChannelSetPosition(Deck.OtherDeck.Stream,
					BASS_ChannelGetPosition(Deck.OtherDeck.Stream, BASS_POS_BYTE) -
					BASS_ChannelSeconds2Bytes(Deck.OtherDeck.Stream, Buf),
				BASS_POS_BYTE);
				Deck.OtherDeck.Queued := False;
				Deck.OtherDeck.Play;
			end;}
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
		mi.Checked := (mi.Tag = Config.Audio.Device[Deck.Index] {AudioManager.DefaultDeviceIndex});
		mi.OnClick := cmbDevicesChange;
		miAudioDevices.Add(mi);
	end;
	InitDevice(Config.Audio.Device[Deck.Index]);
//	InitDevice(AudioManager.DefaultDeviceIndex - 1); // 0-based

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
	time_e, time_r, tm, ts, Y, W, H: Integer;
	se, sr: String;
	Vol: DWord;
begin
	if not Deck.Loaded then
	begin
		if lTime.Tag >= 0 then Exit;
		time_e := 0;
	end
	else
	begin
		if Deck.Graph.Zones.Count < 1 then Exit;
//		bMaster.Caption := Format('%.3f -> %.3f', [Deck.Graph.Zones[Deck.PlayingZone].BPM, Deck.BPM]);
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

	Vol := BASS_ChannelGetLevel(Deck.Stream);
	with pbVU do
	begin
		W := ClientWidth-1; H := ClientHeight;
		Bitmap.FillRect(Bounds(0, 0, W, H), BGRABlack, dmSet);

		if vol > 0 then
		begin
			W := 2;
			Y := H - Trunc((H * ((Vol and $FFFF) / 32767)));
			Bitmap.FillRect(Bounds(0, Y, W, H), BGRAWhite, dmSet);

			Y := H - Trunc((H * ((Vol shr 16) / 32767)));
			Bitmap.FillRect(Bounds(W, Y, W+2, H), BGRAWhite, dmSet);
		end;

		Refresh;
	end;

	DrawGraph;
end;

procedure TDeckFrame.TimerTimer(Sender: TObject);
begin
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
		SampleZoom := Min(4, SampleZoom+1)
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
	case Button of
		mbRight:  Cue(False);
	end;
end;

procedure TDeckFrame.SyncToOtherDeck(Immediate: Boolean);
var
	P: QWord;
	B: Cardinal;
	PT, CT: TPoint;
begin
	if Deck.OtherDeck = nil then Exit;

	if Immediate then
	begin
		CT := Deck.Graph.PosToGraph(Deck.GetPlayPosition(True), False);
		BASS_SetDevice(CurrentDevice);
		PT := Deck.OtherDeck.Graph.PosToGraph(Deck.OtherDeck.GetPlayPosition(False), False);
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
		B := Deck.OtherDeck.GetCurrentBar+1;
		P := Deck.OtherDeck.Graph.Bars[B].Pos;
		P := Deck.OtherDeck.Graph.GraphToSongBytes(P);
		JumpToCue;
		bPlay.StateNormal.Border.LightColor := $0022AAFF;
		bPlay.StateNormal.Border.LightWidth := 2;
		BASS_ChannelSetSync(Deck.OtherDeck.OrigStream,
			BASS_SYNC_POS or BASS_SYNC_MIXTIME or BASS_SYNC_ONETIME, P,
			@Audio_Callback_Play, @Deck);
	end;
end;

procedure TDeckFrame.Cue(DoCue: Boolean);
begin
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
	RedrawGraph;
end;

procedure TDeckFrame.lTimeClick(Sender: TObject);
begin
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

constructor TDeckFrame.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	CurrentZone := 0;

	with ZoneTextStyle do
	begin
		Alignment := taLeftJustify; // TAlignment
		Layout := tlCenter; // TTextLayout
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

//	pbRuler.Height := pbRuler.Height + 1;
//	pbZones.SetBounds(pbZones.Left, pbZones.Top-1,
//		pbZones.Width, pbZones.Height+1);

	//ConvertButtons(Self);
	LoadButtonImages(Self, Config.GetThemePath + 'images');
end;

destructor TDeckFrame.Destroy;
begin
	Ruler.Free;
	Zoner.Free;
	//Deck.Free; //owned by MainForm.DeckList

	inherited Destroy;
end;

function TDeckFrame.ProcessKeyUp(var Key: Word; Shift: TShiftState): Boolean;
begin
	Result := True;

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
	CuePos := P;
	GraphCue := Deck.Graph.PosToGraph(P, True);
	DrawWaveform;
	DrawGraph;
end;

// input in graph pixel coords
procedure TDeckFrame.SetCue(P: TPoint);
begin
	GraphCue := P;
	CuePos := Deck.Graph.GraphToPos(P, True);
	DrawWaveform;
	DrawGraph;
end;

procedure TDeckFrame.JumpToPos(Pos: QWord; Reset: Boolean = False);
var
	Flags: DWord = BASS_POS_BYTE;
begin
	if Reset then
		Flags := Flags or BASS_POS_MIXER_RESET;
	BASS_Mixer_ChannelSetPosition(Deck.OrigStream, Pos, Flags);
	ZoneChanged(ZONECHANGE_GETPOS, False, True);
	ShowPosition;
end;

procedure TDeckFrame.JumpToCue;
begin
	JumpToPos(Deck.Graph.GraphToSongBytes(CuePos), True);
end;

procedure TDeckFrame.JumpToZone(Zone: Word);
begin
	JumpToPos(Deck.Graph.GraphToSongBytes(Deck.Graph.Zones[Zone].Pos));
end;

procedure TDeckFrame.JumpToBar(Bar: Word);
begin
	JumpToPos(Deck.Graph.GraphToSongBytes(Deck.Graph.Bars[Bar].Pos));
end;

procedure TDeckFrame.pbMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	case Button of

		mbLeft:
		begin
			GraphDragging := True;
			pbMouseMove(Sender, Shift, X, Y);
			SetMaster(Tag);
		end;

		mbRight:
			JumpToCue;
	end;
end;

procedure TDeckFrame.pbMouseLeave(Sender: TObject);
begin
	GraphHover := Point(-1, -1);
	DrawGraph;
end;

procedure TDeckFrame.pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
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
	case Button of
		mbLeft:
		begin
			SetMaster(Tag);
			SetCaptureControl(pbWave);
			DragWave.Dragging := True;
			DragWave.Offset := X;
		end;

		mbRight:
			JumpToCue;
	end;
end;

procedure TDeckFrame.pbWaveMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	DragWave.Dragging := False;
	SetCaptureControl(nil);
end;

procedure TDeckFrame.pbWaveMouseWheel(Sender: TObject; Shift: TShiftState;
	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	Handled := True;

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
	if not Deck.Loaded then Exit;

	SetMaster(Tag);
	Y := Deck.Graph.GraphToBar(X + Deck.Graph.Scroll.X);
	CurrentZone := Deck.Graph.Bars[Y].Zone;
	//bMaster.caption := format('X=%d  B=%d  Z=%d', [X, Y, CurrentZone]);
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

procedure TDeckFrame.pbWaveMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
	if not DragWave.Dragging then Exit;

	SetCue(Max(0, CuePos - ((DragWave.Offset - X) * WaveformStep)));
	DragWave.Offset := X;
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
	pbZones.Refresh;
end;

procedure TDeckFrame.DrawRuler(Recalc: Boolean = True);
const
	Col: array[0..3] of TColor = (clWhite, clSilver, clGray, clSilver);
var
	C, A, X, Z, H, Gap: Integer;
begin
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
	pbRuler.Refresh;
end;

procedure TDeckFrame.DrawWaveform;
var
	X, L, W: Cardinal;
	Sam, FAdd, Y: Integer;
	Sample: PByte;
begin
	L := Deck.Graph.GetBarLength(True (*, Deck.Graph.GraphToBar(GraphCue.X)*) )
		* 2 div Trunc(Power(2, SampleZoom));
	if L < 10 then Exit;

	W := pbWave.ClientWidth;
	FAdd := Trunc(L / W);
	WaveformStep := FAdd;

	CuePos := Min(CuePos, Deck.Graph.length_audio - L);

	Sample := @Deck.Graph.AudioData[CuePos];

	pbWave.Bitmap.Fill(BGRABlack);

	for X := 0 to W-1 do
	begin
		Sam := 0;
		for Y := 1 to FAdd do
		begin
			if Sample^ > Sam then Sam := Sample^;
			Inc(Sample);
		end;
		Sam := Min(255, Trunc(Sam * Deck.Graph.Brightness));
		Y := Sam div 8;
		pbWave.Bitmap.VertLine(X, 31-Y, 31+Y, Grays[Sam]);
	end;

	pbWave.Refresh;
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

	pb.Refresh;
end;

procedure TDeckFrame.RedrawGraph;
var
	BPM: Single;
begin
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
			lTime.Caption := 'Loading';
			Screen.Cursor := crHourGlass;
			Invalidate;
			Application.ProcessMessages;
		end;

		MODE_LOAD_FINISH:
		begin
			RedrawGraph;
			SetCue(TPoint.Zero);
			DrawWaveform;
			SliderGraphX.Position := 0;

			Screen.Cursor := crDefault;
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
	if Zone >= Deck.Graph.Zones.Count then Exit;

	Z := Deck.Graph.Zones[Zone];
	Z.Kind := Kind;
	if Kind = zkJump then
		Z.Data := Deck.Graph.GraphToPos(GraphCue)
	else
		Z.Data := 0;

	RedrawGraph;
end;

procedure TDeckFrame.miZoneKind0Click(Sender: TObject);
begin
	SetZoneKind(CurrentZone, TZoneKind((Sender as TMenuItem).Tag));
end;

end.

