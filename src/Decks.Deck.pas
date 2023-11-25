unit Decks.Deck;

{$mode Delphi}
{$INLINE ON}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
	Classes, SysUtils, Forms, Menus, IniFiles, syncobjs,
	BASS, BASSmix, BASSloud, BASS_FX,
	Decks.Config, Decks.Audio, Decks.Song, Decks.SongInfo, Decks.BeatGraph, Decks.Effects;

type
	TDeck = class;

	TBaseDeckFrame = class(TFrame)
	public
		Deck: TDeck;
		procedure Init(ADeck: TDeck);
		procedure DoInit; virtual; abstract;
	end;

	TLoopInfo = record
		Enabled: Boolean;
		StartPos,
		EndPos:  QWord;
		Stream:  HSTREAM;
		Sync:    HSYNC;
		Zone:    Integer;
		LoopLength: Integer;
	end;
	PLoopInfo = ^TLoopInfo;

	TPitchBend = record
		Active:	Boolean;
		Up:		Boolean;
		Diff:	Single;
		Timer:  Integer;
	end;

	THotCue = record
		Enabled,
		Temporary:  Boolean;
		Pos,
		TriggerPos: QWord;
	end;

	TDeck = class(TSong)
	private
		CriticalSection: TCriticalSection;

		CurrVolume: Single;
		HadPitchLock: Boolean;

		FPlayingZone:	Word;
		FPlayPosition: 	QWord;

		procedure	InfoHandler(Keyword: TInfoKeyword; Params: TInfoParams);
		function 	DoGetPlayingZone: Word;
		procedure 	DoSetPlayingZone(Value: Word);
		function 	DoGetPlayPosition: QWord;
		procedure 	DoSetPlayPosition(Value: QWord);
	public
		Index:			Byte;
		IsShifted:		Boolean;
		Synced:			Boolean;
		Cueing:			Boolean;
		CueOn:			Boolean;
		OtherDeck:		TDeck;

		Cue:		array[0..9] of Integer;
		HotCues:	array[0..8] of THotCue;

		Form:		TBaseDeckFrame;
		Graph: 		TBeatGraph;
		PitchBend:	TPitchBend;
		Info:		TSongInfo;
		Tags:       TSongTags;
		MenuItem:	TMenuItem;
		QueuedSync: HSYNC;

		LoopInfo_Song,
		LoopInfo_Zone,
		LoopInfo_Misc: TLoopInfo;

		Filter: 	TFxFilter;
		Equalizer:  TEqualizer;

		OnLoadInfo,
		OnSaveInfo: TInfoFileAccessEvent;

		function	BPMToHz(aBPM: Single; Zone: Word = 0): Cardinal;
		function	HzToBPM(Hz: Single; Zone: Word = 0): Single;

		function	GetOtherOrCurrentDeck(ForSync: Boolean): TDeck;
		function	IsOtherDeckPlaying: Boolean;

		function 	Load(const AFilename: String): Boolean; override;
		function	GetInfo: Boolean;

		function 	GetPlayPosition(FromMixer: Boolean = True; OffsetByStart: Boolean = True): QWord; inline;
		function 	GetCurrentBar(FromMixer: Boolean = False): Word; inline;

		function	CalculateLUFS: Single;

		procedure	BendJog(Amount: Integer; Fast: Boolean);
		procedure	BendStart(Up, Fast: Boolean);
		procedure	BendUpdate;
		procedure	BendStop;

		procedure	SetVolume(NewVolume: Single);
		procedure	ToggleEQKill(BandNum: TEQBand);
		procedure 	ApplyFilter;
		procedure	UpdateCueOutput;

		procedure	LoopZone(ZoneIndex: Word);
		procedure	UnloopZone;
		procedure	UnloopAll;
		procedure	SetLoop(LoopType: TAppMode; Beats: Integer; Enable: Boolean);
		procedure	ApplyLoop(LoopInfo: PLoopInfo);
		procedure	ReapplyLoops;
		procedure	EnableLoop(LoopInfo: PLoopInfo; StartPos, EndPos: QWord);
		procedure	DisableLoop(LoopInfo: PLoopInfo);

		procedure	SetBPM(NewBPM: Double);
		function	GetAvgBPM: Single;

		procedure	SaveInfoFile(aBPM: Double = 0.0);
		procedure	SaveOldInfoFile;

		procedure	UpdateMenuItem;

		property	PlayingZone:  Word  read DoGetPlayingZone  write DoSetPlayingZone;
		property	PlayPosition: QWord read DoGetPlayPosition write DoSetPlayPosition;

		constructor	Create;  override;
		destructor	Destroy; override;
	end;

implementation

uses
	Math,
	Form.Main,
	Decks.TagScanner;

{$WARN 5024 off : Parameter "$1" not used}

{ TBaseDeckFrame }

procedure TBaseDeckFrame.Init(ADeck: TDeck);
begin
	BASS_FX_GetVersion; // force load
	Deck := ADeck;
	DoInit;
	Deck.Equalizer.Reset;
end;

{ TDeck }

constructor TDeck.Create;
begin
	inherited Create;

	CriticalSection := TCriticalSection.Create;

	OnLoadInfo := nil;
	OnSaveInfo := nil;

	Graph := TBeatGraph.Create(TSong(Self), Config.ThemePath);
	Paused := False;
	OtherDeck := nil;
	CurrVolume := 1.0;
	BeatPreviousQuadrant := 255;

	Filter := TFxFilter.Create;

//	Form := TDeckFrame.Create(nil);
//	Form.Init(Self);
end;

destructor TDeck.Destroy;
begin
	MenuItem.Free;
	Graph.Free;
	Form.Free;
	Filter.Free;
	FreeStream(Stream);
	FreeStream(OrigStream);
	CriticalSection.Free;

	inherited Destroy;
end;

function TDeck.BPMToHz(aBPM: Single; Zone: Word = 0): Cardinal;
begin
	if (AvgBPM < 10) or (Graph.Zones.Count < 1) then
		Result := 0
	else
		Result := Round(OrigFreq / Graph.Zones[Zone].BPM * aBPM);
end;

function TDeck.HzToBPM(Hz: Single; Zone: Word = 0): Single;
begin
	if AvgBPM < 10 then
		Result := 0
	else
		Result := SimpleRoundTo(Graph.Zones[Zone].BPM / OrigFreq * Hz, -3);
end;

function TDeck.GetOtherOrCurrentDeck(ForSync: Boolean): TDeck;
begin
	Result := OtherDeck;

	if Result = nil then
		Result := Self
	else
	if ForSync then
	begin
		if Result.Paused then
			Result := Self;
	end;
end;

function TDeck.IsOtherDeckPlaying: Boolean;
begin
	Result := (OtherDeck <> nil) and (not OtherDeck.Paused);
end;

procedure TDeck.ApplyFilter;
begin
	Filter.Stream  := OrigStream;
	Filter.Enabled := True;
	Filter.Apply;
end;

procedure TDeck.ToggleEQKill(BandNum: TEQBand);
var
	B: Boolean;
begin
	with Equalizer do
	begin
		B := not Band[BandNum].Killed;
		Band[BandNum].Killed := B;
		if B then
		begin
			Band[BandNum].KilledGain := Band[BandNum].Gain;
			SetEQ(BandNum, -1500/100);
		end
		else
		begin
			SetEQ(BandNum, Band[BandNum].KilledGain);
		end;
		ModeChange(MODE_EQ_KILL, B);
	end;
end;

procedure TDeck.UpdateCueOutput;
begin
	ApplyMixingMatrix(OrigStream,
		CueOn, Config.Mixer.CueMaster,
		Info.NormalizedAmp * Info.Amp, // amplified volume
		CurrVolume, // apply crossfader for master
		Config.Mixer.CueMix / 100); // normalized cue mix knob value
end;

//
// File I/O
//

function TDeck.Load(const AFilename: String): Boolean;
begin
	ModeChange(MODE_LOAD_START, True);

	Filename := AFilename;
	PlayingZone := 0;
	Info.Amp := 1.0;
	Info.LUFS := 0.0;

	Result := inherited Load(AFilename);

	if Result then
	begin
		ModeChange(MODE_LOAD_SUCCESS, True);
//		BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_VOL, Graph.CalcBrightness / 2);
		Equalizer.Init(Stream);
		ApplyFilter;
	end
	else
	begin
		ModeChange(MODE_LOAD_FAILURE, True);
		Filename := '';
		Loaded := False;
	end;

	ModeChange(MODE_LOAD_FINISH, True);
end;

function TDeck.CalculateLUFS: Single;
const
	buflen = 1024 * 64;
var
	P, maxlen: QWord;
	C: Integer;
	level: Single;
	Percentage, OldPercentage: Integer;
	loudness: HLOUDNESS;
	loudbuf: array of Byte;
{const
	volumetarget = -23; // EBU R-128}
begin
	// calculate loudness normalization value
	Info.NormalizedAmp := 1.0;
	if Config.Audio.TargetLUFS < 0 then
	begin
		if Info.LUFS >= 0.0 then
		begin
			SetLength(loudbuf{%H-}, buflen);
			P := Graph.Zones.First.Pos;
			maxlen := BASS_ChannelGetLength(FileStream, BASS_POS_BYTE) - P;
			BASS_ChannelSetPosition(FileStream, P, BASS_POS_BYTE);
			loudness := BASS_Loudness_Start(FileStream, BASS_LOUDNESS_INTEGRATED, 0);
			P := 0;
			OldPercentage := 0;
			repeat
				C := Integer(BASS_ChannelGetData(FileStream, @loudbuf[0], buflen));
				if C > 0 then
				begin
					Inc(P, C);
					Percentage := Round(P / maxlen * 100);
				end;
				if Percentage <> OldPercentage then
				begin
					OldPercentage := Percentage;
					if (Percentage mod 5 = 0) and (Assigned(Graph.OnLoadProgress)) then
						Graph.OnLoadProgress(Percentage);
				end;
			until (C < buflen) or (Percentage >= 99);
			BASS_Loudness_GetLevel(loudness, BASS_LOUDNESS_INTEGRATED, level{%H-});
			BASS_Loudness_Stop(loudness);
			if (level = Infinity) or (level = NegInfinity) then
				level := 0.0
			else
				Info.LUFS := Trunc(level * 100) / 100;
		end;
		if Info.LUFS < 0.0 then
		try
			Info.NormalizedAmp := Power(10, (Config.Audio.TargetLUFS - Info.LUFS) / 20);
			if Config.Audio.GraphLUFS < 0.0 then
				Graph.Brightness := Power(10, (Config.Audio.GraphLUFS - Info.LUFS) / 20);
		except
		end;
	end
	else
	{if Info.NormalizedAmp >= 0.1 then
		BASS_ChannelSetAttribute(OrigStream, BASS_ATTRIB_VOLDSP, Info.NormalizedAmp);}
	BASS_ChannelSetPosition(FileStream, 0, BASS_POS_BYTE);
	Result := Info.NormalizedAmp;
end;

//
// Read BPM file info
//

procedure TDeck.InfoHandler(Keyword: TInfoKeyword; Params: TInfoParams);
begin
	case Keyword of

		IKW_CUE:
			if High(Params) >= 1 then
			begin
				if Params[0] <= High(Cue) then
					Cue[Params[0]] := Params[1] * Graph.SampleSizeMultiplier;
				if Params[0] = 0 then
					Graph.StartPos := Params[1] * Graph.SampleSizeMultiplier;
			end;

		IKW_BPM, IKW_BPM_OLD:
			if High(Params) >= 1 then
			begin
				Graph.Zones.Clear;
				if Keyword = IKW_BPM_OLD then
					Graph.AddZone(Graph.StartPos, Params[0] + (Params[1] / 1000), True);
			end;

		IKW_ZONE, IKW_ZONE_OLD:
			if High(Params) >= 2 then
			begin
				if Graph.Zones.Count < 1 then
					Graph.AddZone(Graph.StartPos, Params[0] + (Params[1] / 1000), True)
				else
					Graph.AddZone(Params[2], Params[0] + (Params[1] / 1000), Keyword = IKW_ZONE_OLD);
			end;

		IKW_ZONEDATA:
			with Graph.Zones.Last do
			begin
				Kind := TZoneKind(Params[0]);
				Data := Params[1];
			end;

	end;
end;

function TDeck.DoGetPlayingZone: Word;
begin
	CriticalSection.Enter;
	Result := FPlayingZone;
	CriticalSection.Leave;
end;

function TDeck.DoGetPlayPosition: QWord;
begin
	CriticalSection.Enter;
	Result := FPlayPosition;
	CriticalSection.Leave;
end;

procedure TDeck.DoSetPlayingZone(Value: Word);
begin
	CriticalSection.Enter;
	FPlayingZone := Value;
	CriticalSection.Leave;
end;

procedure TDeck.DoSetPlayPosition(Value: QWord);
begin
	CriticalSection.Enter;
	FPlayPosition := Value;
	CriticalSection.Leave;
end;

function TDeck.GetInfo: Boolean;
begin
	Graph.Zones.Clear;
	Tags := ReadFileTags(Filename, False);
	Info := GetSongInfo(ExtractFileName(Filename), OnLoadInfo, InfoHandler);
	Result := Info.Initialized;
	if not Result then
		Info := Tags.Info;
	if Info.Initialized then
	begin
		if Info.Bitrate > 0 then
			Bitrate := Info.Bitrate;
		if Info.Length >= 1 then
			Duration := Info.Length;
	end;
end;

//
// Pitch bend
//
procedure TDeck.BendJog(Amount: Integer; Fast: Boolean);
begin
	if Amount = 0 then Exit;

	if not PitchBend.Active then
	begin
		PitchBend.Up := Amount > 0;
		PitchBend.Diff := 1.5;
		if Fast then
			PitchBend.Diff *= 3.0;
		if not PitchBend.Up then
			PitchBend.Diff *= -1;
		PitchBend.Active := True;
	end
	else
		PitchBend.Diff := PitchBend.Diff * 1.006;
	if not PitchBend.Active then
		ModeChange(MODE_BEND, True);
	PitchBend.Timer := 4;
	SetBPM(MasterBPM + PitchBend.Diff);
end;

procedure TDeck.BendStart(Up, Fast: Boolean);
begin
	if not PitchBend.Active then
	begin
		PitchBend.Up := Up;
		PitchBend.Diff := 1.5;
		if Fast then
			PitchBend.Diff *= 3.0;
		if not Up then
			PitchBend.Diff *= -1;
		PitchBend.Timer := 0;
		PitchBend.Active := True;
		ModeChange(MODE_BEND, True);
		SetBPM(MasterBPM + PitchBend.Diff);
	end;
end;

procedure TDeck.BendUpdate;
begin
	if PitchBend.Active then
	begin
		if PitchBend.Timer > 0 then // used jogwheel to bend
		begin
			PitchBend.Timer := PitchBend.Timer - 1;
			if PitchBend.Timer > 0 then
			begin
				if Abs(PitchBend.Diff) > 20 then
					PitchBend.Diff *= 0.99;
				SetBPM(MasterBPM + PitchBend.Diff);
			end
			else
				BendStop;
		end
		else
		begin
			if Abs(PitchBend.Diff) < 20 then
				PitchBend.Diff *= 1.02;
			SetBPM(MasterBPM + PitchBend.Diff);
		end;
	end;
end;

procedure TDeck.BendStop;
begin
	if PitchBend.Active then
	begin
		PitchBend.Active := False;
		PitchBend.Timer := 0;
		SetBPM(MasterBPM);
		ModeChange(MODE_BEND, False);
	end;
end;

function TDeck.GetAvgBPM: Single;
var
	i, c: Integer;
begin
	AvgBPM := 0.0;
	c := Graph.Zones.Count;
	if c > 0 then
	begin
		for i := 0 to c-1 do
			AvgBPM += Graph.Zones[i].BPM;
		AvgBPM := AvgBPM / c;
	end;
	Result := AvgBPM;
end;

procedure TDeck.SetBPM(NewBPM: Double);
begin
	if (not Loaded) then Exit;

	PlayFreq := BPMToHz(NewBPM, PlayingZone);

	if PitchLock then
	begin
		if not HadPitchLock then
			BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_FREQ, OrigFreq);
		BASS_ChannelSetAttribute(Reverse.Stream, BASS_ATTRIB_TEMPO, (PlayFreq / OrigFreq - 1.0) * 100.0);
	end;

	BPM := NewBPM;

	if not PitchLock then
	begin
		if HadPitchLock then
			BASS_ChannelSetAttribute(Reverse.Stream, BASS_ATTRIB_TEMPO, 1.0);
		BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_FREQ, PlayFreq);
	end;

	HadPitchLock := PitchLock;
	ModeChange(MODE_TEMPOCHANGE, True);
end;

procedure TDeck.SetVolume(NewVolume: Single);
begin
	if NewVolume < 0 then
		NewVolume := CurrVolume
	else
		CurrVolume := NewVolume;

	if Config.Mixer.CueMode = CUE_NONE then
		BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_VOL, NewVolume * (*Graph.{Calc}Brightness *) Info.NormalizedAmp * Info.Amp * (Config.Mixer.MasterVolume / 100))
	else
		UpdateCueOutput;
end;

//
//
//

procedure TDeck.SaveInfoFile(aBPM: Double = 0.0);

	function FloatToString(N: Single): String;
	begin
		Result := Format('%d.%.3d', [Trunc(N), Trunc(Frac(N)*1000)]);
	end;

var
	S, Sect: String;
	Ini: TIniFile;
	Z: TZone;
	i: Integer;
begin
	if BPM < 0.1 then aBPM := AvgBPM;

	S := Config.Directory.BPM + ExtractFilename(Filename) + '.bpm';
	Ini := TIniFile.Create(S);
	try
		Ini.WriteString('Decks 3', 'version', Version);

		Sect := 'song';
			Ini.WriteString(Sect, 'path', ExtractFilePath(Filename));
			Ini.WriteString(Sect, 'file', ExtractFileName(Filename));

			Ini.WriteString(Sect, 'bpm',      FloatToString(aBPM));
			Ini.WriteString(Sect, 'gain',     FloatToString(Info.Amp));
			Ini.WriteString(Sect, 'lufs',     FloatToString(Info.LUFS));
			Ini.WriteInteger(Sect, 'bitrate', Bitrate);
			Ini.WriteString(Sect, 'duration', FloatToString(Duration));

		if aBPM > 10 then
		begin
			Ini.WriteInteger(Sect, 'zones', Graph.Zones.Count);

			Ini.WriteInt64('cue', '0', Graph.StartPos div Graph.SampleSizeMultiplier);

			for i := 0 to Graph.Zones.Count-1 do
			begin
				Z := Graph.Zones[i];
				Sect := Format('zone.%d', [i]);

				Ini.WriteString(Sect, 'bpm',  FloatToString(Z.BPM));
				Ini.WriteInt64 (Sect, 'bar',  Z.barindex);
				Ini.WriteString(Sect, 'kind', ZoneKindNames[Z.Kind]);
				Ini.WriteInt64 (Sect, 'data', Z.Data);
			end;
		end;
		if Assigned(OnSaveInfo) then
			OnSaveInfo(S, Ini);
	finally
		Ini.Free;
	end;
end;

procedure TDeck.SaveOldInfoFile;

	function FloatToString(N: Single): String;
	begin
		Result := Format('%d %d', [Trunc(N), Trunc(Frac(N)*1000)]);
	end;

var
	S: String;
	Sl: TStringList;
	Z: TZone;
begin
	S := Config.Directory.BPM + ExtractFilename(Filename) + '.bpm';
	Sl := TStringList.Create;
	try
		Sl.Add('Decks 3.0b');
		Sl.Add('FILENAME ' + Filename);
		Sl.Add('BPM ' + FloatToString(Graph.Zones.First.BPM));
		Sl.Add('AMP ' + FloatToString(Info.Amp));
		Sl.Add(Format('CUE 0 %d', [Graph.StartPos div Graph.SampleSizeMultiplier]));
		for Z in Graph.Zones do
			if Z <> Graph.Zones.First then
			begin
				Sl.Add(Format('ZONE %s %d',
				[ FloatToString(Z.BPM), Graph.GraphToSongBytes(Z.Pos) div Graph.SampleSizeMultiplier]));
				Sl.Add(Format('!ZONE %s %d',
				[ FloatToString(Z.BPM), Z.barindex ]));
			end;

		Sl.SaveToFile(S);
	finally
		Sl.Free;
	end;
end;

procedure TDeck.UpdateMenuItem;
begin
	MenuItem.Tag := Index;
	MenuItem.Caption := 'Deck ' + MenuItem.Tag.ToString;
	if (Self.Paused) or (not Self.Loaded) then
	begin
		MenuItem.Enabled := True;
		MenuItem.ImageIndex := -1;
	end
	else
	begin
		MenuItem.Enabled := False;
		MenuItem.ImageIndex := 22;
	end;
end;

function TDeck.GetPlayPosition(FromMixer: Boolean = True; OffsetByStart: Boolean = True): QWord;
var
	P: QWord;
begin
	if OffsetByStart then
		P := Graph.StartPos
	else
		P := 0;

	if FromMixer then
		Result := Max(0, BASS_Mixer_ChannelGetPosition(
			OrigStream, BASS_POS_BYTE) - P)
	else
		Result := Max(0, BASS_ChannelGetPosition(
			OrigStream, BASS_POS_BYTE) - P);
end;

function TDeck.GetCurrentBar(FromMixer: Boolean = False): Word;
var
	P: Int64;
begin
	P := GetPlayPosition(FromMixer); //Max(0, BASS_ChannelGetPosition(OrigStream, BASS_POS_BYTE));// - Graph.StartPos);
	Result := Max(0, Graph.PosToBar(P));
end;

//
// Looping
//

procedure Audio_Callback_LoopSync(handle: HSYNC; channel, data: DWord; user: Pointer);
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
	Info: TLoopInfo;
begin
	Info := TLoopInfo(user^);
	if Info.Enabled then
		BASS_ChannelSetPosition(channel, Info.StartPos, BASS_POS_BYTE);
end;

procedure TDeck.ApplyLoop(LoopInfo: PLoopInfo);
begin
	if LoopInfo^.Enabled then
	begin
		LoopInfo^.Stream := OrigStream;
		if LoopInfo^.Sync <> 0 then
			BASS_ChannelRemoveSync(OrigStream, LoopInfo^.Sync);
		LoopInfo^.Sync := BASS_ChannelSetSync(OrigStream,
			BASS_SYNC_POS or BASS_SYNC_MIXTIME, LoopInfo^.EndPos,
			@Audio_Callback_LoopSync, LoopInfo);
	end;
end;

procedure TDeck.EnableLoop(LoopInfo: PLoopInfo; StartPos, EndPos: QWord);
begin
	LoopInfo^.StartPos := StartPos;
	LoopInfo^.EndPos := EndPos;
	LoopInfo^.Enabled := True;
	ApplyLoop(LoopInfo);
end;

procedure TDeck.DisableLoop(LoopInfo: PLoopInfo);
begin
	if LoopInfo^.Enabled then
	begin
		LoopInfo^.Enabled := False;
		BASS_ChannelRemoveSync(OrigStream, LoopInfo^.Sync);
		LoopInfo^.Sync := 0;
	end;
end;

procedure TDeck.ReapplyLoops;
begin
	ApplyLoop(@LoopInfo_Song);
	ApplyLoop(@LoopInfo_Misc);
	if (LoopInfo_Zone.Enabled) and (LoopInfo_Zone.Zone >= 0) then
		LoopZone(LoopInfo_Zone.Zone);
end;

procedure TDeck.UnloopZone;
begin
	DisableLoop(@LoopInfo_Zone);
	LoopInfo_Zone.Zone := -1;
end;

procedure TDeck.LoopZone(ZoneIndex: Word);
var
	Z: TZone;
	StartPos, EndPos: QWord;
begin
	Z := Graph.Zones[ZoneIndex];
	LoopInfo_Zone.Zone := ZoneIndex;
	StartPos := Graph.GraphToSongBytes(Z.Pos);
	EndPos := StartPos + Graph.GraphToSongBytes(Z.length);
	EnableLoop(@LoopInfo_Zone, StartPos, EndPos);
end;

procedure TDeck.UnloopAll;
begin
	DisableLoop(@LoopInfo_Song);
	DisableLoop(@LoopInfo_Misc);
	UnloopZone;
end;

procedure TDeck.SetLoop(LoopType: TAppMode; Beats: Integer; Enable: Boolean);
var
	Bar: Word;
	StartPos, EndPos: QWord;
	LoopInfo: PLoopInfo;
	Pt: TPoint;
	Q: Integer;
	BeatCount: Double;
begin
	case LoopType of

		MODE_LOOP:
		begin
			LoopInfo := @LoopInfo_Misc;
			BeatCount := Max(Beats, 0.5);
			Bar := GetCurrentBar(True);
			StartPos := Graph.GraphToSongBytes(Graph.Bars[Bar].Pos);
			EndPos := Graph.GetBarLength(False, Bar);
			LoopInfo.LoopLength := Beats;
			Pt := Graph.PosToGraph(GetPlayPosition);
			if Beats = 0 then
				Q := 8 else Q := 4; // higher res for 1/2 beat loop
			Pt.Y := Trunc(Pt.Y / Graph.Height * Q); // quadrant
			if Pt.Y > 0 then
				StartPos += Trunc(EndPos * Pt.Y / Q);
			EndPos := Trunc(EndPos / 4 * BeatCount) + StartPos;
		end;

		MODE_LOOP_ZONE:
		begin
			if Enable then
				LoopZone(Graph.GetZoneIndexAt(Graph.SongToGraphBytes(GetPlayPosition)))
			else
				UnloopZone;
			ModeChange(LoopType, Enable);
			Exit;
		end;

		MODE_LOOP_SONG:
		begin
			LoopInfo := @LoopInfo_Song;
			StartPos := Graph.StartPos;
			EndPos := BASS_ChannelGetLength(OrigStream, BASS_POS_BYTE);
		end;

		else Exit;
	end;

	if not Enable then
	begin
		LoopInfo^.Enabled := False;
		BASS_ChannelRemoveSync(OrigStream, LoopInfo^.Sync);
	end
	else
		EnableLoop(LoopInfo, StartPos, EndPos);

	ModeChange(LoopType, Enable);
end;


end.

