unit Decks.Deck;

{$mode Delphi}
{$INLINE ON}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
	Classes, SysUtils, Forms, Menus,
	BASS, BASSmix, BASS_FX,
	Decks.Audio, Decks.Song, Decks.SongInfo, Decks.BeatGraph,
	Decks.Effects;

const
	LOOP_OFF   = 0;
	LOOP_SONG  = 0;
	LOOP_ZONE  = 1;
	LOOP_BARS  = 2;
	LOOP_BEATS = 3;

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
	end;
	PLoopInfo = ^TLoopInfo;

	TPitchBend = record
		Active:	Boolean;
		Up:		Boolean;
		Diff:	Single;
	end;

	TDeck = class(TSong)
	private
		CurrVolume: Single;
		procedure	InfoHandler(Keyword: TInfoKeyword; Params: TInfoParams);
	public
		Index:			Byte;
		PlayingZone:	Word;
		Synced:			Boolean;
		Cueing:			Boolean;
		OtherDeck:		TDeck;

		Cue:		array[0..9] of Integer;

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

		Equalizer:    TEqualizer;

		function	BPMToHz(aBPM: Single; Zone: Word = 0): Cardinal;
		function	HzToBPM(Hz: Single; Zone: Word = 0): Single;

		function	GetOtherOrCurrentDeck: TDeck;

		function 	Load(const AFilename: String): Boolean; override;
		function	GetInfo: Boolean;

		function 	GetPlayPosition(FromMixer: Boolean = True): Int64; inline;
		function 	GetCurrentBar: Word; inline;

		procedure	BendStart(Up, Fast: Boolean);
		procedure	BendUpdate;
		procedure	BendStop;

		procedure	SetVolume(NewVolume: Single);
		procedure	ToggleEQKill(BandNum: TEQBand);

		procedure	LoopZone(ZoneIndex: Word);
		procedure	UnloopZone;
		procedure	SetLoop(LoopType, LoopLength: Integer);
		procedure	EnableLoop(LoopInfo: PLoopInfo; StartPos, EndPos: QWord);
		procedure	DisableLoop(LoopInfo: PLoopInfo);
		procedure	SetBPM(NewBPM: Single);
		function	GetAvgBPM: Single;

		procedure	SaveInfoFile(aBPM: Double = 0.0);
		procedure	SaveOldInfoFile;

		procedure	UpdateMenuItem;

		constructor	Create;  override;
		destructor	Destroy; override;
	end;

implementation

uses
	Math, IniFiles,
	Form.Main,
	Decks.Config, Decks.TagScanner;

{$WARN 5024 off : Parameter "$1" not used}

procedure TDeck.ToggleEQKill(BandNum: TEQBand);
begin
	with Equalizer do
	begin
		Band[BandNum].Killed := not Band[BandNum].Killed;
		if Band[BandNum].Killed then
		begin
			Band[BandNum].KilledGain := Band[BandNum].Gain;
			SetEQ(BandNum, -1500/100);
			ModeChange(MODE_EQ_KILL_ON);
		end
		else
		begin
			SetEQ(BandNum, Band[BandNum].KilledGain);
			ModeChange(MODE_EQ_KILL_OFF);
		end;
	end;
end;

{ TBaseDeckFrame }

procedure TBaseDeckFrame.Init(ADeck: TDeck);
begin
	BASS_FX_GetVersion; // force load
	Deck := ADeck;
	DoInit;
	Deck.Equalizer.Reset;
end;

{ TDeck }

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

function TDeck.GetOtherOrCurrentDeck: TDeck;
begin
	Result := OtherDeck;
	if Result = nil then
		Result := Self;
end;

constructor TDeck.Create;
begin
	inherited Create;

	Graph := TBeatGraph.Create(TSong(Self), Config.ThemePath);
	Paused := False;
	OtherDeck := nil;
	CurrVolume := 1.0;
	BeatPreviousQuadrant := 255;
//	Form := TDeckFrame.Create(nil);
//	Form.Init(Self);
end;

destructor TDeck.Destroy;
begin
	MenuItem.Free;
	Graph.Free;
	Form.Free;
	FreeStream(Stream);
	FreeStream(OrigStream);

	inherited Destroy;
end;

//
// File I/O
//

function TDeck.Load(const AFilename: String): Boolean;
begin
	ModeChange(MODE_LOAD_START);

	PlayingZone := 0;
	Filename := AFilename;
	Result := inherited Load(AFilename);

	if Result then
	begin
		ModeChange(MODE_LOAD_SUCCESS);
		BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_VOL, Graph.CalcBrightness / 2);
		Equalizer.Init(Stream);
	end
	else
	begin
		ModeChange(MODE_LOAD_FAILURE);
		Filename := '';
		Loaded := False;
	end;

	ModeChange(MODE_LOAD_FINISH);
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

function TDeck.GetInfo: Boolean;
begin
	Graph.Zones.Clear;
	Tags := ReadFileTags(Filename, False);
	Info := GetSongInfo(ExtractFileName(Filename), InfoHandler);
	Result := Info.Initialized;
	if not Result then
	begin
		Info := Tags.Info;
		Result := Info.Initialized;
	end;
	if Result then
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

procedure TDeck.BendStart(Up, Fast: Boolean);
begin
	if not PitchBend.Active then
	begin
		PitchBend.Active := True;
		PitchBend.Up := Up;
		PitchBend.Diff := 1.5;
		if Fast then
			PitchBend.Diff *= 3.0;
		if not Up then
			PitchBend.Diff *= -1;
		SetBPM(MasterBPM + PitchBend.Diff);
		ModeChange(MODE_BEND_START);
	end;
end;

procedure TDeck.BendUpdate;
begin
	if PitchBend.Active then
	begin
		if Abs(PitchBend.Diff) < 20 then
			PitchBend.Diff *= 1.02;
		SetBPM(MasterBPM + PitchBend.Diff);
	end;
end;

procedure TDeck.BendStop;
begin
	if PitchBend.Active then
	begin
		PitchBend.Active := False;
		SetBPM(MasterBPM);
		ModeChange(MODE_BEND_STOP);
	end;
end;

function TDeck.GetAvgBPM: Single;
var
	i, c: Integer;
begin
	c := Graph.Zones.Count;
	AvgBPM := 0.0;
	for i := 0 to c-1 do
		AvgBPM += Graph.Zones[i].BPM;
	AvgBPM := AvgBPM / c;
	Result := AvgBPM;
end;

procedure TDeck.SetBPM(NewBPM: Single);
begin
	if (not Loaded) then Exit;
	BPM := NewBPM;
	PlayFreq := BPMToHz(NewBPM, PlayingZone);
	BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_FREQ, PlayFreq);
	ModeChange(MODE_TEMPOCHANGE);
end;

procedure TDeck.SetVolume(NewVolume: Single);
begin
	if NewVolume < 0 then
		NewVolume := CurrVolume
	else
		CurrVolume := NewVolume;
	BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_VOL, NewVolume * Graph.{Calc}Brightness);
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
			Ini.WriteString(Sect, 'amp',      FloatToString(Info.Amp));
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

				Ini.WriteString(Sect, 'bpm', FloatToString(Z.BPM));
				Ini.WriteInt64(Sect, 'bar', Z.barindex);
				Ini.WriteString(Sect, 'kind', ZoneKindNames[Z.Kind]);
				Ini.WriteInt64(Sect, 'data', Z.Data);
			end;
		end;
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

function TDeck.GetPlayPosition(FromMixer: Boolean = True): Int64;
begin
	if FromMixer then
		Result := Max(0, BASS_Mixer_ChannelGetPosition(
			OrigStream, BASS_POS_BYTE) - Graph.StartPos)
	else
		Result := Max(0, BASS_ChannelGetPosition(
			OrigStream, BASS_POS_BYTE) - Graph.StartPos);
end;

function TDeck.GetCurrentBar: Word;
var
	P: Int64;
begin
	P := GetPlayPosition; //Max(0, BASS_ChannelGetPosition(OrigStream, BASS_POS_BYTE));// - Graph.StartPos);
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

procedure TDeck.EnableLoop(LoopInfo: PLoopInfo; StartPos, EndPos: QWord);
begin
	LoopInfo^.StartPos := StartPos;
	LoopInfo^.EndPos := EndPos;
	LoopInfo^.Stream := OrigStream;
	LoopInfo^.Enabled := True;
	LoopInfo^.Sync := BASS_ChannelSetSync(OrigStream,
		BASS_SYNC_POS or BASS_SYNC_MIXTIME, EndPos,
		@Audio_Callback_LoopSync, LoopInfo);
end;

procedure TDeck.DisableLoop(LoopInfo: PLoopInfo);
begin
	if LoopInfo^.Enabled then
	begin
		LoopInfo^.Enabled := False;
		BASS_ChannelRemoveSync(OrigStream, LoopInfo^.Sync);
	end;
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

procedure TDeck.SetLoop(LoopType, LoopLength: Integer);
var
	Bar: Word;
	StartPos, EndPos: QWord;
	LoopInfo: PLoopInfo;
begin
	case LoopType of

		LOOP_BEATS, LOOP_BARS:
		begin
			LoopInfo := @LoopInfo_Misc;
			Bar := GetCurrentBar;
			StartPos := Graph.GraphToSongBytes(Graph.Bars[Bar].Pos);
			EndPos := Graph.GetBarLength(False, Bar);
			if LoopType = LOOP_BEATS then
				EndPos := Trunc(EndPos / 4 * LoopLength)
			else
				EndPos := EndPos * LoopLength;
			EndPos += StartPos;
		end;

		LOOP_ZONE:
		begin
			LoopZone(Graph.GetZoneIndexAt(Graph.SongToGraphBytes(GetPlayPosition)));
			Exit;
		end;

		LOOP_SONG:
		begin
			LoopInfo := @LoopInfo_Song;
			StartPos := Graph.StartPos;
			EndPos := BASS_ChannelGetLength(OrigStream, BASS_POS_BYTE);
		end;

		else Exit;
	end;

	if LoopLength = LOOP_OFF then
	begin
		LoopInfo^.Enabled := False;
		BASS_ChannelRemoveSync(OrigStream, LoopInfo^.Sync);
	end
	else
		EnableLoop(LoopInfo, StartPos, EndPos);
end;


end.

