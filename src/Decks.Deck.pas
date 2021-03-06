unit Decks.Deck;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils, Forms, Menus,
	BASS, BASSmix, BASS_FX,
	Decks.Audio, Decks.Song, Decks.SongInfo, Decks.BeatGraph;

type
	TEQBand = ( EQ_BAND_LOW, EQ_BAND_MID, EQ_BAND_HIGH );

const
	CenterFreqs: array[TEQBand] of Word = (125, 1000, 8000);

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
	end;
	PLoopInfo = ^TLoopInfo;

	TPitchBend = record
		Active:	Boolean;
		Up:		Boolean;
		Diff:	Single;
	end;

	TEQBandData = record
		CenterFreq: Word;   // Hz
		Gain,
		KilledGain: Single; // -15..+15
		Killed:     Boolean;
	end;

	TEqualizer = record
		Handle:   HFX;
		Stream:   HSTREAM;
		Band:     array[TEQBand] of TEQBandData; // low, mid, high
		FX:       BASS_BFX_PEAKEQ;

		procedure Reset;
		procedure Init(TheStream: HSTREAM);
		procedure SetEQ(BandNum: TEQBand; Gain: Single);
		procedure Apply(BandNum: Integer = -1);
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
		MenuItem:	TMenuItem;

		LoopInfo_Song,
		LoopInfo_Zone,
		LoopInfo_Misc: TLoopInfo;

		Equalizer:	TEqualizer;

		function	BPMToHz(aBPM: Single; Zone: Word = 0): Cardinal;
		function	HzToBPM(Hz: Single; Zone: Word = 0): Single;

		function 	Load(const AFilename: String): Boolean; override;
		function	GetInfo: Boolean;

		function 	GetPlayPosition(FromMixer: Boolean = True): Int64; inline;
		function 	GetCurrentBar: Word; inline;

		procedure	BendStart(Up, Fast: Boolean);
		procedure	BendUpdate;
		procedure	BendStop;

		procedure	SetVolume(NewVolume: Single);
		procedure	ToggleEQKill(BandNum: TEQBand);

		procedure	SetLoop(LoopType, LoopLength: Integer);
		procedure	SetBPM(NewBPM: Single);

		procedure	SaveInfoFile;

		procedure	UpdateMenuItem;

		constructor	Create;  override;
		destructor	Destroy; override;
	end;

implementation

uses
	Math,
	Form.Main,
	Decks.Config;

//
// Equalizer
//

procedure TEqualizer.Reset;
var
	i: TEQBand;
begin
	Stream := 0;
	for i in TEQBand do
	begin
		Band[i].CenterFreq := CenterFreqs[i];
		Band[i].Gain := 1.0;
	end;
end;

procedure TEqualizer.Init(TheStream: HSTREAM);
begin
	Stream := TheStream;
	Handle := BASS_ChannelSetFX(Stream, BASS_FX_BFX_PEAKEQ, 0);
	Assert(Handle <> 0, 'TEqualizer.Init: BASS_ChannelSetFX failed! Stream=' + TheStream.ToString);
	Apply;
end;

procedure TEqualizer.SetEQ(BandNum: TEQBand; Gain: Single);
begin
	Assert(BandNum <= High(Band), 'TEqualizer.SetEQ: Invalid BandNum');
	Band[BandNum].Gain := Gain;
	Apply(Ord(BandNum));
end;

procedure TEqualizer.Apply(BandNum: Integer = -1);
var
	i: TEQBand;
	B: Boolean;
begin
	with FX do
	begin
		fBandwidth := 2.0;
		fQ := 0;
		lChannel := BASS_BFX_CHANALL;
		for i in TEQBand do
		begin
			if (BandNum < 0) or (i = TEQBand(BandNum)) then
			begin
				lBand := Ord(i);
				fCenter := Band[i].CenterFreq;
				fGain := Band[i].Gain;
				B := BASS_FXSetParameters(Handle, @FX);
				Assert(B = True, 'TEqualizer.Apply: BASS_FXSetParameters failed! Handle=' + Handle.ToString);
			end;
		end;
	end;

	//for v := 0 to 2 do
	//	Audio_SetEqualizer(deck, v, d_fxEQ[deck].Gain[v]);

	//Audio_SetEQKill(deck, d_isEQKilled[deck]);
end;

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
	if (OrigBPM < 10) or (Graph.Zones.Count < 1) then
		Result := 0
	else
		Result := Round(OrigFreq / Graph.Zones[Zone].BPM * aBPM);
end;

function TDeck.HzToBPM(Hz: Single; Zone: Word = 0): Single;
begin
	if OrigBPM < 10 then
		Result := 0
	else
		Result := SimpleRoundTo(Graph.Zones[Zone].BPM / OrigFreq * Hz, -3);
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
var
	SamMul: Integer;
begin
	SamMul := Graph.SampleSizeMultiplier;

	case Keyword of

		IKW_CUE:
		if High(Params) >= 1 then
		begin
			if Params[0] <= High(Cue) then
				Cue[Params[0]] := Params[1] * SamMul;
			if Params[0] = 0 then
				Graph.StartPos := Params[1] * SamMul;
		end;

		IKW_BPM:
		if High(Params) >= 1 then
		begin
			Graph.Zones.Clear;
			Graph.AddZone(Graph.StartPos, Params[0] + (Params[1] / 1000), True);
		end;

		IKW_ZONE, IKW_ZONE_OLD:
		if High(Params) >= 2 then
			Graph.AddZone(Params[2], Params[0] + (Params[1] / 1000), Keyword = IKW_ZONE_OLD);

	end;
end;

function TDeck.GetInfo: Boolean;
begin
	Graph.Zones.Clear;
	Info := GetSongInfo(ExtractFileName(Filename), InfoHandler);
	Result := Info.Initialized;
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

procedure TDeck.SetBPM(NewBPM: Single);
begin
	if not Loaded then Exit;
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

procedure TDeck.SaveInfoFile;

	function FloatToString(N: Single): String;
	begin
		Result := Format('%d %d', [Trunc(N), Trunc(Frac(N)*1000) ]);
	end;

var
	S: String;
	Sl: TStringList;
	Z: TZone;
begin
(*
Decks 2.0a
FILENAME X:\mix\house\475_ Bob Sinclair - Gymtonic.mp3
BPM 133 716
AMP 1 0
CUE 0 66486
ZONE 133 657 6082036
*)
	S := Config.Directory.BPM + ExtractFilename(Filename) + '.bpm';
	Sl := TStringList.Create;
	try
		Sl.Add('Decks 3.0a');
		Sl.Add('FILENAME ' + Filename);
		Sl.Add('BPM ' + FloatToString(OrigBPM));
		Sl.Add('AMP ' + FloatToString(Graph.Brightness / 2));
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
		MenuItem.ImageIndex := -1
	else
		MenuItem.ImageIndex := 22;
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

procedure TDeck.SetLoop(LoopType, LoopLength: Integer);
var
	Z: TZone;
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
			LoopInfo := @LoopInfo_Zone;
			Z := Graph.GetZoneAt(GetPlayPosition);
			StartPos := Graph.GraphToSongBytes(Z.Pos);
			EndPos := Graph.GraphToSongBytes(Z.Pos + Z.length);
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
	begin
		//MainForm.Caption := Format('SET: %d - %d    Curr: %d', [StartPos, EndPos, GetPlayPosition]);
		LoopInfo^.StartPos := StartPos;
		LoopInfo^.EndPos := EndPos;
		LoopInfo^.Stream := OrigStream;
		LoopInfo^.Enabled := True;
		LoopInfo^.Sync := BASS_ChannelSetSync(OrigStream,
			BASS_SYNC_POS or BASS_SYNC_MIXTIME, EndPos,
			@Audio_Callback_LoopSync, LoopInfo);
	end;

end;


end.

