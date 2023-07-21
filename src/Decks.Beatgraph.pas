unit Decks.Beatgraph;

{$mode Delphi}
{$INLINE ON}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
	Classes, SysUtils, Types, Math, FGL,
	BGRABitmap, BGRABitmapTypes,
	BASS, BASSMIX,
	Decks.Song;

{$WARN 5024 off : Parameter "$1" not used}

const
	ZONECHANGE_STOPPED = -2;
	ZONECHANGE_GETPOS  = -1;

	Iterations = 3;

type
	TBGRAPalette = array[0..255] of TBGRAPixel;

	TBeatGraph = class; // forward

	TZoneKind = (
		zkNormal,	// no special processing
		zkLoop,		// loop current zone indefinitely
		zkJump,		// jump to bar # defined in Data
		zkSkip,		// jump straight to next zone
		zkEnd		// marks the end of song
	);

	TGraphIterationEvent = procedure of Object;

	TZoneChangeEvent   = procedure(Zone: Integer; MixTime: Boolean) of Object;
	TLoadProgressEvent = procedure(Percentage: Integer) of Object;

	TZoneChangeEventInfo = class
		Zone:  Integer;
		Event: TZoneChangeEvent;
	end;

	TBar = packed record
		Pos:			QWord;
		Zone:			Word;
	end;

	TZone = class
		Pos:			QWord;
		BPM:			Single;
		step:			Double;
		length,
		amount_bars:	Cardinal;
		length_bar:		Double;
		barindex:		Word;
		Kind:			TZoneKind;
		Data:			QWord;
		Sync,
		MixSync:		HSYNC;
		Graph:			TBeatGraph;
		ChangeEvent:	TZoneChangeEventInfo;
		barpos:			array of QWord;
		ysample:		array of DWord;

		function 	GetBarLength(ForGraph: Boolean = False):  QWord;
		procedure	Calculate;

		constructor	Create;
		destructor	Destroy; override;
	end;

	TBeatGraph = class
	const
		SampleSizeMultiplier = 2;
	private
		Generating:			Boolean;
		graph_Hz,
		HzDivisor:			Word;
		Song:				TSong;
		EndSync:			HSYNC;
		EndSyncEvent:		TZoneChangeEventInfo;
	public
		Zoom, WantedZoom,
		Width, Height:		Word;
		OnZoneChange:		TZoneChangeEvent;
		OnGraphIteration:	TGraphIterationEvent;
		OnLoadProgress:		TLoadProgressEvent;
		BitmapSize:			TPoint;
		Bitmap:				TBGRABitmap; //TBitmap32;
		Scroll:				TPoint;
		Brightness,
		CalcBrightness:		Single;
		length_audio:		QWord;
		amount_bars:		Cardinal;
		StartPos:			QWord; // song pos
		LoadChunkSize:		DWord;
		NeedRecalc,
		ZoomChanged,
		QueueDraw,
		Drawing:			Boolean;

		AudioData:			array of Byte;
		Bars:				array of TBar;
		Zones:				TFPGObjectList<TZone>;

		function	HasZones: Boolean; inline;
		function 	GetZoneIndexAt(P: QWord): Word;
		function 	GetZoneAt(P: QWord): TZone; inline;
		procedure 	ZonesChanged;
		procedure 	ZonesLoaded;
		function 	AddZone(Barpos: QWord; BPM: Single = 0; IsSongPos: Boolean = False): TZone;
		function	RemoveZone(ZoneIndex: Word): Boolean;

		function	BytesPerSample: Byte; inline;
		function	GetFreq(ForGraph: Boolean): Word; inline;
		function	BarToGraph(B: Word): Word; inline;
		function	GraphToBar(X: Word): Word; inline;
		function 	SongToGraphBytes(Pos: QWord): QWord; inline;
		function	GraphToSongBytes(Pos: QWord; ForGraph: Boolean = False): QWord;
		function	GraphToPos(P: TPoint; ForGraph: Boolean = False): QWord;
		function	GetYPos(P: QWord): Single;
		function 	PosToBar(P: QWord; ForGraph: Boolean = False): Integer;
		function	PosToGraph(P: QWord; ForGraph: Boolean = False): TPoint;
		function	GetBeatLength(ForGraph: Boolean = False): Integer; inline;
		function	GetNextBeat(P: QWord; out IsFirstBeat: Boolean): QWord;
		function	GetBarLength(ForGraph: Boolean = False; X: Integer = -1): Integer;
		function 	GetBarLengthAt(var P: QWord): Single;

		procedure	Clear;
		procedure	Generate;
		procedure	ColorizeArea(Buffer: TBGRABitmap; P1, P2: QWord; Color: TBGRAPixel);
		procedure	Draw(AWidth: Word = 0; AHeight: Word = 0);
		procedure	DoDraw;

		constructor	Create(var aSong: TSong; const ThemePath: String);
		destructor	Destroy; override;
	end;

const
	ZoneKindNames: array [TZoneKind] of String = ( '', 'loop', 'jump', 'skip', 'end' );

var
	Grays, Grays2: TBGRAPalette;
	AlphaBlack: array[1..10] of TBGRAPixel;


implementation

uses
	Forms, Dialogs,
	Decks.Audio;

{$WARN 5024 off : Parameter "$1" not used}

{$IFDEF DEBUGLOG}
var
	slLog: TStringList;
{$ENDIF}

procedure InitLog;
begin
	{$IFDEF DEBUGLOG}
	slLog := TStringList.Create;
	{$ENDIF}
end;

procedure Log(const S: String; Save: Boolean = False);
begin
	{$IFDEF DEBUGLOG}
	slLog.Add(S);
	if Save then
		slLog.SaveToFile('O:\Projects\Decks\log.txt');
	{$ENDIF}
end;

procedure Audio_Callback_ZoneSync(handle: HSYNC; channel, data: DWord; user: Pointer);
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
	Info: TZoneChangeEventInfo;
begin
	Info := TZoneChangeEventInfo(user^);
	if Assigned(Info.Event) then
		Info.Event(Info.Zone, False);
end;

procedure Audio_Callback_ZoneSync_MixTime(handle: HSYNC; channel, data: DWord; user: Pointer);
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
	Info: TZoneChangeEventInfo;
begin
	Info := TZoneChangeEventInfo(user^);
	if Assigned(Info.Event) then
		Info.Event(Info.Zone, True);
end;

{ TZone }

function CompareZone(A, B: TZone): Integer;
begin
	if A.barindex < B.barindex then
		Result := -1
	else if A.barindex > B.barindex then
		Result := 1
	else
		Result := 0;
end;

constructor TZone.Create;
begin
	inherited;
	Graph := nil;
	Kind := zkNormal;
	ChangeEvent := TZoneChangeEventInfo.Create;
	Sync := 0;
	MixSync := 0;
	Pos := 0;
	{$IFDEF DEBUGLOG} Log('TZone.Create'); {$ENDIF}
end;

destructor TZone.Destroy;
begin
	{$IFDEF DEBUGLOG} Log('TZone.Destroy'); {$ENDIF}
	if Sync <> 0 then
		BASS_ChannelRemoveSync(Graph.Song.OrigStream, Sync);
	if MixSync <> 0 then
		BASS_Mixer_ChannelRemoveSync(Graph.Song.OrigStream, MixSync);
	ChangeEvent.Free;
	inherited Destroy;
end;

function TZone.GetBarLength(ForGraph: Boolean = False):  QWord;
const
	HzDivisor = 7;
var
	Hz: Integer;
begin
	Hz := 44100;
	if ForGraph then Hz := Hz div HzDivisor;

	Result := Trunc(Hz * ((60000 / BPM) / 250));
end;

{ TBeatGraph }

function TBeatGraph.HasZones: Boolean;
begin
	Result := Zones.Count > 1;
end;

procedure TZone.Calculate;
var
	t: Double;
	b: Integer;
begin
	{$IFDEF DEBUGLOG} Log('[TZone.Calculate]');	{$ENDIF}

	t := 60000 / BPM / 250;

	length_bar := Graph.graph_hz * t;
	step := length_bar / Graph.Height;
	length := Min(Trunc(amount_bars * length_bar), Graph.length_audio - Pos);
	if amount_bars = 0 then
	begin
		length := Graph.length_audio - Pos;
		amount_bars := Round(length / length_bar);
	end;

	SetLength(barpos, amount_bars+1);
	for b := 0 to amount_bars do
		barpos[b] := Pos + Trunc(b * length_bar);

	SetLength(ysample, Graph.Height);
	for b := 0 to Graph.Height-1 do
		ysample[b] := Trunc(b * step);

	{$IFDEF DEBUGLOG}
//	Log(Format('   length_bar: %d', [length_bar]));
	Log(Format('   amount_bars: %d', [amount_bars]));
	Log(Format('   barindex:    %d', [barindex]));
	Log(Format('   pos:   %d => %d', [Pos, Graph.GraphToSongBytes(Pos) ]));
	Log(Format('   length:      %d', [length]));
	Log(Format('   barpos[0]:   %d', [barpos[0]]));
	{$ENDIF}
end;

procedure TBeatGraph.ZonesChanged;
var
	Z: TZone;
	i, ZZ: Integer;
	b: Cardinal;
	P, SP: QWord;
begin
	if Generating then Exit;

	EndSyncEvent.Event := OnZoneChange;

	Height := BitmapSize.Y;
	amount_bars := 0;

	{$IFDEF DEBUGLOG}
	Log(Format('[ZonesChanged] height=%d  bars=%d  zones=%d', [Height, amount_bars, Zones.Count]));
	{$ENDIF}

	if (Height < 4) or (Zones.Count < 1) then Exit;

	P := SongToGraphBytes(StartPos);
	Zones.First.Pos := P;
	if Zones.Count = 1 then
		Zones.First.amount_bars := 0;

	for i := 0 to Zones.Count-1 do
	begin
		Z := Zones[i];

		Z.ChangeEvent.Zone := i;
		Z.ChangeEvent.Event := OnZoneChange;

		if i > 0 then
		begin
			//Z.barindex := amount_bars;
			Z.Pos := P;
		end;

		{$IFDEF DEBUGLOG} Log('###  ZONE  ' + i.ToString + '  ###'); {$ENDIF}

		Z.Calculate;
		Inc(amount_bars, Z.amount_bars);
		Inc(P, Z.length);
		SP := GraphToSongBytes(Z.Pos);

		if Z.Sync <> 0 then
			BASS_ChannelRemoveSync(Song.OrigStream, Z.Sync);
        Z.Sync := BASS_ChannelSetSync(Song.OrigStream,
			BASS_SYNC_POS, SP,
			@Audio_Callback_ZoneSync, @Z.ChangeEvent);

		if Z.MixSync <> 0 then
			BASS_Mixer_ChannelRemoveSync(Song.OrigStream, Z.MixSync);
        Z.MixSync := BASS_Mixer_ChannelSetSync(Song.OrigStream,
			BASS_SYNC_POS or BASS_SYNC_MIXTIME, SP,
			@Audio_Callback_ZoneSync_MixTime, @Z.ChangeEvent);
	end;

	{$IFDEF DEBUGLOG}
	Log(Format('Song amount_bars: %d', [amount_bars]), True);
	{$ENDIF}

	SetLength(Bars, amount_bars);

	b := 0; ZZ := 0;
	for Z in Zones do
	begin
		for i := 0 to Z.amount_bars-1 do
		begin
			Bars[b].Pos := Z.barpos[i];
			Bars[b].Zone := ZZ;
			Inc(b);
			if b >= amount_bars then Break;
		end;
		if b >= amount_bars then Break;
		Inc(ZZ);
	end;
end;

procedure TBeatGraph.ZonesLoaded;
begin
	{$IFDEF DEBUGLOG} Log('[ZonesLoaded]', True); {$ENDIF}
	ZonesChanged;
	NeedRecalc := True;
end;

function TBeatGraph.RemoveZone(ZoneIndex: Word): Boolean;
begin
	if (Generating) or (Zones.Count <= 1) or (ZoneIndex >= Zones.Count) then Exit(False);

	{$IFDEF DEBUG}
	ShowMessage(Format('Delete Zone %d (%f BPM)', [ZoneIndex, Zones[ZoneIndex].BPM]));
	{$ENDIF}

	if ZoneIndex < 1 then Exit(False); // todo

	Inc(Zones[ZoneIndex-1].amount_bars, Zones[ZoneIndex].amount_bars);
	Inc(Zones[ZoneIndex-1].length, Zones[ZoneIndex].length);
	Zones.Delete(ZoneIndex);

	ZonesLoaded;
	Result := True;
end;

function TBeatGraph.AddZone(Barpos: QWord; BPM: Single = 0; IsSongPos: Boolean = False): TZone;
var
	I: Integer; // new zone index
	Z: TZone;
	P: QWord;
begin
	if Generating then Exit;

	{$IFDEF DEBUGLOG}
	Log('', False);
	Log(Format('[AddZone] Barpos=%d  BPM=%f', [Barpos, BPM]));
	{$ENDIF}

	if Zones.Count > 0 then
	begin
		if IsSongPos then
		begin
			P := SongToGraphBytes(Barpos * SampleSizeMultiplier);
			{$IFDEF DEBUGLOG}
			Log(Format('Find bar (%d) in range %d - %d', [P, Bars[0].Pos, Bars[amount_bars-1].Pos]));
			{$ENDIF}
			Barpos := amount_bars-1;
			for I := 0 to amount_bars-2 do
			begin
				if (P >= Bars[I].Pos) and (P < Bars[I+1].Pos) then
				begin
					Barpos := I;
					{$IFDEF DEBUGLOG} Log(Format('-> Found: %d', [I])); {$ENDIF}
					Break;
				end;
			end;
		end;

		for Z in Zones do
			if Z.barindex = Barpos then
			begin
				{$IFDEF DEBUGLOG} Log('Zone already exists at location'); {$ENDIF}
				Exit(Z);
			end;
	end;

	Result := TZone.Create;
	Result.Graph := Self;
	Result.BPM := BPM;
	Result.amount_bars := 0;

	if Zones.Count > 0 then
	begin
		{$IFDEF DEBUGLOG} Log('Create normal zone'); {$ENDIF}

		(*
		I := Bars[Barpos].Zone + 1;
		for ZZ := 0 to Zones.Count-2 do
		begin
			if (Zones[ZZ].barindex > Barpos) then
			begin
				I := ZZ;
				Break;
			end;
		end;
		*)

		Result.barindex := Barpos;

		Zones.Add(Result);
		Zones.Sort(@CompareZone);

		I := Zones.IndexOf(Result);

		if BPM < 1 then
		begin
			if I > 0 then
				Result.BPM := Zones[I-1].BPM
			else
				Result.BPM := Song.AvgBPM;
			Result.BPM := Max(60, Result.BPM);
		end;

		if I > 0 then
		begin
			if Result <> Zones.Last then
				Result.amount_bars := Zones[I+1].barindex - Barpos;
			Zones[I-1].amount_bars := Barpos - Zones[I-1].barindex;
		end;

		{$IFDEF DEBUGLOG}
		Log(Format('   * I=%d        Barpos:  %d', [I,Barpos]));
		Log(Format('   * Zones[I-1].barindex: %d', [Zones[I-1].barindex]));
		Log(Format('   * Barpos - Zones[I-1]: %d', [Barpos - Zones[I-1].barindex]));
		{$ENDIF}

		(*
		if I >= Zones.Count-1 then
		begin
			{$IFDEF DEBUGLOG} Log(' -> Add as last'); {$ENDIF}
			Zones.Add(Result);
		end
		else
		begin
			{$IFDEF DEBUGLOG} Log(' -> Insert at ' + I.ToString); {$ENDIF}
			Result.amount_bars := Zones[I+1].barindex - Barpos;
			Zones.Insert(I, Result);
		end;
		*)
	end
	else
	begin
		{$IFDEF DEBUGLOG} Log('Create Main zone'); {$ENDIF}
		if IsSongPos then
			Result.Pos := SongToGraphBytes(Barpos * SampleSizeMultiplier);
		Result.barindex := 0;
		Result.length := length_audio - Result.Pos;
		Zones.Add(Result);
	end;

	ZonesLoaded;
end;

function TBeatGraph.BytesPerSample: Byte;
begin
//	Result := Song.ChannelInfo.chans * 2; // 16-bit
	Result := Song.ChannelInfo.chans * 4; // 32-bit float
end;

function TBeatGraph.GetFreq(ForGraph: Boolean): Word;
begin
	if Generating then
		Result := 0
	else
	if ForGraph then
		Result := graph_hz
	else
		Result := Song.ChannelInfo.freq;
end;

function TBeatGraph.BarToGraph(B: Word): Word;
begin
	if Generating then
		Result := 0
	else
		Result := B * Zoom;
end;

function TBeatGraph.GraphToBar(X: Word): Word;
begin
	if Generating then
		Result := 0
	else
		Result := Min(X div Zoom, High(Bars));
end;

function TBeatGraph.SongToGraphBytes(Pos: QWord): QWord;
begin
	if Generating then
		Result := 0
	else
		Result := Pos div BytesPerSample div HzDivisor;
end;

function TBeatGraph.GraphToSongBytes(Pos: QWord; ForGraph: Boolean = False): QWord;
begin
	if Generating then Exit(0);
	if ForGraph then
		Result := Pos
	else
		Result := Pos * BytesPerSample * HzDivisor;
end;

// returns index of TZone (P is graph byte offset)
function TBeatGraph.GetZoneIndexAt(P: QWord): Word;
var
	i: Integer;
begin
	Result := 0;
	if Generating then Exit;
	if (Zones.Count > 1) and (P >= Zones[0].Pos) then
	begin
		for i := Zones.Count-1 downto 0 do
			if (P >= Zones[i].Pos) then
				Exit(i);
	end;
end;

function TBeatGraph.GetZoneAt(P: QWord): TZone;
begin
	if Generating then
		Result := nil
	else
		Result := Zones[GetZoneIndexAt(P)];
end;

// result in song bytes
function TBeatGraph.GraphToPos(P: TPoint; ForGraph: Boolean = False): QWord;
var
	Z: TZone;
begin
	if (Generating) or (Zoom < 1) or (Zones.Count < 1) then Exit(0);
	Result := Bars[GraphToBar(P.X)].Pos;

	if P.Y > 0 then
	begin
		Z := GetZoneAt(Result);
		P.Y := Min(P.Y, High(Z.ysample));
		Inc(Result, Z.ysample[P.Y]);
	end;

	Result := GraphToSongBytes(Result, ForGraph);
end;

function TBeatGraph.PosToBar(P: QWord; ForGraph: Boolean = False): Integer;
var
	Bar: Integer;
begin
	if not Generating then
	begin
		if not ForGraph then
			P := SongToGraphBytes(P + StartPos);
		for Bar := amount_bars-1 downto 0 do
			if (P >= Bars[Bar].Pos) then // and (P <= Bars[Bar+1].Pos) then
				Exit(Bar);
	end;
	Result := -1;
end;

// P = song byte pos, returns 0..1 (multiplier for length_bar)
function TBeatGraph.GetYPos(P: QWord): Single;
var
	Bar: Integer;
	GP: QWord;
	BL: Single;
begin
	Result := 0;
	if (Generating) or (P = 0) then Exit;

	GP := P;
	BL := GetBarLengthAt(GP);
	if BL <= 0.1 then Exit;

	Bar := PosToBar(GP, True);
	if Bar >= 0 then
		Result := ((GP - Bars[Bar].Pos) / BL);
end;

// P = song byte pos, changes to graph byte pos
function TBeatGraph.GetBarLengthAt(var P: QWord): Single;
var
	Zone: TZone;
begin
	if Generating then Exit(0);
	P := SongToGraphBytes(P);
	Zone := GetZoneAt(P);
	if Zone <> nil then
		Result := Zone.length_bar
	else
		Result := 0;
end;

function TBeatGraph.PosToGraph(P: QWord; ForGraph: Boolean = False): TPoint;
var
	Bar: Integer;
	Zone: TZone;
begin
	Result := TPoint.Zero;
	if (Generating) or (P = 0) then Exit;

	if not ForGraph then
		P := SongToGraphBytes(P + StartPos);

	Bar := PosToBar(P, True);
	if Bar < 0 then Exit;

	Result.X := BarToGraph(Bar);
	Zone := GetZoneAt(P);
	if Zone <> nil then
		Result.Y := Trunc((P - Bars[Bar].Pos) / Zone.length_bar * Height) mod Height;
end;

// result in samples, not bytes! TODO zones
function TBeatGraph.GetBeatLength(ForGraph: Boolean = False): Integer;
begin
	if Generating then Exit(0);
	Result := Trunc(GetFreq(ForGraph) * ((60000 / Song.AvgBPM) / 1000));
end;

function TBeatGraph.GetBarLength(ForGraph: Boolean = False; X: Integer = -1): Integer;
begin
	if Generating then Exit(0);
	if (X >= 0) and (X < High(Bars)) then
		Result := GraphToSongBytes(Bars[X+1].Pos - Bars[X].Pos, ForGraph)
	else
		Result := Trunc(GetFreq(ForGraph) * ((60000 / Song.AvgBPM) / 250));
end;

function TBeatGraph.GetNextBeat(P: QWord; out IsFirstBeat: Boolean): QWord;
var
	Y, Bar, BL: Integer;
	BarPos, BarPos2: QWord;
begin
	Result := 0;

	Bar := PosToBar(P, True);
	if Bar < 0 then Exit;

	BL := Trunc(GetBarLength(True, Bar) / 4);
	if BL <= 0 then Exit;

	BarPos  := Bars[Bar].Pos;
	BarPos2 := BarPos + GetBarLength(True, Bar);
	Result := BarPos;

	for Y := 0 to 7 do
	begin
		if Result >= P then
		begin
			IsFirstBeat := (Abs(Result - BarPos) < 100) or (Abs(Result - BarPos2) < 100);
			Exit;
		end;
		Inc(Result, BL);
	end;
end;

procedure TBeatGraph.Clear;
begin
	if Generating then Exit;
	Zones.Clear;
	StartPos := 0;
	{$IFDEF DEBUGLOG} slLog.Clear; {$ENDIF}
end;

constructor TBeatGraph.Create(var aSong: TSong; const ThemePath: String);
const
	Alphas: array[1..10] of Byte = ( 0,0,30,60,90, 160,190,200,230,255 );
var
	i: Integer;
	st: Single;
	Fn: String;
	ExtPal: Boolean;
begin
	Drawing   := False;
	QueueDraw := False;
	StartPos := 0;
	Zoom := 1;
	LoadChunkSize := 1024 * 128;

	InitLog;

	Bitmap := TBGRABitmap.Create;
	Fn := IncludeTrailingPathDelimiter(ThemePath) + 'graph.png';

	if FileExists(Fn) then
	begin
		Bitmap.LoadFromFile(Fn);
		ExtPal := (Bitmap.Width >= 256);
	end
	else
		ExtPal := False;

	if ExtPal then
	begin
		st := (Bitmap.Width-0) / 256;
		for i := 0 to 255 do
		begin
			Grays[i] := Bitmap.GetPixel(i*st, 0);
			Grays2[i] := Grays[i];
			Grays2[i].red := Grays2[i].red div 2;
			Grays2[i].blue := Grays2[i].blue div 2;
		end;
	end
	else
	begin
		for i := 0 to 255 do
			Grays[i] := BGRA(i, Trunc(i*0.92), Trunc(i*0.85));
	end;

	for i := 1 to 10 do
		AlphaBlack[i] := BGRA(0, 0, 0, Alphas[i]);

	Bitmap.Fill(ColorToBGRA($000000));

	Zones := TFPGObjectList<TZone>.Create;

	EndSyncEvent := TZoneChangeEventInfo.Create;
	EndSyncEvent.Zone := ZONECHANGE_STOPPED; // song finished playing
	EndSyncEvent.Event := OnZoneChange;

	OnGraphIteration := nil;
	Song := aSong;
end;

destructor TBeatGraph.Destroy;
begin
	BASS_Mixer_ChannelRemoveSync(Song.OrigStream, EndSync);
	EndSyncEvent.Free;

	Bitmap.Free;
	Zones.Free;
	inherited Destroy;
end;

procedure TBeatGraph.Generate;
var
	GraphStream: HSTREAM;
	i, s, m, Percentage, OldPercentage: Integer;
	p, maxlen, len: QWord;
	sam: PByte;
begin
	if Generating then Exit;
	Generating := True;

	case Song.ChannelInfo.freq of
		11025: HzDivisor := 1;
		22050: HzDivisor := 4;
		44100: HzDivisor := 7;
		48000: HzDivisor := 8;
		96000: HzDivisor := 16;
	else
		HzDivisor := 7;
	end;

	graph_hz := Song.ChannelInfo.freq div HzDivisor;
	BitmapSize := TPoint.Zero;

	GraphStream := BASS_Mixer_StreamCreate(graph_hz, 1, BASS_STREAM_DECODE or BASS_SAMPLE_8BITS);
	BASS_Mixer_StreamAddChannel(GraphStream, Song.OrigStream,
		BASS_MIXER_DOWNMIX or BASS_MIXER_NORAMPIN);

	maxlen := TranslateStreamLength(Song.OrigStream, GraphStream) + LoadChunkSize;
	SetLength(AudioData, maxlen);

	p := 0;
	m := 0; // highest sample value
	OldPercentage := 0;
	sam := @AudioData[0];
	repeat
		len := BASS_ChannelGetData(GraphStream, sam, LoadChunkSize);
		Inc(p, len);
		for i := 1 to len do
		begin
			s := Min(255, Abs(sam^ - 128) * 2);
			sam^ := s;
			if s > m then m := s;
			Inc(sam);
		end;

		Percentage := Round(p / maxlen * 100);
		if Percentage <> OldPercentage then
		begin
			OldPercentage := Percentage;
			if Assigned(OnLoadProgress) then
				OnLoadProgress(Percentage);
		end;

	until len < LoadChunkSize;

	//SetLength(AudioData, p);
	length_audio := p;
	CalcBrightness := 256 / m;
	Brightness := CalcBrightness;

	BASS_StreamFree(GraphStream);

	BASS_ChannelSetPosition(Song.OrigStream, 0, BASS_POS_BYTE);

	// using BASS_Mixer_ChannelSetSync here causes the sync to not trigger for some reason
	EndSync := BASS_ChannelSetSync(Song.OrigStream,
		BASS_SYNC_END or BASS_SYNC_MIXTIME, 0,
		@Audio_Callback_ZoneSync_MixTime, @EndSyncEvent);

	Generating := False;
end;

procedure TBeatGraph.ColorizeArea(Buffer: TBGRABitmap; P1, P2: QWord; Color: TBGRAPixel);

	function FixX(X: Integer): Integer; inline;
	begin
		Result := X * Zoom - Scroll.X;
	end;

var
	PS, PE: TPoint;
	R: TRect;
	I, W: Integer;
begin
	PS := PosToGraph(P1 - StartPos, False); // get pixel coords from song byte position
	PE := PosToGraph(P2 - StartPos, False);
	PS.X := PS.X div Zoom;
	PE.X := PE.X div Zoom;
	if PE.Y < 1 then
	begin
		PE.X := PE.X - 1;
		PE.Y := Height;
	end;
	W := PE.X - PS.X;
//	PS.X := PS.X - Scroll.X; // start pixel coords
//	PE.X := PE.X - Scroll.X; // end pixel coords
	PS.X := FixX(PS.X);

	if W <= 0 then
	begin
		R := Rect(PS.X, PS.Y, PS.X + Zoom, PE.Y);
		Buffer.FillRect(R, Color, dmLinearBlend);
	end
	else
	begin
		PE.X := FixX(PE.X);

		R := Rect(PS.X, PS.Y, PS.X + Zoom, Height);
		Buffer.FillRect(R, Color, dmLinearBlend);

		if W > 2 then
		begin
			R := Rect(PS.X+Zoom, 0, PE.X, Height);
			Buffer.FillRect(R, Color, dmLinearBlend);
		end;

		R := Bounds(PE.X, 0, Zoom, PE.Y);
		Buffer.FillRect(R, Color, dmLinearBlend);
	end;
end;

procedure TBeatGraph.Draw(AWidth: Word = 0; AHeight: Word = 0);
begin
	if Generating then Exit;
	if not Drawing then
	begin
		if (AWidth + AHeight > 0) then
		begin
			if (AWidth <> BitmapSize.X) or (AHeight <> BitmapSize.Y) then
			begin
				BitmapSize := Point(AWidth, AHeight);
				NeedRecalc := True;
			end;
		end;
		DoDraw;
	end
	else
	begin
		Drawing := False;
		QueueDraw := True;
	end;
end;

procedure TBeatGraph.DoDraw;
var
	i, BB, X1, X2: Integer;
	b: Cardinal;
	Z: TZone;

	procedure DrawBar(Z: TZone; bar, X, W: Word);
	var
		avg: QWord;
		px, Y, H: Integer;
		Bri: Single;
		col: TBGRAPixel;
		PP: PBGRAPixel;
		bp: PByte;
	begin
		if X >= Width then Exit;

		// don't attempt to process past audio data
		H := Height-1;
		if (Z.barpos[bar] + Z.ysample[H]) > length_audio then
		begin
			avg := Z.barpos[bar];
			for Y := 0 to Height-1 do
			begin
				avg += Z.ysample[Y];
				if avg >= length_audio then
				begin
					H := Max(0, Y-1);
					Break;
				end;
			end;
		end;

		// don't write pixels out of bounds
		if X + W >= Width then
			W := Width - X - 1;

		if (W < 1) or (Z.barpos[bar] + Z.length_bar >= length_audio) then Exit;

		Bri := Brightness * 2;

		// draw
		for Y := 0 to H do
		begin
			bp := @AudioData[Z.barpos[bar] + Z.ysample[Y]];
			avg := 0;
			for px := 0 to Trunc(Z.step) do // average samples for current pixel
			begin
				avg := avg + bp^;
				Inc(bp);
			end;
			col := Grays[Min(((Trunc(avg * Bri / Z.step))), 255)];

			//Bitmap.HorizLine(X, Y, X+W, col);

			PP := PBGRAPixel(Bitmap.GetPixelAddress(X, Y));
			for px := 1 to W do // horizontal zoom
			begin
				PP^ := col; // write pixel
				Inc(PP);
			end;
		end;
		px := Min(W, 10);
		if AlphaBlack[px].alpha > 0 then
			Bitmap.VertLine(X+W{%H-}-1, 0, H, AlphaBlack[px]);
	end;

begin
	if Generating then Exit;
	QueueDraw := False;

	if (Song = nil) or (not Song.Loaded) or (Song.AvgBPM <= 1) then Exit;
	if (BitmapSize.X <= 0) or (BitmapSize.Y <= 0) then Exit;

	if (NeedRecalc) or (ZoomChanged) then
	begin
		if NeedRecalc then
		begin
			ZonesChanged;
			NeedRecalc := False;
		end;

		Zoom := 1;
		if amount_bars < 1 then Exit;
		while (amount_bars * Zoom) {%H-}< BitmapSize.X do
			Inc(Zoom);
		Inc(Zoom, WantedZoom-1);

		Width := amount_bars * Zoom;
		Height := BitmapSize.Y;
		Bitmap.SetSize(Width, Height);

		ZoomChanged := False;
	end;

	Bitmap.Fill(BGRABlack);

	if amount_bars > 0 then
	begin
		Drawing := True;
		i := 0;
		BB := 0;
		for Z in Zones do
		begin
			b := 0;
			Z.barindex := BB;
			X1 := i;
			while (Drawing) and (b < Z.amount_bars) and (i < Width) do
			begin
				DrawBar(Z, b, i, Zoom);
				Inc(i, Zoom);
				Inc(b);
				Inc(BB);
			end;
			X2 := i;

			if not Drawing then Break;

			case Z.Kind of
				zkSkip:
					Bitmap.FillRect(X1, 0, X2, Height-1, BGRA(255,0,0), dmSet, 65535 div 3);
				zkEnd:
					Bitmap.FillRect(X1, 0, X2, Height-1, BGRABlack, dmSet, 65535 div 3);
			end;
		end;
	end;

	Drawing := False;
end;

end.

