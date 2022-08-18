unit Decks.Beatgraph;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils, Types, Math, FGL,
	BGRABitmap, BGRABitmapTypes,
	BASS, BASSMIX,
	Decks.Song;

const
	ZONECHANGE_STOPPED = -2;
	ZONECHANGE_GETPOS  = -1;

	Iterations = 3;

type
	TBeatGraph = class; // forward

	TGraphIterationEvent = procedure of Object;

	TZoneChangeEvent = procedure(Zone: Integer) of Object;

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
		step:			Single;
		length,
		amount_bars:	Cardinal;
		length_bar:		Single;
		barindex:		Word;
		Sync:			HSYNC;
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
		BitmapSize:			TPoint;
		Bitmap:				TBGRABitmap; //TBitmap32;
		Scroll:				TPoint;
		Brightness,
		CalcBrightness:		Single;
		length_audio:		QWord;
		amount_bars:		Cardinal;
		StartPos:			QWord; // song pos
		NeedRecalc,
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
		function	GetBarLength(ForGraph: Boolean = False; X: Integer = -1): Integer;
		function 	GetBarLengthAt(var P: QWord): Single;

		procedure	Clear;
		procedure	Generate;
		procedure	Draw(AWidth: Word = 0; AHeight: Word = 0);
		procedure	DoDraw;

		constructor	Create(var aSong: TSong; const ThemePath: String);
		destructor	Destroy; override;
	end;

var
	Grays: array[0..255] of TBGRAPixel;
	AlphaBlack: array[1..10] of TBGRAPixel;


implementation

uses
	Dialogs,
	Decks.Audio;

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
		Info.Event(Info.Zone);
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
	ChangeEvent := TZoneChangeEventInfo.Create;
	Sync := 0;
	Pos := 0;
	{$IFDEF DEBUGLOG} Log('TZone.Create'); {$ENDIF}
end;

destructor TZone.Destroy;
begin
	{$IFDEF DEBUGLOG} Log('TZone.Destroy'); {$ENDIF}
	if Sync <> 0 then
		BASS_Mixer_ChannelRemoveSync(Graph.Song.OrigStream, Sync);
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
	t: Single;
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
		amount_bars := Trunc(length / length_bar);
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
	i, b, ZZ: Integer;
	P: QWord;
begin
	{$IFDEF DEBUGLOG}
	Log('[ZonesChanged]');
	{$ENDIF}

	EndSyncEvent.Event := OnZoneChange;

	Height := BitmapSize.Y;
	amount_bars := 0;

	{$IFDEF DEBUGLOG}
	Log(Format('   height=%d  bars=%d  zones=%d', [Height, amount_bars, Zones.Count]));
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

		if Z.Sync <> 0 then
			BASS_Mixer_ChannelRemoveSync(Song.OrigStream, Z.Sync);

        Z.Sync := BASS_Mixer_ChannelSetSync(Song.OrigStream,
			BASS_SYNC_POS, GraphToSongBytes(Z.Pos),
			@Audio_Callback_ZoneSync, @Z.ChangeEvent);
	end;

	{$IFDEF DEBUGLOG}
	Log(Format('Song amount_bars: %d', [amount_bars]), True);
	{$ENDIF}

	SetLength(Bars, amount_bars+1);

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
var
	X, BI: Integer;
	P: QWord;
begin
	if (Zones.Count <= 1) or (ZoneIndex >= Zones.Count) then Exit(False);

	ShowMessage(Format('Delete Zone %d (%f BPM)', [ZoneIndex, Zones[ZoneIndex].BPM]));

	P := Zones[ZoneIndex].Pos;
	BI := Zones[ZoneIndex].barindex;
	for X := BI to amount_bars do
		Bars[X].Zone -= 1;
	Zones.Delete(ZoneIndex);
	Zones[ZoneIndex].Pos := P;

	ZonesLoaded;
	Result := True;
end;

function TBeatGraph.AddZone(Barpos: QWord; BPM: Single = 0; IsSongPos: Boolean = False): TZone;
var
	I: Integer; // new zone index
	Z: TZone;
	P: QWord;
begin
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
				Result.BPM := Song.OrigBPM;
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
	if ForGraph then
		Result := graph_hz
	else
		Result := Song.ChannelInfo.freq;
end;

function TBeatGraph.BarToGraph(B: Word): Word;
begin
	Result := B * Zoom;
end;

function TBeatGraph.GraphToBar(X: Word): Word;
begin
	Result := Min(X div Zoom, High(Bars));
end;

function TBeatGraph.SongToGraphBytes(Pos: QWord): QWord;
begin
	Result := Pos div BytesPerSample div HzDivisor;
end;

function TBeatGraph.GraphToSongBytes(Pos: QWord; ForGraph: Boolean = False): QWord;
begin
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
	if (Zones.Count > 1) and (P >= Zones[0].Pos) then
	begin
		for i := Zones.Count-1 downto 0 do
			if (P >= Zones[i].Pos) then
				Exit(i);
	end;
end;

function TBeatGraph.GetZoneAt(P: QWord): TZone;
begin
	Result := Zones[GetZoneIndexAt(P)];
end;

// result in song bytes
function TBeatGraph.GraphToPos(P: TPoint; ForGraph: Boolean = False): QWord;
var
	Z: TZone;
begin
	if Zoom < 1 then Exit(0);

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
	if not ForGraph then
		P := SongToGraphBytes(P + StartPos);
	for Bar := amount_bars-1 downto 0 do
		if (P >= Bars[Bar].Pos) then // and (P <= Bars[Bar+1].Pos) then
			Exit(Bar);
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
	if P > 0 then
	begin
		GP := P;
		BL := GetBarLengthAt(GP);
		if BL > 0 then
		begin
			Bar := PosToBar(GP, True);
			if Bar >= 0 then
				Result := ((GP - Bars[Bar].Pos) / BL);
		end;
	end;
end;

// P = song byte pos, changes to graph byte pos
function TBeatGraph.GetBarLengthAt(var P: QWord): Single;
var
	Zone: TZone;
begin
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
	if P = 0 then Exit;

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
	Result := Trunc(GetFreq(ForGraph) * ((60000 / Song.OrigBPM) / 1000));
end;

function TBeatGraph.GetBarLength(ForGraph: Boolean = False; X: Integer = -1): Integer;
begin
	if (X >= 0) and (X < High(Bars)) then
		Result := GraphToSongBytes(Bars[X+1].Pos - Bars[X].Pos, ForGraph)
	else
		Result := Trunc(GetFreq(ForGraph) * ((60000 / Song.OrigBPM) / 250));
end;

procedure TBeatGraph.Clear;
begin
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
			Grays[i] := Bitmap.GetPixel(i*st, 0);
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
	i, s, m: Integer;
	p, len: QWord;
	sam: PByte;
begin
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

	len := TranslateStreamLength(Song.OrigStream, GraphStream);

	SetLength(AudioData, len);
	p := BASS_ChannelGetData(GraphStream, @AudioData[0], len);

	SetLength(AudioData, p);
	length_audio := p;
	sam := @AudioData[0];
	m := 0; // highest sample value

	for i := 0 to High(AudioData) do
	begin
		s := Min(255, Abs(sam^ - 128) * 2);
		sam^ := s;
		if s > m then m := s;
		Inc(sam);
	end;

	CalcBrightness := 256 / m;
	Brightness := CalcBrightness;

	BASS_StreamFree(GraphStream);

	BASS_ChannelSetPosition(Song.OrigStream, 0, BASS_POS_BYTE);

	// using BASS_Mixer_ChannelSetSync here causes the sync to not trigger for some reason
	EndSync := BASS_ChannelSetSync(Song.OrigStream,
		BASS_SYNC_END or BASS_SYNC_MIXTIME, 0,
		@Audio_Callback_ZoneSync, @EndSyncEvent);
end;

procedure TBeatGraph.Draw(AWidth: Word = 0; AHeight: Word = 0);
begin
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
	i, BB: Integer;
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
			Bitmap.VertLine(X+W-1, 0, H, AlphaBlack[px]);
	end;

begin
	QueueDraw := False;

	if (Song = nil) or (not Song.Loaded) or (Song.OrigBPM <= 1) then Exit;
	if (BitmapSize.X <= 0) or (BitmapSize.Y <= 0) then Exit;

	if NeedRecalc then
	begin
		ZonesChanged;
		NeedRecalc := False;

		Zoom := 1;
		if amount_bars < 1 then Exit;
		while (amount_bars * Zoom) < BitmapSize.X do
			Inc(Zoom);
		Inc(Zoom, WantedZoom-1);

		Width := amount_bars * Zoom;
		Bitmap.SetSize(Width, Height);
	end;

//	Bitmap.BeginUpdate;
//	Bitmap.Clear(clBlack32);
	Bitmap.Fill(ColorToBGRA($000000));

	if amount_bars > 0 then
	begin
		Drawing := True;
		i := 0;
		//ZZ := 0;
		BB := 0;
		for Z in Zones do
		begin
			b := 0;
			Z.barindex := BB;
			while (b < Z.amount_bars) and (i < Width) do
			begin
				DrawBar(Z, b, i, Zoom);
//				Bars[BB].Zone := ZZ;
				Inc(i, Zoom);
				Inc(b);
				Inc(BB);
			end;
//			Inc(ZZ);
		end;
	end;

//	Bitmap.EndUpdate;
	Drawing := False;
end;

end.

