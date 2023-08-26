unit Decks.Song;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils,
	BASS, BASSMIX, BASS_FX; //, basszxtune;

const
	MODE_PLAY_STOP    = 0;
	MODE_PLAY_START   = 1;
	MODE_PLAY_PAUSE   = 2;
	MODE_PLAY_FAILURE = 3;
	MODE_PLAY_WAITSYNC= 4;
	MODE_BEND_STOP    = 10;
	MODE_BEND_START   = 11;
	MODE_TEMPOCHANGE  = 13;
	MODE_SYNC_ON      = 16;
	MODE_SYNC_OFF     = 17;
	MODE_EQ_KILL_ON   = 18;
	MODE_EQ_KILL_OFF  = 19;
	MODE_LOOP_ON      = 20;
	MODE_LOOP_OFF     = 21;
	MODE_CUE_ON       = 22;
	MODE_CUE_OFF      = 23;

	MODE_LOAD_START   = 30;
	MODE_LOAD_SUCCESS = 31;
	MODE_LOAD_FAILURE = 32;
	MODE_LOAD_GRAPH   = 33;
	MODE_LOAD_FINISH  = 39;

type
	TSongModeEvent = procedure(Kind: Integer) of object;

	TSong = class
	protected
		EndSync:	HSYNC;
		procedure	ModeChange(Kind: Integer); inline;
	public
		Loaded:		Boolean;
		Paused:		Boolean;
		Reverse: record
			Enabled,
			Temporary: Boolean;
			StartPos: QWord;
			Stream: HSTREAM;
		end;

		OrigFreq,
		PlayFreq:	Cardinal;
		BPM,
		AvgBPM:		Single;
		ByteLength: QWord;
		Duration:	Single;
		Bitrate:	Word;

		BeatFadeCounter,
		BeatPreviousQuadrant: Byte;

		FileStream,
		OrigStream,
		Stream: 	HSTREAM;

		OnModeChange: TSongModeEvent;

		ChannelInfo: BASS_CHANNELINFO;

		Filename:	String;

		function	Load(const AFilename: String): Boolean; virtual;
		procedure	SetReverse(Reversed, KeepSync: Boolean);

		procedure	Play;
		procedure	Stop;
		procedure	Pause;

		constructor	Create; virtual;
		destructor	Destroy; override;
	end;

	TFileCue = record
	private
		Playing:    Boolean;
		FileStream,
		OutStream:  HSTREAM;
		Filename:   String;
	public
		function  Start(const AFilename: String): Boolean;
		function  Stop: Boolean;
		procedure UnInit;
	end;

var
	FileCue: TFileCue;


implementation

uses
	Decks.Audio, Decks.Config;

//
// Callbacks
//

procedure Audio_Callback_EndSync(handle: HSYNC; channel, data, user: Pointer);
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
	if user <> nil then
		TSong(user^).Stop;
end;

{ TSong }

//
// Utility
//

procedure TSong.ModeChange(Kind: Integer); inline;
begin
	if Assigned(OnModeChange) then
		OnModeChange(Kind);
end;

//
// Init
//

constructor TSong.Create;
begin
	inherited;
	Loaded := False;
end;

destructor TSong.Destroy;
begin
	inherited Destroy;
end;

//
// File I/O
//

function TSong.Load(const AFilename: String): Boolean;
var
	UF: UnicodeString;
	F: Cardinal;
	TmpBitrate: Single;
begin
	Stop;
	FreeStream(FileStream);
	FreeStream(OrigStream);

	UF := UnicodeString(AFilename);

	// load song from file
	OrigStream := BASS_StreamCreateFile(False, PWideChar(UF), 0, 0,
		BASS_STREAM_DECODE or BASS_STREAM_PRESCAN or
		BASS_SAMPLE_FLOAT or BASS_UNICODE);
	if OrigStream = 0 then Exit(False);

	BASS_ChannelGetInfo(OrigStream, ChannelInfo);
	ByteLength := BASS_ChannelGetLength(OrigStream, BASS_POS_BYTE);
	Duration := BASS_ChannelBytes2Seconds(OrigStream, ByteLength);
	BASS_ChannelGetAttribute(OrigStream, BASS_ATTRIB_BITRATE, TmpBitrate{%H-});
	Bitrate := Trunc(TmpBitrate);

	// resample and downmix
	FreeStream(Stream);
	Stream := BASS_Mixer_StreamCreate(Config.Audio.Hz, 4, BASS_MIXER_RESUME);
	BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_BUFFER, 0); // disable playback buffering

	// generate the graph externally here before plugging the
	// stream into the final output mixer
	ModeChange(MODE_LOAD_GRAPH);

//	EndSync := BASS_Mixer_ChannelSetSync(OrigStream, BASS_SYNC_END, 0,
//		@Audio_Callback_EndSync, @Self);

	F := BASS_STREAM_DECODE or BASS_SAMPLE_LOOP or BASS_FX_FREESOURCE;
	Reverse.Stream := BASS_FX_ReverseCreate(OrigStream, 1.0, F);

	FileStream := OrigStream;
	OrigStream := Reverse.Stream;

	BASS_Mixer_StreamAddChannel(Stream, OrigStream,
		BASS_MIXER_CHAN_MATRIX {or BASS_MIXER_DOWNMIX} or BASS_MIXER_NORAMPIN);

	BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_MIXER_THREADS, Config.Audio.Threads);

	AvgBPM   := 0;
	OrigFreq := Config.Audio.Hz; //ChannelInfo.freq;
	PlayFreq := OrigFreq;

	Loaded := True;
	Result := Loaded;
	Paused := True;
	SetReverse(False, False);
end;

procedure TSong.SetReverse(Reversed, KeepSync: Boolean);
var
	P: QWord;
begin
	if not Loaded then Exit;
	Reverse.Enabled := Reversed;
	Reverse.Temporary := True;
	if Reversed then
	begin
		if KeepSync then
			Reverse.StartPos := BASS_Mixer_ChannelGetPosition(Reverse.Stream, BASS_POS_BYTE);
		BASS_ChannelSetAttribute(Reverse.Stream, BASS_ATTRIB_REVERSE_DIR, BASS_FX_RVS_REVERSE);
	end
	else
	begin
		BASS_ChannelSetAttribute(Reverse.Stream, BASS_ATTRIB_REVERSE_DIR, BASS_FX_RVS_FORWARD);
		if KeepSync then
		begin
			P := BASS_Mixer_ChannelGetPosition(Reverse.Stream, BASS_POS_BYTE);
			BASS_Mixer_ChannelSetPosition(Reverse.Stream, Reverse.StartPos + Abs(Reverse.StartPos - P), BASS_POS_BYTE);
		end;
	end;
end;

procedure TSong.Play;
begin
	if not Loaded then Exit;

	Paused := False;
	BeatPreviousQuadrant := 255;

	if BASS_ChannelPlay(Stream, True) then
		ModeChange(MODE_PLAY_START)
	else
		ModeChange(MODE_PLAY_FAILURE);
end;

procedure TSong.Stop;
begin
	if not Loaded then Exit;

	Paused := True;
	BASS_ChannelStop(Stream);
	ModeChange(MODE_PLAY_STOP);
end;

procedure TSong.Pause;
begin
	if not Loaded then Exit;

	Paused := not Paused;
	if Paused then
	begin
		BASS_ChannelPause(Stream);
		ModeChange(MODE_PLAY_PAUSE);
	end
	else
		Play;
end;

{ TFileCue }

function TFileCue.Start(const AFilename: String): Boolean;
var
	UFilename: UnicodeString;
begin
	Result := False;

	if AFilename <> Filename then
	begin
		if (AFilename.IsEmpty) or (not FileExists(AFilename)) then Exit;

		UnInit;
		Filename := AFilename;
		UFilename := UnicodeString(AFilename);

		FileStream := BASS_StreamCreateFile(False, PWideChar(UFilename), 0, 0,
			BASS_STREAM_DECODE or BASS_SAMPLE_FLOAT or BASS_UNICODE or BASS_SAMPLE_LOOP);
		if FileStream = 0 then Exit;

		OutStream := BASS_Mixer_StreamCreate(Config.Audio.Hz, 4, BASS_MIXER_RESUME);
		BASS_ChannelSetAttribute(OutStream, BASS_ATTRIB_BUFFER, 0);
		BASS_Mixer_StreamAddChannel(OutStream, FileStream, BASS_MIXER_CHAN_MATRIX);
	end;

	if OutStream <> 0 then
	begin
		ApplyMixingMatrix(FileStream, True, 1, 0, 0);
		Playing := BASS_ChannelPlay(OutStream, True);
	end;
	Result := Playing;
end;

function TFileCue.Stop: Boolean;
begin
	Result := Playing;
	if Result then
		BASS_ChannelStop(OutStream);
	Playing := False;
end;

procedure TFileCue.UnInit;
begin
	FreeStream(FileStream);
	FreeStream(OutStream);
end;

initialization

	FileCue := Default(TFileCue);

finalization

	FileCue.Uninit;

end.

