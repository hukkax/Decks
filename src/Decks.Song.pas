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

	MODE_LOAD_START   = 20;
	MODE_LOAD_SUCCESS = 21;
	MODE_LOAD_FAILURE = 22;
	MODE_LOAD_GRAPH   = 23;
	MODE_LOAD_FINISH  = 29;

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
		OrigBPM:	Single;
		ByteLength: QWord;
		Duration:	Single;
		Bitrate:	Word;

		BeatFadeCounter,
		BeatPreviousQuadrant: Byte;

		OrigStream: HSTREAM;
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


implementation

uses
	Decks.Audio;

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
	FreeStream(OrigStream);
	FreeStream(Reverse.Stream);

	UF := UnicodeString(AFilename);

	// load song from file
	OrigStream := BASS_StreamCreateFile(False, PWideChar(UF), 0, 0,
		BASS_STREAM_DECODE or BASS_STREAM_PRESCAN or
		BASS_SAMPLE_FLOAT or BASS_UNICODE);
	if OrigStream = 0 then Exit(False);

	BASS_ChannelGetInfo(OrigStream, ChannelInfo);
	ByteLength := BASS_ChannelGetLength(OrigStream, BASS_POS_BYTE);
	Duration := BASS_ChannelBytes2Seconds(OrigStream, ByteLength);
	BASS_ChannelGetAttribute(OrigStream, BASS_ATTRIB_BITRATE, TmpBitrate);
	Bitrate := Trunc(TmpBitrate);

	// resample and downmix to 44100 Hz 16-bit Stereo
	FreeStream(Stream);
	Stream := BASS_Mixer_StreamCreate(44100, 2, 0);//BASS_STREAM_DECODE);
	BASS_ChannelSetAttribute(Stream, BASS_ATTRIB_BUFFER, 0); // disable playback buffering


	// generate the graph externally here before plugging the
	// stream into the final output mixer
	ModeChange(MODE_LOAD_GRAPH);

//	EndSync := BASS_Mixer_ChannelSetSync(OrigStream, BASS_SYNC_END, 0,
//		@Audio_Callback_EndSync, @Self);

	F := BASS_STREAM_DECODE or BASS_SAMPLE_LOOP or BASS_FX_FREESOURCE;
	Reverse.Stream := BASS_FX_ReverseCreate(OrigStream, 1.0, F);
	OrigStream := Reverse.Stream;

	BASS_Mixer_StreamAddChannel(Stream, OrigStream,
		BASS_MIXER_DOWNMIX or (*BASS_MIXER_BUFFER or*) BASS_MIXER_NORAMPIN);

	OrigBPM  := 0;
	OrigFreq := 44100; //ChannelInfo.freq;
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


end.

