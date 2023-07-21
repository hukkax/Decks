unit Decks.Audio;

{$mode Delphi}

interface

uses
	Classes, SysUtils, FGL, BASS;

const
	OUTPUTS: array[0..3] of Integer = (
		BASS_SPEAKER_FRONT, BASS_SPEAKER_REAR, BASS_SPEAKER_CENLFE, BASS_SPEAKER_REAR2);

type
	TSpeakerAssignmentInfo = record
		SpeakerFlags: DWord;
		Caption: String;
	end;

	TAudioDeviceInfo = record
		Initialized: Boolean;
		MinRate,			// The minimum sample rate supported by the hardware
		MaxRate,			// The maximum sample rate supported by the hardware
		MinBuffer,			// The minimum buffer length for the BASS_CONFIG_BUFFER config option
		Latency,			// The average delay for playback of HSTREAM/HMUSIC channels
		Outputs,			// The # of speakers which can be accessed via speaker assignment flags
		Frequency: DWord;	// The device's current output sample rate
	end;

	TAudioDevice = class
		Index:       Word;   // Device index
		IsDefault:  Boolean; // Is this the system default output device?
		Name:       String;  // Description of the device
		Filename:   String;  // The filename of the driver
		Speakers:   Byte;    // Index of SpeakerAssignmentInfo[]
		DeviceInfo: TAudioDeviceInfo;
	end;

	TAudioManager = class
	public
		Devices:	TFPGObjectList<TAudioDevice>;
		DefaultDeviceIndex: Integer;
		BASSVersion: String;

		function	InitDevice(WinHandle: QWord; Device: Integer = -1): Boolean;
		function	InitPlugins(const Path: String): Integer;

		constructor	Create;
		destructor	Destroy; override;
	end;


	function  StreamLengthInSeconds(Stream: HSTREAM): Double;
	function  TranslateStreamLength(OriginalStream, MixerStream: HSTREAM): QWord;
	procedure FreeStream(Stream: HSTREAM);

var
	SpeakerAssignmentInfo: array [0..11] of TSpeakerAssignmentInfo;

implementation

uses
	FileUtil,
	Decks.Config;
	//basszxtune;

procedure SetSpeakerAssignmentInfo(Index: Byte; Spk: DWord; const ACaption: String);
begin
	if Index <= 11 then
	with SpeakerAssignmentInfo[Index] do
	begin
		SpeakerFlags := Spk;
		Caption := ACaption;
	end;
end;

function StreamLengthInSeconds(Stream: HSTREAM): Double;
var
	len: QWord;
	Flags: DWord;
begin
	Flags := BASS_POS_BYTE;
	//if BASS_ChannelGetLength(Stream, BASS_POS_ZXTUNE_SUB_COUNT) > 0 then
	//	Flags := Flags or BASS_POS_ZXTUNE_SUB_LENGTH; // only decode first subtune

	len := BASS_ChannelGetLength(Stream, Flags);
	Result := BASS_ChannelBytes2Seconds(Stream, len);
end;

function TranslateStreamLength(OriginalStream, MixerStream: HSTREAM): QWord;
begin
	Result := BASS_ChannelSeconds2Bytes(MixerStream, StreamLengthInSeconds(OriginalStream));
end;

procedure FreeStream(Stream: HSTREAM);
begin
	if Stream <> 0 then
		BASS_StreamFree(Stream);
end;

{ TAudioManager }

constructor TAudioManager.Create;
var
	i: Integer;
	V: DWord;
	Dev: TAudioDevice;
	Info: BASS_DEVICEINFO;
	//Fn: UnicodeString;
begin
	SetSpeakerAssignmentInfo(0,  BASS_SPEAKER_FRONT,		'Stereo: Front');
	SetSpeakerAssignmentInfo(1,  BASS_SPEAKER_REAR,			'Stereo: Rear');
	SetSpeakerAssignmentInfo(2,  BASS_SPEAKER_CENLFE,		'Stereo: Center/LFE');
	SetSpeakerAssignmentInfo(3,  BASS_SPEAKER_REAR2,		'Stereo: Rear Center');
	SetSpeakerAssignmentInfo(4,  BASS_SPEAKER_FRONTLEFT,	'Mono: Left');
	SetSpeakerAssignmentInfo(5,  BASS_SPEAKER_FRONTRIGHT,	'Mono: Right');
	SetSpeakerAssignmentInfo(6,  BASS_SPEAKER_REARLEFT,		'Mono: Rear Left');
	SetSpeakerAssignmentInfo(7,  BASS_SPEAKER_REARRIGHT,	'Mono: Rear Right');
	SetSpeakerAssignmentInfo(8,  BASS_SPEAKER_CENTER,		'Mono: Center');
	SetSpeakerAssignmentInfo(9,  BASS_SPEAKER_LFE,			'Mono: LFE');
	SetSpeakerAssignmentInfo(10, BASS_SPEAKER_REAR2LEFT,	'Mono: Side Left');
	SetSpeakerAssignmentInfo(11, BASS_SPEAKER_REAR2RIGHT,	'Mono: Side Right');

	Devices := TFPGObjectList<TAudioDevice>.Create(True);

	i := 1; // first real audio output device (0 = no sound)
	DefaultDeviceIndex := i;

	while BASS_GetDeviceInfo(i, Info{%H-}) do
	begin
		if (Info.flags and BASS_DEVICE_ENABLED) <> 0 then
		begin
			Dev := TAudioDevice.Create;
			Dev.Index := i;
			Dev.Filename := Info.driver;
			Dev.Speakers := 0;
			Dev.Name := Info.name;
			Dev.IsDefault := (Info.flags and BASS_DEVICE_DEFAULT) <> 0;
			if Dev.IsDefault then
				DefaultDeviceIndex := i;
			Devices.Add(Dev);
		end;
		Inc(i);
	end;

//	Fn := UnicodeString(Config.PluginPath + basszxtunedll);
//	if BASS_PluginLoad(PChar(Fn), BASS_UNICODE) = 0 then
//		showmessage('Plugin load failed: ' + Fn);

	V := BASS_GetVersion;
	BASSVersion := Format('%d.%d.%d.%d',
		[ V shr 24 and 255, V shr 16 and 255, V shr 8 and 255, V and 255 ]);
end;

destructor TAudioManager.Destroy;
begin
	Devices.Free;
	BASS_Free;
	inherited Destroy;
end;

function TAudioManager.InitDevice(WinHandle: QWord; Device: Integer = -1): Boolean;
var
	Info: BASS_INFO;
	BufLen: Integer;
begin
	Result := False;

	if (Device < 0) or (Device >= Devices.Count) then
		Device := DefaultDeviceIndex - 1;

	//showmessage('init dev #' + Devices[Device].Index.ToString + '='+Devices[Device].Name );
	if not BASS_Init(Devices[Device].Index, Config.Audio.Hz,
		BASS_DEVICE_LATENCY or BASS_DEVICE_SPEAKERS,
		{$IFNDEF MSWINDOWS}@{$ENDIF}WinHandle, nil) then Exit;

	if not BASS_GetInfo(Info{%H-}) then Exit;

	with Devices[Device].DeviceInfo do
	begin
		Initialized := True;
		MinRate := Info.minrate;
		MaxRate := Info.maxrate;
		MinBuffer := Info.minbuf;
		Latency := Info.latency;
		Outputs := Info.speakers;
		Frequency := Info.freq;
	end;

	BASS_SetConfig(BASS_CONFIG_DEV_NONSTOP, 1);
	BASS_SetConfig(BASS_CONFIG_SRC, 3); // 32 point sinc interpolation
	BASS_SetConfig(BASS_CONFIG_UPDATETHREADS, 2);
	BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, Config.Audio.UpdatePeriod);

	if Config.Audio.Buffer > 0 then
		BufLen := Config.Audio.Buffer
	else
		BufLen := Info.minbuf + Config.Audio.UpdatePeriod + 20;

	BASS_SetConfig(BASS_CONFIG_BUFFER, BufLen);

	Result := True;
//showmessage('Outputs=' + deviceinfo.Outputs.ToString);
//showmessage('lat=' + deviceinfo.latency.ToString);
end;

function TAudioManager.InitPlugins(const Path: String): Integer;
const
	{$IFDEF MSWINDOWS}
	Ext = 'dll';
	{$ELSE}
	Ext = 'so';
	{$ENDIF}
var
	FileList: TStringList;
	i: Integer;
	S, FS: String;
	Plugin: HPLUGIN;
	Info: PBASS_PLUGININFO;
begin
	FileList := TStringList.Create;
	try
		try
			FindAllFiles(FileList, Path, '*.' + Ext, False);
		except
			Exit(-1);
		end;

		Result := FileList.Count;
		if Result < 1 then Exit;

		Result := 0;
		for S in FileList do
		begin
			Plugin := BASS_PluginLoad(PChar(S), 0);
			if Plugin = 0 then Continue;

			Info := BASS_PluginGetInfo(Plugin);
			for i := 0 to Info.formatc-1 do
			begin
				FS := StrPas(Info.Formats[i].exts);
				if S.IsEmpty then Continue;

				FS := FS.Replace('*', '', [rfReplaceAll]);
				FS := FS.Replace(';', ' ', [rfReplaceAll]);
				SupportedFormats += FS;
				Inc(Result);
			end;

		end;
	finally
		FileList.Free;
	end;
end;

end.

