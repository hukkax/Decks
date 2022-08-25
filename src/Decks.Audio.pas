unit Decks.Audio;

{$mode Delphi}

interface

uses
	Classes, SysUtils, FGL, //Generics.Collections,
	BASS, BASSMIX;

const
	OUTPUTS: array[0..3] of Integer = (
		BASS_SPEAKER_FRONT, BASS_SPEAKER_REAR, BASS_SPEAKER_CENLFE, BASS_SPEAKER_REAR2);

type
	TAudioDevice = class
		Index:		Word;	// Device index
		Name,				// Description of the device
		Filename:	String;	// The filename of the driver
		IsDefault:	Boolean;
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

	TAudioManager = class
	public
		Devices:	TFPGObjectList<TAudioDevice>;
		DeviceInfo: TAudioDeviceInfo;
		DefaultDeviceIndex: Integer;
		BASSVersion: String;

		function	InitDevice(WinHandle: QWord; Device: Integer = -1): Boolean;

		constructor	Create;
		destructor	Destroy; override;
	end;


	function  StreamLengthInSeconds(Stream: HSTREAM): Double;
	function  TranslateStreamLength(OriginalStream, MixerStream: HSTREAM): QWord;
	procedure FreeStream(Stream: HSTREAM);


implementation

uses
	Dialogs,
	Decks.Config;
	//basszxtune;


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
	Fn: UnicodeString;
begin
	Devices := TFPGObjectList<TAudioDevice>.Create(True);

	i := 1; // first real audio output device (0 = no sound)
	DefaultDeviceIndex := i;

	while BASS_GetDeviceInfo(i, Info) do
	begin
		if (Info.flags and BASS_DEVICE_ENABLED) <> 0 then
		begin
			Dev := TAudioDevice.Create;
			Dev.Index := i;
			Dev.Filename := Info.driver;
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
	if not BASS_Init(Devices[Device].Index, 44100,
		BASS_DEVICE_LATENCY or BASS_DEVICE_SPEAKERS,
		{$IFNDEF MSWINDOWS}@{$ENDIF}WinHandle, nil) then Exit;

	if not BASS_GetInfo(Info) then Exit;

	with DeviceInfo do
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

end.

