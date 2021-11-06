{
  BASSWASAPI 2.4 Delphi unit
  Copyright (c) 2009-2020 Un4seen Developments Ltd.

  See the BASSWASAPI.CHM file for more detailed documentation
}

unit BassWASAPI;

interface

uses Windows, Bass;

const
  // Additional error codes returned by BASS_ErrorGetCode
  BASS_ERROR_WASAPI           = 5000; // no WASAPI
  BASS_ERROR_WASAPI_BUFFER    = 5001; // buffer size is invalid
  BASS_ERROR_WASAPI_CATEGORY  = 5002; // can't set category
  BASS_ERROR_WASAPI_DENIED    = 5003; // access denied

  // BASS_WASAPI_DEVICEINFO "type"
  BASS_WASAPI_TYPE_NETWORKDEVICE   = 0;
  BASS_WASAPI_TYPE_SPEAKERS        = 1;
  BASS_WASAPI_TYPE_LINELEVEL       = 2;
  BASS_WASAPI_TYPE_HEADPHONES      = 3;
  BASS_WASAPI_TYPE_MICROPHONE      = 4;
  BASS_WASAPI_TYPE_HEADSET         = 5;
  BASS_WASAPI_TYPE_HANDSET         = 6;
  BASS_WASAPI_TYPE_DIGITAL         = 7;
  BASS_WASAPI_TYPE_SPDIF           = 8;
  BASS_WASAPI_TYPE_HDMI            = 9;
  BASS_WASAPI_TYPE_UNKNOWN         = 10;

  // BASS_WASAPI_DEVICEINFO flags
  BASS_DEVICE_ENABLED              = 1;
  BASS_DEVICE_DEFAULT              = 2;
  BASS_DEVICE_INIT                 = 4;
  BASS_DEVICE_LOOPBACK             = 8;
  BASS_DEVICE_INPUT                = 16;
  BASS_DEVICE_UNPLUGGED            = 32;
  BASS_DEVICE_DISABLED             = 64;

  // BASS_WASAPI_Init flags
  BASS_WASAPI_EXCLUSIVE            = 1;
  BASS_WASAPI_AUTOFORMAT           = 2;
  BASS_WASAPI_BUFFER               = 4;
  BASS_WASAPI_EVENT                = 16;
  BASS_WASAPI_SAMPLES              = 32;
  BASS_WASAPI_DITHER               = 64;
  BASS_WASAPI_RAW                  = 128;
  BASS_WASAPI_ASYNC                = $100;

  BASS_WASAPI_CATEGORY_MASK = $f000;
  BASS_WASAPI_CATEGORY_OTHER = $0000;
  BASS_WASAPI_CATEGORY_FOREGROUNDONLYMEDIA = $1000;
  BASS_WASAPI_CATEGORY_BACKGROUNDCAPABLEMEDIA = $2000;
  BASS_WASAPI_CATEGORY_COMMUNICATIONS = $3000;
  BASS_WASAPI_CATEGORY_ALERTS = $4000;
  BASS_WASAPI_CATEGORY_SOUNDEFFECTS = $5000;
  BASS_WASAPI_CATEGORY_GAMEEFFECTS = $6000;
  BASS_WASAPI_CATEGORY_GAMEMEDIA = $7000;
  BASS_WASAPI_CATEGORY_GAMECHAT = $8000;
  BASS_WASAPI_CATEGORY_SPEECH = $9000;
  BASS_WASAPI_CATEGORY_MOVIE = $a000;
  BASS_WASAPI_CATEGORY_MEDIA = $b000;

  // BASS_WASAPI_INFO "format"
  BASS_WASAPI_FORMAT_FLOAT         = 0;
  BASS_WASAPI_FORMAT_8BIT          = 1;
  BASS_WASAPI_FORMAT_16BIT         = 2;
  BASS_WASAPI_FORMAT_24BIT         = 3;
  BASS_WASAPI_FORMAT_32BIT         = 4;

  // BASS_WASAPI_Set/GetVolume modes
  BASS_WASAPI_CURVE_DB             = 0;
  BASS_WASAPI_CURVE_LINEAR         = 1;
  BASS_WASAPI_CURVE_WINDOWS        = 2;
  BASS_WASAPI_VOL_SESSION          = 8;

  // Special WASAPIPROCs
  WASAPIPROC_PUSH                  = Pointer(0);		// push output
  WASAPIPROC_BASS                  = Pointer(-1);		// BASS channel

  // Device notifications
  BASS_WASAPI_NOTIFY_ENABLED       = 0;
  BASS_WASAPI_NOTIFY_DISABLED      = 1;
  BASS_WASAPI_NOTIFY_DEFOUTPUT     = 2;
  BASS_WASAPI_NOTIFY_DEFINPUT      = 3;
  BASS_WASAPI_NOTIFY_FAIL          = $100;

type
  // Device info structure
  BASS_WASAPI_DEVICEINFO = record
    name: PAnsiChar;
    id: PAnsiChar;
    type_: DWORD; 
    flags: DWORD;
    minperiod: Single;
    defperiod: Single;
    mixfreq: DWORD;
    mixchans: DWORD;
  end;

  BASS_WASAPI_INFO = record
	initflags: DWORD;
	freq: DWORD;
	chans: DWORD;
	format: DWORD;
	buflen: DWORD;
	volmax: Single;
	volmin: Single;
	volstep: Single;
  end;

  WASAPIPROC = function(buffer:Pointer; length:DWORD; user:Pointer): DWORD; stdcall;
  {
    WASAPI callback function.
    buffer : Buffer containing the sample data
    length : Number of bytes
    user   : The 'user' parameter given when calling BASS_WASAPI_Init
    RETURN : The number of bytes written (output devices), 0/1 = stop/continue (input devices)
  }

  WASAPINOTIFYPROC = procedure(notify,device:DWORD; user:Pointer); stdcall;
  {
    WASAPI device notification callback function.
    notify : The notification (BASS_WASAPI_NOTIFY_xxx)
    device : Device that the notification applies to
    user   : The 'user' parameter given when calling BASS_WASAPI_SetNotify
  }


const
  basswasapidll = 'basswasapi.dll';

function BASS_WASAPI_GetVersion: DWORD; stdcall; external basswasapidll;
function BASS_WASAPI_SetNotify(proc:WASAPINOTIFYPROC; user:Pointer): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_GetDeviceInfo(device:DWORD; var info:BASS_WASAPI_DEVICEINFO): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_GetDeviceLevel(device:DWORD; chan:Integer): Single; stdcall; external basswasapidll;
function BASS_WASAPI_SetDevice(device:DWORD): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_GetDevice(): DWORD; stdcall; external basswasapidll;
function BASS_WASAPI_CheckFormat(device:Integer; freq,chans,flags:DWORD): DWORD; stdcall; external basswasapidll;
function BASS_WASAPI_Init(device:Integer; freq,chans,flags:DWORD; buffer,period:Single; proc:WASAPIPROC; user:Pointer): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_Free(): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_GetInfo(var info:BASS_WASAPI_INFO): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_GetCPU(): Single; stdcall; external basswasapidll;
function BASS_WASAPI_Lock(lock:BOOL): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_Start(): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_Stop(reset:BOOL): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_IsStarted(): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_SetVolume(mode:DWORD; volume:Single): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_GetVolume(mode:DWORD): Single; stdcall; external basswasapidll;
function BASS_WASAPI_SetMute(mode:DWORD; mute:BOOL): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_GetMute(mode:DWORD): BOOL; stdcall; external basswasapidll;
function BASS_WASAPI_PutData(buffer: Pointer; length: DWORD): DWORD; stdcall; external basswasapidll;
function BASS_WASAPI_GetData(buffer: Pointer; length: DWORD): DWORD; stdcall; external basswasapidll;
function BASS_WASAPI_GetLevel(): DWORD; stdcall; external basswasapidll;
function BASS_WASAPI_GetLevelEx(levels: PSingle; length: Single; flags: DWORD): BOOL; stdcall; external basswasapidll;

implementation

end.
