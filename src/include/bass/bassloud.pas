{
  BASSloud 2.4 Delphi unit
  Copyright (c) 2023 Un4seen Developments Ltd.

  See the BASSLOUD.CHM file for more detailed documentation
}

Unit BASSloud;

interface

{$IFDEF MSWINDOWS}
uses BASS, Windows;
{$ELSE}
uses BASS;
{$ENDIF}

const
  // BASS_Loudness_Start flags / BASS_Loudness_GetLevel modes
  BASS_LOUDNESS_CURRENT         = 0;
  BASS_LOUDNESS_INTEGRATED      = 1;
  BASS_LOUDNESS_RANGE           = 2;
  BASS_LOUDNESS_PEAK            = 4;
  BASS_LOUDNESS_TRUEPEAK        = 8;
  BASS_LOUDNESS_AUTOFREE        = $8000;

type
  HLOUDNESS = DWORD;   // loudness handle
  PHLOUDNESS = ^HLOUDNESS;

const
{$IFDEF MSWINDOWS}
  basslouddll = 'bassloud.dll';
{$ENDIF}
{$IFDEF LINUX}
  basslouddll = 'libbassloud.so';
{$ENDIF}
{$IFDEF ANDROID}
  basslouddll = 'libbassloud.so';
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
    basslouddll = 'libbassloud.a';
  {$ELSE}
    basslouddll = 'libbassloud.dylib';
  {$ENDIF}
{$ENDIF}

function BASS_Loudness_GetVersion: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basslouddll;

function BASS_Loudness_Start(handle, flags: DWORD; priority: Integer): HLOUDNESS; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basslouddll;
function BASS_Loudness_Stop(handle: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basslouddll;
function BASS_Loudness_SetChannel(handle: HLOUDNESS; channel: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basslouddll;
function BASS_Loudness_GetChannel(handle: HLOUDNESS): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basslouddll;
function BASS_Loudness_GetLevel(handle: HLOUDNESS; mode: DWORD; var level: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basslouddll;
function BASS_Loudness_GetLevelMulti(handles: PHLOUDNESS; count, mode: DWORD; var level: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basslouddll;

implementation

end.
