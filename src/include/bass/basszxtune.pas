{*************************************************************************}
{ basszxtune.pas - BASS sound library plugin                              }
{                                                                         }
{ (c) Alexey Parfenov, 2014                                               }
{                                                                         }
{ e-mail: zxed@alkatrazstudio.net                                         }
{                                                                         }
{ Header conversion by hukka 2020 (hukkax gmail)                          }
{                                                                         }
{ This program is free software; you can redistribute it and/or           }
{ modify it under the terms of the GNU General Public License             }
{ as published by the Free Software Foundation; either version 3 of       }
{ the License, or (at your option) any later version.                     }
{                                                                         }
{ This program is distributed in the hope that it will be useful,         }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of          }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        }
{ General Public License for more details.                                }
{                                                                         }
{ You may read GNU General Public License at:                             }
{   http://www.gnu.org/copyleft/gpl.html                                  }
{*************************************************************************}

unit basszxtune;

interface

uses BASS;

const
	{$IFDEF MSWINDOWS}
	basszxtunedll = 'basszxtune.dll';
	{$ENDIF}
	{$IFDEF LINUX}
	basszxtunedll = 'libbasszxtune.so';
	{$ENDIF}
	{$IFDEF ANDROID}
	basszxtunedll = 'libbasszxtune.so';
	{$ENDIF}
	{$IFDEF MACOS}
		{$IFDEF IOS}
		basszxtunedll = 'libbasszxtune.a';
		{$ELSE}
		basszxtunedll = 'libbasszxtune.dylib';
		{$ENDIF}
	{$ENDIF}

	BASS_CTYPE_MUSIC_ZXTUNE        = $CF1D0000;
	BASS_CONFIG_ZXTUNE_MAXFILESIZE = $CF1D0100;
	BASS_POS_ZXTUNE_SUB_COUNT      = $00F10000;
	BASS_POS_ZXTUNE_SUB_LENGTH     = $00F20000;
	BASS_TAG_ZXTUNE_SUB_OGG        = $00F10000;

function BASS_ZXTUNE_StreamCreateFile
	(mem:BOOL; f:Pointer; offset,length:QWORD; flags,freq:DWORD): HSTREAM;
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basszxtunedll;
function BASS_ZXTUNE_StreamCreateFileUser
	(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer; freq:DWORD): HSTREAM;
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external basszxtunedll;

implementation

end.
