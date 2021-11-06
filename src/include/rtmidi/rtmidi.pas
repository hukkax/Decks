unit RtMidi;

interface

{$IFDEF MSWINDOWS}
uses Windows;
{$ELSE}
{$ENDIF}


const
	rtmididll =
	{$IFDEF MSWINDOWS}'rtmidi.dll';     {$ENDIF}
	{$IFDEF LINUX}    'librtmidi.so';   {$ENDIF}
	{$IFDEF MACOS}    'librtmidi.dylib';{$ENDIF}

type
	// Wraps an RtMidi object for C function return statuses.
	TRtMidiWrapper = record
		// The wrapped RtMidi object.
		ptr, data: Pointer; // void*
		// True when the last function call was OK. 
		ok: Boolean;
		// If an error occured (ok != true), set to an error message.
		msg: PChar; // const char*
	end;
	PRtMidiWrapper = ^TRtMidiWrapper;

	RtMidiPtr,
	RtMidiInPtr,
	RtMidiOutPtr: PRtMidiWrapper;

	// MIDI API specifier arguments.  See RtMidi::Api.
	RtMidiApi = (
		RTMIDI_API_UNSPECIFIED,    // Search for a working compiled API.
		RTMIDI_API_MACOSX_CORE,    // Macintosh OS-X CoreMIDI API.
		RTMIDI_API_LINUX_ALSA,     // The Advanced Linux Sound Architecture API.
		RTMIDI_API_UNIX_JACK,      // The Jack Low-Latency MIDI Server API.
		RTMIDI_API_WINDOWS_MM,     // The Microsoft Multimedia MIDI API.
		RTMIDI_API_RTMIDI_DUMMY,   // A compilable but non-functional API.
		RTMIDI_API_NUM             // Number of values in this enum.
	);

	// Defined RtMidiError types. See RtMidiError::Type.
	RtMidiErrorType = (
		RTMIDI_ERROR_WARNING,           // A non-critical error.
		RTMIDI_ERROR_DEBUG_WARNING,     // A non-critical error which might be useful for debugging.
		RTMIDI_ERROR_UNSPECIFIED,       // The default, unspecified error type.
		RTMIDI_ERROR_NO_DEVICES_FOUND,  // No devices found on system.
		RTMIDI_ERROR_INVALID_DEVICE,    // An invalid device ID was specified.
		RTMIDI_ERROR_MEMORY_ERROR,      // An error occured during memory allocation.
		RTMIDI_ERROR_INVALID_PARAMETER, // An invalid parameter was specified to a function.
		RTMIDI_ERROR_INVALID_USE,       // The function was called incorrectly.
		RTMIDI_ERROR_DRIVER_ERROR,      // A system driver error occured.
		RTMIDI_ERROR_SYSTEM_ERROR,      // A system error occured.
		RTMIDI_ERROR_THREAD_ERROR       // A thread error occured.
	);

	size_t = QWord;

	// The type of a RtMidi callback function.
	// * timeStamp:  The time at which the message has been received.
	// * message:    The midi message.
	// * userData:   Additional user data for the callback.
	// See RtMidiIn::RtMidiCallback.
	//
	RtMidiCCallback = procedure(timeStamp: Double; msg: PAnsiChar; messageSize: size_t; userData: Pointer);
	{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

//==========================================================================
// RtMidi API
//==========================================================================

// Determine the available compiled MIDI APIs.
// If the given 'apis' parameter is null, returns the number of available APIs.
// Otherwise, fill the given apis array with the RtMidi::Api values.
// * apis:      An array or a null value.
// * apis_size: Number of elements pointed to by apis
// Returns number of items needed for apis array if apis==NULL, or
// number of items written to apis array otherwise.
// A negative return value indicates an error.
// See RtMidi::getCompiledApi().
//
function rtmidi_get_compiled_api(apis: RtMidiApi; apis_size: UInt): Integer;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Return the name of a specified compiled MIDI API.
// See RtMidi::getApiName().
//
function rtmidi_api_name(api: RtMidiApi): PAnsiChar;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Return the display name of a specified compiled MIDI API.
// See RtMidi::getApiDisplayName().
//
function rtmidi_api_display_name(api: RtMidiApi): PAnsiChar;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Return the compiled MIDI API having the given name.
// See RtMidi::getCompiledApiByName().
//
function rtmidi_compiled_api_by_name(name: PAnsiChar): RtMidiApi;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Report an error.
//
procedure rtmidi_error(errortype: RtMidiErrorType; errorString: PAnsiChar);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Open a MIDI port.
// * port:      Must be greater than 0
// * portName:  Name for the application port.
// See RtMidi::openPort().
//
procedure rtmidi_open_port(device: RtMidiPtr; portNumber: UInt; portName: PAnsiChar);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Creates a virtual MIDI port to which other software applications can connect.  
// portName: Name for the application port.
// See RtMidi::openVirtualPort().
//
procedure rtmidi_open_virtual_port(device: RtMidiPtr; portName: PAnsiChar);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Close a MIDI connection.
// See RtMidi::closePort().
//
procedure rtmidi_close_port(device: RtMidiPtr);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Return the number of available MIDI ports.
// See RtMidi::getPortCount().
//
function rtmidi_get_port_count(device: RtMidiPtr): UInt;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Return a string identifier for the specified MIDI input port number.
// See RtMidi::getPortName().

function rtmidi_get_port_name(device: RtMidiPtr; portNumber: UInt): PAnsiChar;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

//==========================================================================
// RtMidiIn API
//==========================================================================

// Create a default RtMidiInPtr value, with no initialization.
//
function rtmidi_in_create_default: RtMidiInPtr;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Create a  RtMidiInPtr value, with given api, clientName and queueSizeLimit.
// api            An optional API id can be specified.
// clientName     An optional client name can be specified. This
//                       will be used to group the ports that are created
//                       by the application.
// queueSizeLimit An optional size of the MIDI input queue can be
//                       specified.
// See RtMidiIn::RtMidiIn().
//
function rtmidi_in_create (api: RtMidiApi; clientName: PAnsiChar; queueSizeLimit: UInt): RtMidiInPtr;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Free the given RtMidiInPtr.
//
procedure rtmidi_in_free(device: RtMidiInPtr);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Returns the MIDI API specifier for the given instance of RtMidiIn.
// See RtMidiIn::getCurrentApi().
//
function rtmidi_in_get_current_api(device: RtMidiPtr): RtMidiApi;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Set a callback function to be invoked for incoming MIDI messages.
// See RtMidiIn::setCallback().
//
procedure rtmidi_in_set_callback(device: RtMidiInPtr; callback: RtMidiCCallback; userData: Pointer);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Cancel use of the current callback function (if one exists).
// See RtMidiIn::cancelCallback().
//
procedure rtmidi_in_cancel_callback(device: RtMidiInPtr);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Specify whether certain MIDI message types should be queued or ignored during input.
// See RtMidiIn::ignoreTypes().
//
procedure rtmidi_in_ignore_types(device: RtMidiInPtr; midiSysex, midiTime, midiSense: Boolean);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Fill the user-provided array with the data bytes for the next available
//MIDI message in the input queue and return the event delta-time in seconds.
// message:   Must point to a char* that is already allocated.
//                 SYSEX messages maximum size being 1024, a statically
//                 allocated array could be sufficient. 
// size:      Is used to return the size of the message obtained. 
// See RtMidiIn::getMessage().
//
function rtmidi_in_get_message(device: RtMidiInPtr; msg: PAnsiChar; size: size_t): Double;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

//==========================================================================
// RtMidiOut API
//==========================================================================

// Create a default RtMidiInPtr value, with no initialization.
//
function rtmidi_out_create_default: RtMidiOutPtr;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Create a RtMidiOutPtr value, with given and clientName.
// api            An optional API id can be specified.
// clientName     An optional client name can be specified. This
//                       will be used to group the ports that are created
//                       by the application.
// See RtMidiOut::RtMidiOut().
//
function rtmidi_out_create(api: RtMidiApi; clientName: PAnsiChar): RtMidiOutPtr;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Free the given RtMidiOutPtr.
//
procedure rtmidi_out_free(device: RtMidiOutPtr);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Returns the MIDI API specifier for the given instance of RtMidiOut.
// See RtMidiOut::getCurrentApi().
//
function rtmidi_out_get_current_api(device: RtMidiPtr): RtMidiApi;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;

// Immediately send a single message out an open MIDI output port.
// See RtMidiOut::sendMessage().
//
function rtmidi_out_send_message(device: RtMidiOutPtr; msg: PAnsiChar; length: Integer): Integer;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external rtmididll;


end.
