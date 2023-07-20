unit RtMidi;

interface

uses
	{$IFDEF MSWINDOWS}Windows,{$ENDIF}
	Classes, SysUtils;

{$MACRO ON}
{$DEFINE DLLCall:=cdecl; external rtmididll}

const
	rtmididll =
	{$IFDEF MSWINDOWS}'librtmidi-5.dll';{$ENDIF}
	{$IFDEF LINUX}    'librtmidi.so';   {$ENDIF}
	{$IFDEF MACOS}    'librtmidi.dylib';{$ENDIF}

type
	UInt    = Cardinal;
	size_t  = QWord;
	CString = PAnsiChar;

	// Wraps an RtMidi object for C function return statuses.
	TRtMidiWrapper = record
		// The wrapped RtMidi object.
		ptr, data: Pointer; // procedure*
		// True when the last function call was OK. 
		ok: Boolean;
		// If an error occured (ok != true), set to an error message.
		msg: CString;
	end;
	PRtMidiWrapper = ^TRtMidiWrapper;

	TRtMidiPtr    = PRtMidiWrapper;
	TRtMidiInPtr  = PRtMidiWrapper;
	TRtMidiOutPtr = PRtMidiWrapper;

	// MIDI API specifier arguments.  See RtMidi::Api.
	TRtMidiApi = (
		RTMIDI_API_UNSPECIFIED,    // Search for a working compiled API.
		RTMIDI_API_MACOSX_CORE,    // Macintosh OS-X CoreMIDI API.
		RTMIDI_API_LINUX_ALSA,     // The Advanced Linux Sound Architecture API.
		RTMIDI_API_UNIX_JACK,      // The Jack Low-Latency MIDI Server API.
		RTMIDI_API_WINDOWS_MM,     // The Microsoft Multimedia MIDI API.
		RTMIDI_API_RTMIDI_DUMMY,   // A compilable but non-functional API.
		RTMIDI_API_NUM             // Number of values in this enum.
	);

	TRtMidiApiSet = set of TRtMidiApi;

	// Defined RtMidiError types. See RtMidiError::Type.
	TRtMidiErrorType = (
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

	// The type of a RtMidi callback function.
	// * timeStamp:  The time at which the message has been received.
	// * message:    The midi message.
	// * userData:   Additional user data for the callback.
	// See RtMidiIn::RtMidiCallback.
	//
	TRtMidiCallback = procedure(Timestamp: Double; Data: PByte; Size: size_t; UserData: Pointer); cdecl;

	TRtMidiErrorCallback = procedure(ErrorKind: TRtMidiErrorType; ErrorMessage: String; UserData: Pointer);

	TRtMidi = class
	private
		PortOpened: Boolean;
	public
		API: TRtMidiApi;
		Device: TRtMidiPtr;

		procedure OpenPort(PortNumber: UInt=0; PortName: String = 'RtMidi'); virtual;
		procedure OpenVirtualPort(PortName: String = 'RtMidi'); virtual;
		function  GetPortCount: UInt; virtual;
		function  GetPortName(PortNumber: UInt = 0): String; virtual;
		procedure ClosePort; virtual;
		function  IsPortOpen: Boolean; virtual;
		function  GetCurrentAPI: TRtMidiApi;
		procedure SetErrorCallback(ErrorCallback: TRtMidiErrorCallback = nil; UserData: Pointer = nil); virtual;

		class function GetVersion: String;
		class function GetCompiledAPI: TRtMidiApiSet;
		class function GetAPIName(api: TRtMidiApi): String;
		class function GetAPIDisplayName(api: TRtMidiApi): String;
		class function GetCompiledAPIByName(const Name: AnsiString): TRtMidiApi;
	end;

	TRtMidiIn = class(TRtMidi)
	private
	public
		Callback: TRtMidiCallback;

		constructor Create(TheAPI: TRtMidiApi = RTMIDI_API_UNSPECIFIED;
			ClientName: String = 'RtMidi Input Client';
			QueueSizeLimit: UInt=100);
		destructor  Destroy; override;

		procedure SetCallback(ACallback: TRtMidiCallback; UserData: Pointer);
		procedure CancelCallback;
		procedure IgnoreTypes(MidiSysex: Boolean = True; MidiTime: Boolean = True; MidiSense: Boolean = True);
		function  GetMessage(var Sl: TStrings): Double;
		function  GetDeviceList: TStringList;
	end;

	TRtMidiOut = class(TRtMidi)
	private
	public
		constructor Create(TheAPI: TRtMidiApi = RTMIDI_API_UNSPECIFIED;
			ClientName: String = 'RtMidi Output Client';
			QueueSizeLimit: UInt=100);
		destructor  Destroy; override;

		procedure SendMessage(const Message: array of Byte);
		function  GetDeviceList: TStringList;
	end;


	function GetMidiDevices(Device: TRtMidiPtr = nil; WantOutput: Boolean = True): TStringList;
	function GetMidiInDevices(Device: TRtMidiPtr = nil): TStringList;
	function GetMidiOutDevices(Device: TRtMidiPtr = nil): TStringList;
	function GetMidiPortName(Device: TRtMidiPtr; PortNumber: UInt): String;


//==========================================================================
// RtMidi API
//==========================================================================

// Determine the available compiled MIDI APIs.
// If the given 'apis' parameter is null, returns the number of available APIs.
// Otherwise, fill the given apis array with the RtMidi::Api values.
// * apis:      An array or a null value.
// * apis_size: Number of elements pointed to by apis
// Returns number of items needed for apis array if apis==NULL, or number of items
// written to apis array otherwise. A negative return value indicates an error.
// See RtMidi::getCompiledApi().
//
function rtmidi_get_compiled_api(apis: TRtMidiApi; apis_size: UInt): Integer; DLLCall;

// Return the name of a specified compiled MIDI API.
// See RtMidi::getApiName().
//
function rtmidi_api_name(api: TRtMidiApi): CString; DLLCall;

// Return the display name of a specified compiled MIDI API.
// See RtMidi::getApiDisplayName().
//
function rtmidi_api_display_name(api: TRtMidiApi): CString; DLLCall;

// Return the compiled MIDI API having the given name.
// See RtMidi::getCompiledApiByName().
//
function rtmidi_compiled_api_by_name(name: CString): TRtMidiApi; DLLCall;

// Report an error.
//
procedure rtmidi_error(errortype: TRtMidiErrorType; errorString: CString); DLLCall;

// Open a MIDI port.
// * port:      Must be greater than 0
// * portName:  Name for the application port.
// See RtMidi::openPort().
//
procedure rtmidi_open_port(device: TRtMidiPtr; portNumber: UInt; portName: CString); DLLCall;

// Creates a virtual MIDI port to which other software applications can connect.  
// * portName: Name for the application port.
// See RtMidi::openVirtualPort().
//
procedure rtmidi_open_virtual_port(device: TRtMidiPtr; portName: CString); DLLCall;

// Close a MIDI connection.
// See RtMidi::closePort().
//
procedure rtmidi_close_port(device: TRtMidiPtr); DLLCall;

// Return the number of available MIDI ports.
// See RtMidi::getPortCount().
//
function rtmidi_get_port_count(device: TRtMidiPtr): UInt; DLLCall;

// Return a string identifier for the specified MIDI input port number.
// See RtMidi::getPortName().
{$IFDEF RTMIDI_NEED_STRING_BUFFER}
// this call now needs a string buffer:
// RTMIDIAPI int rtmidi_get_port_name (RtMidiPtr device, unsigned int portNumber, char * bufOut, int * bufLen);
function rtmidi_get_port_name(device: TRtMidiPtr; portNumber: UInt; bufout: CString; buflen: PInteger): Integer; DLLCall;
{$ELSE}
function rtmidi_get_port_name(device: TRtMidiPtr; portNumber: UInt): CString; DLLCall;
{$ENDIF}

//==========================================================================
// RtMidiIn API
//==========================================================================

// Create a default RtMidiInPtr value, with no initialization.
//
function rtmidi_in_create_default: TRtMidiInPtr; DLLCall;

// Create a  RtMidiInPtr value, with given api, clientName and queueSizeLimit.
// * api:            An optional API id can be specified.
// * clientName:     An optional client name can be specified. This will be
//                   used to group the ports that are created by the application.
// * queueSizeLimit: An optional size of the MIDI input queue can be specified.
// See RtMidiIn::RtMidiIn().
//
function rtmidi_in_create(api: TRtMidiApi; clientName: CString; queueSizeLimit: UInt): TRtMidiInPtr; DLLCall;

// Free the given RtMidiInPtr.
//
procedure rtmidi_in_free(device: TRtMidiInPtr); DLLCall;

// Returns the MIDI API specifier for the given instance of RtMidiIn.
// See RtMidiIn::getCurrentApi().
//
function rtmidi_in_get_current_api(device: TRtMidiPtr): TRtMidiApi; DLLCall;

// Set a callback function to be invoked for incoming MIDI messages.
// See RtMidiIn::setCallback().
//
procedure rtmidi_in_set_callback(device: TRtMidiInPtr; callback: TRtMidiCallback; userData: Pointer); DLLCall;

// Cancel use of the current callback function (if one exists).
// See RtMidiIn::cancelCallback().
//
procedure rtmidi_in_cancel_callback(device: TRtMidiInPtr); DLLCall;

// Specify whether certain MIDI message types should be queued or ignored during input.
// See RtMidiIn::ignoreTypes().
//
procedure rtmidi_in_ignore_types(device: TRtMidiInPtr; midiSysex, midiTime, midiSense: Boolean); DLLCall;

// Fill the user-provided array with the data bytes for the next available
// MIDI message in the input queue and return the event delta-time in seconds.
// * message: Must point to a char* that is already allocated.
//            SYSEX messages maximum size being 1024, a statically
//            allocated array could be sufficient.
// * size:    Is used to return the size of the message obtained.
// See RtMidiIn::getMessage().
//
function rtmidi_in_get_message(device: TRtMidiInPtr; msg: CString; var size: size_t): Double; DLLCall;

//==========================================================================
// RtMidiOut API
//==========================================================================

// Create a default RtMidiOutPtr value, with no initialization.
//
function rtmidi_out_create_default: TRtMidiOutPtr; DLLCall;

// Create a RtMidiOutPtr value, with given and clientName.
// * api:        An optional API id can be specified.
// * clientName: An optional client name can be specified. This will be used
//               to group the ports that are created by the application.
// See RtMidiOut::RtMidiOut().
//
function rtmidi_out_create(api: TRtMidiApi; clientName: CString): TRtMidiOutPtr; DLLCall;

// Free the given RtMidiOutPtr.
//
procedure rtmidi_out_free(device: TRtMidiOutPtr); DLLCall;

// Returns the MIDI API specifier for the given instance of RtMidiOut.
// See RtMidiOut::getCurrentApi().
//
function rtmidi_out_get_current_api(device: TRtMidiPtr): TRtMidiApi; DLLCall;

// Immediately send a single message out an open MIDI output port.
// See RtMidiOut::sendMessage().
//
function rtmidi_out_send_message(device: TRtMidiOutPtr; msg: CString; length: Integer): Integer; DLLCall;


implementation


function GetMidiDevices(Device: TRtMidiPtr = nil; WantOutput: Boolean = True): TStringList;
var
	i, x, nPorts: Integer;
	Created: Boolean = False;
	S: String;
begin
	Result := TStringList.Create;

	if Device = nil then
	begin
		Created := True;
		if WantOutput then
			Device := rtmidi_out_create_default
		else
			Device := rtmidi_in_create_default;
	end;

	if (Device = nil) or (not Device.ok) then Exit;
	nPorts := rtmidi_get_port_count(Device);

	for i := 0 to nPorts-1 do
	begin
		try
			S := GetMidiPortName(Device, i);
			if S.IsEmpty then Continue;

			x := LastDelimiter(' ', S);
			S := Copy(S, 1, x-1);

			x := Pos(':', S);
			if x > 1 then
				S := Copy(S, 1, x-1);

			if not S.IsEmpty then
				Result.Add(S);
		except
		end;
	end;

	if Created then
	begin
		if WantOutput then
			rtmidi_out_free(Device)
		else
			rtmidi_in_free(Device);
	end;
end;

function GetMidiInDevices(Device: TRtMidiPtr = nil): TStringList;
begin
	Result := GetMidiDevices(Device, False);
end;

function GetMidiOutDevices(Device: TRtMidiPtr = nil): TStringList;
begin
	Result := GetMidiDevices(Device, True);
end;

function GetMidiPortName(Device: TRtMidiPtr; PortNumber: UInt): String;
var
	CS: AnsiString;
	{$IFDEF RTMIDI_NEED_STRING_BUFFER}
	L: Integer;
	{$ENDIF}
begin
	{$IFDEF RTMIDI_NEED_STRING_BUFFER}
	CS := '';
	rtmidi_get_port_name(Device, PortNumber, nil, @L); // get string length
	SetLength(CS, L+1);
	rtmidi_get_port_name(Device, PortNumber, @CS[1], @L); // get the string
	{$ELSE}
	CS := rtmidi_get_port_name(Device, PortNumber);
	{$ENDIF}
	Result := CS;
end;

//==========================================================================
// TRtMidi API
//==========================================================================

function TRtMidi.GetCurrentAPI: TRtMidiApi;
begin
	Result := API;
end;

class function TRtMidi.GetVersion: String;
begin
	Result := '';
end;

class function TRtMidi.GetCompiledAPI: TRtMidiApiSet;
begin
	Result := [];
end;

class function TRtMidi.GetAPIName(api: TRtMidiApi): String;
begin
	Result := rtmidi_api_name(API);
end;

class function TRtMidi.GetAPIDisplayName(api: TRtMidiApi): String;
begin
	Result := rtmidi_api_display_name(API);
end;

class function TRtMidi.GetCompiledAPIByName(const Name: AnsiString): TRtMidiApi;
begin
	Result := rtmidi_compiled_api_by_name(PAnsiChar(Name));
end;

function TRtMidi.GetPortCount: UInt;
begin
	Result := rtmidi_get_port_count(Device);
end;

function TRtMidi.GetPortName(PortNumber: UInt): String;
begin
	Result := GetMidiPortName(Device, PortNumber);
end;

procedure TRtMidi.ClosePort;
begin
	if PortOpened then
		rtmidi_close_port(Device);
	PortOpened := False;
end;

function TRtMidi.IsPortOpen: Boolean;
begin
	Result := PortOpened;
end;

procedure TRtMidi.OpenPort(PortNumber: UInt; PortName: String);
begin
	rtmidi_open_port(Device, PortNumber, CString(PortName));
	PortOpened := True;
end;

procedure TRtMidi.OpenVirtualPort(PortName: String);
begin
	rtmidi_open_virtual_port(Device, CString(PortName));
	PortOpened := True;
end;

procedure TRtMidi.SetErrorCallback(ErrorCallback: TRtMidiErrorCallback; UserData: Pointer);
begin
//
end;

//==========================================================================
// RtMidiIn API
//==========================================================================

constructor TRtMidiIn.Create(TheAPI: TRtMidiApi; ClientName: String; QueueSizeLimit: UInt);
begin
	inherited Create;
	Device := rtmidi_in_create(TheAPI, CString(ClientName), QueueSizeLimit);
end;

destructor TRtMidiIn.Destroy;
begin
	rtmidi_in_free(Device);
	inherited Destroy;
end;

procedure TRtMidiIn.SetCallback(ACallback: TRtMidiCallback; UserData: Pointer);
begin
	Callback := ACallback;
	rtmidi_in_set_callback(Device, ACallback, UserData);
end;

procedure TRtMidiIn.CancelCallback;
begin
	rtmidi_in_cancel_callback(Device);
end;

procedure TRtMidiIn.IgnoreTypes(MidiSysex: Boolean; MidiTime: Boolean; MidiSense: Boolean);
begin
	rtmidi_in_ignore_types(Device, MidiSysex, MidiTime, MidiSense);
end;

function TRtMidiIn.GetDeviceList: TStringList;
begin
	Result := GetMidiInDevices(Device);
end;

function TRtMidiIn.GetMessage(var Sl: TStrings): Double;
var
	S: String;
	sz: size_t;
begin
	Result := 0;
	for S in Sl do
		Result += rtmidi_in_get_message(Device, CString(S), sz);
end;

//==========================================================================
// RtMidiOut API
//==========================================================================

constructor TRtMidiOut.Create(TheAPI: TRtMidiApi; ClientName: String; QueueSizeLimit: UInt);
begin
	inherited Create;
	Device := rtmidi_out_create(TheAPI, PAnsiChar(ClientName));
end;

destructor TRtMidiOut.Destroy;
begin
	rtmidi_out_free(Device);
	inherited Destroy;
end;

function TRtMidiOut.GetDeviceList: TStringList;
begin
	Result := GetMidiOutDevices(Device);
end;

procedure TRtMidiOut.SendMessage(const Message: array of Byte);
begin
	rtmidi_out_send_message(Device, @Message[0], Length(Message));
end;

end.
