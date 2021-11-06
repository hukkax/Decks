unit Decks.MIDI;

{$mode delphi}

interface

uses
	Classes, SysUtils, LMessages,
	RtMidi;

const
	MIDI_NOTE_ON  = $90;
	MIDI_NOTE_OFF = $80;

	MIDI_CTRLCODE_BUTTON_PRESS   = MIDI_NOTE_ON;
	MIDI_CTRLCODE_BUTTON_RELEASE = MIDI_NOTE_OFF;
	MIDI_CTRLCODE_ABSOLUTE = $B0;
	MIDI_CTRLCODE_RELATIVE = $B0;

	WM_MIDI = LM_USER + 2004;

(*
	-2 -1 00 +1 +2	modes (from BC MIDI documentation)
	--------------
	7E 7F 00 01 02	relative-1 twos complement
	3E 3F 40 41 42	relative-2 binary offset
	42 41 00 01 02	relative-3 sign and magnitude
					(note that bit 6 is 0 for positive values,
					 -1 for negative values, bits 1-5 = value)

	 -2   -1   00   +1   +2		14-bit showing MSB LSB
	7F7E 7F7F 0000 0001 0002	relative -1
	3F7E 3F7F 4000 4001 4002	relative-2 (offset from 4000)
	4002 4001 0000 0001 0002	relative-3 (sign is in bit 6 of MSB)
----
enc-r:	Relative Signed Bit.
		If the most significant bit is set, then the offset is positive.
		The lower 6 significant bits are the offset.
		The offset value is formed as 0svvvvvv, where s is the sign or direction.
enc-l:	Relative Signed Bit 2.
		This is the same as enc-r but with the direction of turn reversed.
enc-2:	Relative 2's Complement.
		Positive offsets are sent as normal from 1 to 64 and negative offsets are
		sent as 2's complement negative numbers. This is a signed 7 bit int.
enc-b:	Relative Binary Offset.
		Positive offsets are sent as offset plus 64 and negative offsets are
		sent as 64 minus offset. 64 is zero, 65 is +1, 63 is -1.
14-bit rpn-delta:
		The value is expected to be a signed 14bit value that is added to the current value.
14-bit nrpn-delta: -"-
----
My Axiom 25 has the following methods:

145 Program Change Increment/Decrement — —
146 2’s Complement from 64 / Relative (Binary Offset) 065 - 127 063 - 000
147 2’s Complement from 0 / Relative (2’s Complement) 001 - 64 127 - 065
148 Sign Magnitude / Relative (Signed Bit) 065 - 127 001 - 063
149 Sign Magnitude / Relative (Signed Bit 2) 001 - 063 065 - 127
150 Single Value Increment/Decrement 096 097
151 RPN Increment/Decrement Message 096 followed by 000 - 127 097 followed by 000 - 127
152 NRPN Increment/Decrement Message 096 followed by 000 - 127 097 followed by 000 - 127
*)
type
	TMidiControlKind = (
		MIDI_CTRL_BUTTON,
		MIDI_CTRL_ABSOLUTE,
		MIDI_CTRL_RELATIVE_TWOSCOMPLEMENT,
		MIDI_CTRL_RELATIVE_BINARYOFFSET,
		MIDI_CTRL_RELATIVE_SIGNMAGNITUDE
	);

	TDecksActionKind = (
		NO_ACTION,
		DECK_PLAY,			DECK_REVERSE,
		DECK_CUE,			DECK_SEEK,
		DECK_SYNC,			DECK_LOAD,
		DECK_AMP,			DECK_BEND,
		MIXER_EQ_LOW,		MIXER_EQ_MID,	MIXER_EQ_HIGH,
		MIXER_EQ_KILL,		MIXER_CROSSFADER,
		UI_LIST,			UI_SELECT,
		UI_SELECT_TOGGLE,	UI_SELECT_OPEN
	);

const
	// list of TDecksActionKinds that expect a parameter
	// to specify a deck number or other byte parameter
	ParameterizedActions: set of TDecksActionKind = [
		DECK_PLAY,			DECK_REVERSE,
		DECK_CUE,			DECK_SEEK,
		DECK_SYNC,			DECK_LOAD,
		DECK_AMP,			DECK_BEND,
		MIXER_EQ_LOW,		MIXER_EQ_MID,	MIXER_EQ_HIGH,	MIXER_EQ_KILL,
		UI_SELECT_TOGGLE,	UI_SELECT_OPEN //temporary
	];

	MidiCtrlCode: array[TMidiControlKind] of Byte = (
		MIDI_CTRLCODE_BUTTON_PRESS,
		MIDI_CTRLCODE_ABSOLUTE,
		MIDI_CTRLCODE_RELATIVE, // twoscomplement
		MIDI_CTRLCODE_RELATIVE,
		MIDI_CTRLCODE_RELATIVE
	);

type
	TDecksAction = record
		Kind:  TDecksActionKind;
		Param: Byte;
	end;

	// a button/knob/other control on a midi device
	TMIDIControl = record
		Enabled:   Boolean;
		HasLED:    Boolean;
		Kind:      TMidiControlKind;
		Parameter: Integer;
		Name:      String;
		Group:     String;
		Action:    TDecksAction;
	end;

	TMIDI = record
		Input:            TRtMidiIn;
		Output:           TRtMidiOut;
		Debug:            Boolean;
		DeviceName:       String;
		InDevice,
		OutDevice:        Integer;
		InputDeviceList,
		OutputDeviceList: TStringList;
		Controls:         array[Byte] of TMIDIControl;
		Leds:             array[TDecksActionKind, 1..2] of Integer;
		ActionNames:      array[TDecksActionKind] of String;

		procedure Init;
		procedure Uninit;
		procedure SetLed(Kind: TDecksActionKind; RightDeck: Boolean; LedState: Boolean = True);
	end;

	TExecuteParams = record
		Action:  TDecksAction;
		Kind:    TMidiControlKind;
		Value:   Integer;
		Pressed: Boolean;
	end;
	PExecuteParams = ^TExecuteParams;


implementation

uses
	IniFiles, Forms, Math, dialogs,
	Decks.Config,
	Form.Main;

procedure MIDIInCallback(Timestamp: Double; Data: PByte; Size: size_t; UserData: Pointer); cdecl;
var
	Ctrl: TMIDIControl;
	Params: PExecuteParams;
begin
	if Size < 3 then Exit;

	Ctrl := MIDI.Controls[Data[1]];
	if not Ctrl.Enabled then Exit;

	New(Params);
	Params^.Kind := Ctrl.Kind;

	case Ctrl.Kind of
		MIDI_CTRL_BUTTON:
		begin
			Params^.Action := Ctrl.Action;
			Params^.Value := Ctrl.Parameter;
			Params^.Pressed := (Data[0] = MIDI_CTRLCODE_BUTTON_PRESS);
		end;
		MIDI_CTRL_ABSOLUTE:
		begin
			Params^.Action := Ctrl.Action;
			Params^.Value := Data[2];
			Params^.Pressed := True;
		end;
		MIDI_CTRL_RELATIVE_TWOSCOMPLEMENT:
		begin
			if (Data[2] and 64) <> 0 then
				Params^.Value := (Data[2] and 63) - 64// negative
			else
				Params^.Value := (Data[2] and 63);     // positive
			Params^.Action  := Ctrl.Action;
			Params^.Pressed := True;
		end;
		MIDI_CTRL_RELATIVE_BINARYOFFSET:  ; // TODO
		MIDI_CTRL_RELATIVE_SIGNMAGNITUDE: ; // TODO
	end;

	Application.RemoveAsyncCalls(MainForm);
	Application.QueueAsyncCall(MainForm.ASyncExecute, {%H-}PtrInt(Params));
end;

procedure TMIDI.Init;

	procedure ReadControllerSection(Ini: TIniFile; const Sect: String; Kind: TMidiControlKind);
	var
		i: Integer;
		S: String;
		sl: TStringList;
		B: Boolean;
	begin
		sl := TStringList.Create;
		Ini.ReadSection(Sect, sl);
		for S in sl do
		begin
			i := Ini.ReadInteger(Sect, S, -1);
			B := (i < 0);
			if B then i := Abs(i);
			if i in [0..255] then
			begin
				Controls[i].Enabled := True;
				Controls[i].HasLED  := B;
				Controls[i].Name := UpperCase(S);
				Controls[i].Parameter := 0;
				Controls[i].Kind := Kind;
			end;
		end;
		sl.Free;
	end;

	function GetControl(const Name: String): Integer;
	var
		i: Integer;
		N: String;
	begin
		N := UpperCase(Name);
		for i := 0 to 255 do
			if Controls[i].Name = N then
				Exit(i);
		Result := -1;
	end;

	function GetControlParamValue(var S: String): Integer;
	var
		X: Integer;
	begin
		X := Pos(S[1], '-+');
		if X > 0 then
		begin
			Result := IfThen(X=1, -1, +1);
			S := Copy(S, 2, MaxInt);
		end
		else
			Result := 0;
	end;

var
	S, V, PS: String;
	i, P, PV: Integer;
	Ini: TIniFile;
	Act: TDecksActionKind;
	//Ctrl: TMIDIControl;
	sl: TStringList;
begin
	InDevice   := -1;
	OutDevice  := -1;
	DeviceName := '';

	for Act in TDecksActionKind do
	begin
		WriteStr(S, Act);
		ActionNames[Act] := UpperCase(S.Replace('_', '.', [rfReplaceAll]));
		Leds[Act, 1] := -1;
		Leds[Act, 2] := -1;
	end;

	for i := 0 to 255 do
		with Controls[i] do
		begin
			Enabled := False;
			Action.Kind  := NO_ACTION;
			Action.Param := 0;
		end;

	S := Config.Controller.Config;
	if S = '' then Exit;
	S := IncludeTrailingPathDelimiter(Config.Path + 'midi') + S;
	if not FileExists(S) then Exit;

	Ini := TIniFile.Create(S);
	Ini.Options := [];

	Debug := Ini.ReadBool('info', 'debug', False);

	DeviceName := Ini.ReadString('info', 'devicename', '');
	if DeviceName <> '' then
	begin
		Input := TRtMidiIn.Create();
		InputDeviceList := Input.GetDeviceList;
		for i := 0 to InputDeviceList.Count-1 do
			if InputDeviceList[i] = DeviceName then
			begin
				InDevice := i;
				Input.OpenPort(i);
				Input.SetCallback(MIDIInCallback, nil);
				Break;
			end;

		Output := TRtMidiOut.Create();
		OutputDeviceList := Output.GetDeviceList;
		for i := 0 to OutputDeviceList.Count-1 do
			if OutputDeviceList[i] = DeviceName then
			begin
				OutDevice := i;
				Output.OpenPort(i);
				Break;
			end;

		ReadControllerSection(Ini, 'controller.buttons',  MIDI_CTRL_BUTTON);
		ReadControllerSection(Ini, 'controller.absolute', MIDI_CTRL_ABSOLUTE);
		ReadControllerSection(Ini, 'controller.relative', MIDI_CTRL_RELATIVE_TWOSCOMPLEMENT);

		for Act in TDecksActionKind do
		begin
			Leds[Act, 1] := Ini.ReadInteger('leds', ActionNames[Act] + '(1)', -1);
			Leds[Act, 2] := Ini.ReadInteger('leds', ActionNames[Act] + '(2)', -1);
		end;

		sl := TStringList.Create;

		Ini.ReadSection('groups', sl);
		for S in sl do
		begin
			i := GetControl(S);
			if i >= 0 then
				Controls[i].Group := UpperCase(Ini.ReadString('groups', S, ''));
		end;
		sl.Clear;

		Ini.ReadSection('mappings', sl);
		for S in sl do
		begin
			V := UpperCase(Ini.ReadString('mappings', S, ''));

			if V.Contains('()') then
			begin
				// control group; create mapping for all group members
				// e.g. PLAY=deck.play()
				PV := GetControlParamValue(V);
				V := V.Replace('()', '');
				PS := UpperCase(S);
				P := 1; // deck number
				for i := Low(Controls) to High(Controls) do
					if Controls[i].Group = PS then
						for Act in TDecksActionKind do
							if ActionNames[Act] = V then
							with Controls[i] do
							begin
								Enabled := True;
								Parameter := PV;
								Action.Kind  := Act;
								Action.Param := P;
								Inc(P);
								Break;
							end;
			end
			else
			begin
				// single control, e.g. A_PLAY=deck.play(1)
				i := GetControl(S);
				if i < 0 then Continue;

				PV := GetControlParamValue(V);
				P := V.IndexOf('('); // find deck number/param value
				if P > 0 then
				begin
					PS := Copy(V, P+1, MaxInt);
					PS := PS.Replace('(', '', [rfReplaceAll]);
					PS := PS.Replace(')', '', [rfReplaceAll]);
					V := Copy(V, 1, P);
					P := StrToInt(PS); // get param value
				end
				else
					P := 0;

				for Act in TDecksActionKind do
					if ActionNames[Act] = V then
					with Controls[i] do
					begin
						Enabled := True;
						Parameter := PV;
						Action.Kind  := Act;
						Action.Param := P;
						Break;
					end;
			end;
		end;

{
		sl.Clear;
		for Ctrl in Controls do
		if Ctrl.Enabled then
		begin
			sl.Add('['+Ctrl.Name+']');
			if Ctrl.Group <> '' then
				sl.Add(Format('group=%s', [Ctrl.Group]));
			sl.Add(Format('kind=%d', [Ctrl.Kind]));
			//sl.Add(Format('', [Ctrl.Enabled, Ctrl.HasLED]));
			if Ctrl.Action.Kind <> NO_ACTION then
				sl.Add(Format('action=%d (%d)', [Ctrl.Action.Kind, Ctrl.Action.Param]));
			//sl.Add(Format('', []));
			sl.Add('');
		end;
		sl.SaveToFile('O:\projects\decks\mididebug.txt');
}

		sl.Free;
	end;

	Ini.Free;
end;

procedure TMIDI.Uninit;
var
	Act: TDecksActionKind;
	i: Integer;
begin
	// turn off all leds on controller
	for i := 1 to 2 do
		for Act in TDecksActionKind do
			SetLed(Act, i=2, False);

	Input.Free;
	Output.Free;
	InputDeviceList.Free;
	OutputDeviceList.Free;
end;

procedure TMIDI.SetLed(Kind: TDecksActionKind; RightDeck: Boolean; LedState: Boolean = True);
var
	L: Integer;
begin
	if Outdevice >= 0 then
	begin
		L := Leds[Kind, IfThen(RightDeck, 2, 1)];
		if L >= 0 then
			Output.SendMessage([MIDI_NOTE_ON, L, LedState.ToInteger]);
	end;
end;

end.

