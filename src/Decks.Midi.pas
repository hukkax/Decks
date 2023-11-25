unit Decks.MIDI;

{$MODE DELPHI}
{.$DEFINE USEMIDI}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
	Classes, SysUtils, Generics.Collections,
	{$IFDEF USEMIDI}
	RtMidi,
	{$ENDIF}
	Decks.Config,
	LMessages;

const
	MaxDecks = 4;
	MidiMaxValue  = $3FFF;

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

// type=note|absolute|absolute-14bit|relative-signedbit|relative-signedbit-rev|relative-twoscomplement|relative-binaryoffset|relative-14bit|meter
type
	TMidiControlKind = (
		MIDI_CTRL_BUTTON,
		MIDI_CTRL_ABSOLUTE,
		MIDI_CTRL_ABSOLUTE_14BIT,
		MIDI_CTRL_RELATIVE_SIGNEDBIT,
		MIDI_CTRL_RELATIVE_SIGNEDBIT_REV,
		MIDI_CTRL_RELATIVE_TWOSCOMPLEMENT,
		MIDI_CTRL_RELATIVE_BINARYOFFSET,
		MIDI_CTRL_RELATIVE_14BIT,
		MIDI_CTRL_VUMETER
	);

const
	MidiControlKindNames: array[TMidiControlKind] of String = (
		'note',
		'absolute',
		'absolute-14bit',
		'relative-signedbit',
		'relative-signedbit-rev',
		'relative-twoscomplement',
		'relative-binaryoffset',
		'relative-14bit',
		'vumeter'
		);

type
	TMIDIShortMessage = DWord; // KK D0 D1 D2

	TMIDIShortMessageRec = packed record
	case Boolean of
		True: (
			Kind:   Byte;
			Data:   array[0..2] of Byte;
			);
		False: ( Raw: TMIDIShortMessage; );
	end;

	TMIDIShortMessages = TList<TMIDIShortMessage>;

	TMIDIMacro = class
	public
		Mode:      TAppMode;
		Condition: AnsiString;
		Messages:  TMIDIShortMessages;

		constructor Create;
		destructor  Destroy; override;
	end;

	TMIDIMacroList = TObjectList<TMIDIMacro>;

	TDecksActionKind = (
		NO_ACTION,			DECK_SHIFT,
		DECK_PLAY,			DECK_REVERSE,
		DECK_CUE,			DECK_LOAD,
		DECK_SEEK,			DECK_SEEK_BAR,
		DECK_SYNC_TOGGLE,	DECK_SYNC,
		DECK_AMP,			DECK_PFL,
		DECK_BEND,			DECK_BEND_UP,		DECK_BEND_DOWN,
		DECK_LOOP,			DECK_LOOP_SONG,		DECK_LOOP_ZONE,
		DECK_LOOP_HALVE,	DECK_LOOP_DOUBLE,
		DECK_HOTCUE,		DECK_HOTCUE_TEMP,	DECK_HOTCUE_SET,		DECK_HOTCUE_CLEAR,
		DECK_PADS_PAGE,
		DECK_FX_ENABLE,		DECK_FX_SELECT,		DECK_FX_SELECT_PARAM,	DECK_FX_PARAM,
		MIXER_CUE_MIX,		MIXER_CUE_LEVEL,	MIXER_CUE_MASTER,		MIXER_CUE_POSTFADER,
		MIXER_EQ_LOW,		MIXER_EQ_MID,		MIXER_EQ_HIGH,			MIXER_EQ_KILL,
		MIXER_FILTER,		MIXER_CROSSFADER,	MIXER_VOLUME,			MIXER_MASTER,
		MIXER_VUMETER,
		UI_LIST,			UI_SELECT,			UI_SELECT_CONTROL,
		UI_SELECT_TOGGLE,	UI_SELECT_OPEN,
		UI_SELECT_ENTER,	UI_SELECT_EXIT,
		UI_BUTTON,			UI_MENU
	);

	TDecksButtonKind = (
		BUTTON_TOOLBAR_VIEW_EFFECTS,
		BUTTON_TOOLBAR_VIEW_MIXER,
		BUTTON_TOOLBAR_VIEW_GRAPHLINES,
		BUTTON_TOOLBAR_VIEW_LEFTPANE,
		BUTTON_WINDOW_MINIMIZE,
		BUTTON_WINDOW_MAXIMIZE,
		BUTTON_WINDOW_CLOSE,
		BUTTON_FILE_CUE,
		BUTTON_FILE_LOAD_NEW,
		BUTTON_FILE_LOAD_DECK1,
		BUTTON_FILE_LOAD_DECK2,
		BUTTON_FILE_LOAD_DECK3,
		BUTTON_FILE_RENAME,
		BUTTON_FILE_DELETE
	);

const
	// list of TDecksActionKinds that expect a parameter
	// to specify a deck number or other byte parameter
	ParameterizedActions: set of TDecksActionKind = [
		DECK_SHIFT,
		DECK_PLAY,			DECK_REVERSE,
		DECK_CUE,			DECK_LOAD,
		DECK_SEEK,			DECK_SEEK_BAR,
		DECK_SYNC_TOGGLE,	DECK_SYNC,
		DECK_AMP,			DECK_PFL,
		DECK_BEND,			DECK_BEND_UP,		DECK_BEND_DOWN,
		DECK_LOOP,			DECK_LOOP_SONG,		DECK_LOOP_ZONE,
		DECK_LOOP_HALVE,	DECK_LOOP_DOUBLE,
		DECK_HOTCUE,		DECK_HOTCUE_TEMP,	DECK_HOTCUE_SET,		DECK_HOTCUE_CLEAR,
		DECK_PADS_PAGE,
		DECK_FX_ENABLE,		DECK_FX_SELECT,		DECK_FX_SELECT_PARAM,	DECK_FX_PARAM,
		MIXER_EQ_LOW,		MIXER_EQ_MID,		MIXER_EQ_HIGH,			MIXER_EQ_KILL,
		MIXER_FILTER,		MIXER_VOLUME,		MIXER_VUMETER,
		UI_SELECT_TOGGLE,	UI_SELECT_OPEN //temporary
	];

	MidiCtrlCode: array[TMidiControlKind] of Byte = (
		MIDI_CTRLCODE_BUTTON_PRESS,
		MIDI_CTRLCODE_ABSOLUTE,
		MIDI_CTRLCODE_ABSOLUTE,
		MIDI_CTRLCODE_RELATIVE, // twoscomplement
		MIDI_CTRLCODE_RELATIVE,
		MIDI_CTRLCODE_RELATIVE,
		MIDI_CTRLCODE_RELATIVE,
		MIDI_CTRLCODE_RELATIVE,
		MIDI_CTRLCODE_BUTTON_PRESS
	);

type
	TDecksAction = record
		Kind:  TDecksActionKind;
		Param: Byte;
	end;

	// a button/knob/other control on a midi device
	TMidiControl = class
		Data:     Word; // MSB
		Send:     Word; // outbound message or LSB for 14-bit input
		Value:    Word;
		Param:    DWord;
		Led:      Word;
		Kind:     TMidiControlKind; // <type>
		Deck:     Byte;
		DeadZone: Word;
		Action:   TDecksAction;
		Name:     String;
	end;

	TMIDIControls = TObjectList<TMIDIControl>;

	TMIDI = record
		{$IFDEF USEMIDI}
		Input:            TRtMidiIn;
		Output:           TRtMidiOut;
		{$ENDIF}
		Debug:            Boolean;
		DeviceName:       String;
		InDevice,
		OutDevice:        Integer;
		InputDeviceList,
		OutputDeviceList: TStringList;
		Controls:         TMIDIControls;
		ActionNames:      array [TDecksActionKind] of String;
		Macros:           TMIDIMacroList;

		procedure Init;
		procedure Uninit;

		procedure ClearLeds;
		procedure SetLed(Kind: TDecksActionKind; DeckNum: Byte; LedState: Boolean = True; Param: DWord = PARAM_ANY);
		procedure SetVUMeter(Deck: Byte; Vol: Single); inline;

		procedure ReadMidiMessages(Sl: TStrings; Kind: TAppMode);
		procedure SendMidiMessages(Kind: TAppMode; BoolParam: Boolean; Deck: Integer = 0; Param: Integer = 0);
		procedure SendMidiMessage(Code: TMIDIShortMessage);
	end;

	TExecuteParams = record
		Action:  TDecksAction;
		Kind:    TMidiControlKind;
		Value:   Integer;
		Param:   DWord;
		Pressed: Boolean;
		{.$IFDEF DEBUG}
		Raw:     String;
		{.$ENDIF}
	end;
	PExecuteParams = ^TExecuteParams;

	TControlRec = record
		Valid:  Boolean;
		IsNote: Boolean;
		Index:  Byte;
	end;

	function Evaluate(const Expr: String; deck, param: Integer; boolparam: Boolean): Boolean;


implementation

uses
	fpexprpars,
	IniFiles, Forms, Math,
	Form.Main;

var
	ModeNames: array[TAppMode] of String;
	Parser: TFPExpressionParser;
	VU: array[0..MaxDecks] of TMIDIShortMessageRec;

{$IFDEF USEMIDI}
procedure MIDIInCallback(Timestamp: Double; Data: PByte; Size: size_t; UserData: Pointer); cdecl;
var
	Ctrl, Ct: TMIDIControl;
	DataVal: Word;
	Params: PExecuteParams;
	i, DeadZone, CenterVal: Integer;
	S: String;
	IsLSB: Boolean = False;
begin
	if Size < 3 then Exit;

	if Data[0] = $80 then
	begin
		Data[0] := $90;
		Data[2] := $00;
	end;

	DataVal := (Data[0] shl 8) + Data[1];

	Ctrl := nil;
	for Ct in MIDI.Controls do
	begin
		if (Ct.Data = DataVal) or (Ct.Send = DataVal) then
		begin
			if Ct.Data <> DataVal then // matched Send value
			begin
				// is this the LSB for a 14-bit value?
				if Ct.Kind in [MIDI_CTRL_ABSOLUTE_14BIT, MIDI_CTRL_RELATIVE_14BIT] then
					IsLSB := True
				else
					Continue;
			end;
			Ctrl := Ct;
			Break;
		end;
	end;
	if Ctrl = nil then
	begin
		{$IFDEF DEBUG}
		Log('Unhandled message: ' + DataVal.ToHexString(4));
		{$ENDIF}
		Exit;
	end;

	New(Params);
	Params^.Kind := Ctrl.Kind;
	CenterVal := 0;

	{.$IFDEF DEBUG}
	if MIDI.Debug then
	begin
		S := '';
		for i := 0 to Size-1 do
			S := S + IntToHex(Data[i], 2) + ' ';
		Params^.Raw := S;
	end;
	{.$ENDIF}

	case Ctrl.Kind of

		// Note On/Off
		// 8x/9x
		MIDI_CTRL_BUTTON:
		begin
			Params^.Value := Ctrl.Value;
			//Params^.Value := Data[2] shl 7;
			Params^.Action  := Ctrl.Action;
			Params^.Pressed := (Data[0] and $F0) = MIDI_CTRLCODE_BUTTON_PRESS;
			if Params^.Pressed then
				Params^.Pressed := Data[2] = $7F; // !!!
			//if Ctrl.Send > 0 then
			//	MIDI.SendMidiMessage(MIDI.Leds[Ctrl.Action.Kind, Ctrl.Deck], IfThen(Params^.Pressed, $7F, $00));
		end;

		// Absolute
		// 0..127
		MIDI_CTRL_ABSOLUTE:
		begin
			Params^.Value := Data[2] shl 7;
			Params^.Action  := Ctrl.Action;
			Params^.Pressed := True;
			CenterVal := 64 shl 7;
		end;

		// Relative Two's Complement
		// Increases from 01 to 64, decrease from 127 to 65.
		MIDI_CTRL_RELATIVE_TWOSCOMPLEMENT:
		begin
			if Data[2] > 64 then
				Params^.Value := -Abs(128 - (Data[2] and 127)) * $40 // negative
			else
				Params^.Value := (Data[2] and 127) * $40; // positive
			Params^.Action  := Ctrl.Action;
			Params^.Pressed := True;
		end;

		// Relative Binary Offset
		// Increases from 65 to 127, decreases from 63 to 00.
		MIDI_CTRL_RELATIVE_BINARYOFFSET:
		begin
			if Data[2] < 64 then
				Params^.Value := -(64 - Data[2]) * $40 // negative
			else
			if Data[2] > 64 then
				Params^.Value := (Data[2] - 64) * $40; // positive
			Params^.Action  := Ctrl.Action;
			Params^.Pressed := True;
		end;

		// 14-Bit RPN/NRPN
		// 7-bit MSB, 7-bit LSB => $0..$3FFF
		MIDI_CTRL_ABSOLUTE_14BIT,
		MIDI_CTRL_RELATIVE_14BIT:
		begin
			if not IsLSB then
				Ctrl.Value := (Data[2] shl 7) + (Ctrl.Value and $7F)
			else
				Ctrl.Value := (Ctrl.Value and ($7F shl 7)) + (Data[2] and $7F);
			Params^.Value  := Ctrl.Value;
			Params^.Action := Ctrl.Action;
			CenterVal := 64 shl 7;
		end;

	else
		Dispose(Params);
		Exit;
	end;

	if (CenterVal <> 0) and (Ctrl.DeadZone <> 0) then
	begin
		DeadZone := Ctrl.DeadZone div 2;
		if (Params^.Value >= (CenterVal - DeadZone)) and (Params^.Value <= (CenterVal + DeadZone)) then
			Params^.Value := CenterVal;
	end;

	Params^.Param := Ctrl.Param;
	Params^.Action.Param := Ctrl.Deck;

//	Application.RemoveAsyncCalls(MainForm);
	Application.QueueAsyncCall(MainForm.ASyncExecute, {%H-}PtrInt(Params));
end;
{$ENDIF}

{ TMIDIMacro }

constructor TMIDIMacro.Create;
begin
	inherited;
	Messages := TMIDIShortMessages.Create;
end;

destructor TMIDIMacro.Destroy;
begin
	Messages.Free;
	inherited Destroy;
end;

procedure TMIDI.ReadMidiMessages(Sl: TStrings; Kind: TAppMode);
var
	M: TMIDIMacro = nil;

	procedure AddMacro(const sCondition: String = '');
	begin
		M := TMIDIMacro.Create;
		M.Mode := Kind;
		M.Condition := sCondition;
		Macros.Add(M);
	end;

var
	S, L: String;
	V: Integer;
	R: TMIDIShortMessageRec;
begin
	if Sl = nil then Exit;

	for L in Sl do
	begin
		if L.StartsWith(';') then Continue;

		if L.StartsWith('if') then
			AddMacro(Copy(L, 3, MaxInt))
		else
		begin
			S := '$' + Trim(L.Replace(' ', ''));
			if (TryStrToInt(S, V)) and (V > 0) and (V <= $FFFFFF) then
			begin
				if M = nil then
					AddMacro;
				R.Kind    := Ord(Kind);
				R.Data[0] := V shr 16 and $FF;
				R.Data[1] := V shr 8 and $FF;
				R.Data[2] := V and $FF;
				M.Messages.Add(R.Raw);
			end;
		end;
	end;
end;

procedure TMIDI.SendMidiMessage(Code: TMIDIShortMessage);
var
	R: TMIDIShortMessageRec;
begin
	{$IFDEF USEMIDI}
	R.Raw := Code;
	Output.SendMessage(R.Data);
	{$ENDIF}
end;

function Evaluate(const Expr: String; deck, param: Integer; boolparam: Boolean): Boolean;
var
	Res: TFPExpressionResult;
begin
	Parser.Identifiers.Clear;
	Parser.Identifiers.AddIntegerVariable('deck',   deck);
	Parser.Identifiers.AddIntegerVariable('param',  param);
	Parser.Identifiers.AddBooleanVariable('enable', boolparam);
	Parser.Expression := LowerCase(Expr);

	Res := Parser.Evaluate;
	Result := Res.ResBoolean;
end;

procedure TMIDI.SendMidiMessages(Kind: TAppMode; BoolParam: Boolean; Deck: Integer = 0; Param: Integer = 0);
var
	Macro: TMIDIMacro;
	Code: TMIDIShortMessage;
	R: TMIDIShortMessageRec;
begin
	{$IFDEF USEMIDI}
	if Macros <> nil then
	for Macro in Macros do
	begin
		if (Macro = nil) or (Macro.Mode <> Kind) then Continue;
		if (Macro.Condition.IsEmpty) or (Evaluate(Macro.Condition, Deck, Param, BoolParam)) then
		begin
			for Code in Macro.Messages do
			begin
				R.Raw := Code;
				Output.SendMessage(R.Data);
			end;
		end;
	end;
	{$ENDIF}
end;

procedure TMIDI.SetVUMeter(Deck: Byte; Vol: Single);
var
	B: Byte;
begin
	if Deck <= MaxDecks then
	with VU[Deck] do
	begin
		if Data[0] <> 0 then
		begin
			B := Min($7F, Trunc(Vol * Data[2]));
			if B <> Kind then // don't send if value unchanged
			begin
				Kind := B;
				Output.SendMessage([Data[0], Data[1], B]);
			end;
		end;
	end;
end;

procedure TMIDI.Init;

	{function GetControlParamValue(var S: String): Integer;
	var
		X: Integer;
	begin
		if S.IsEmpty then Exit(0);
		X := Pos(S[1], '-+');
		if X > 0 then
		begin
			Result := IfThen(X=1, -1, +1);
			S := Copy(S, 2, MaxInt);
		end
		else
			Result := 0;
	end;}

	function GetActionKind(S: String): TDecksActionKind;
	var
		Act: TDecksActionKind;
	begin
		S := UpperCase(S);
		for Act in TDecksActionKind do
			if ActionNames[Act] = S then
				Exit(Act);
		Result := NO_ACTION;
	end;

	function GetHexWord(const SS: String): Word;
	var
		S: String;
		V: Integer;
	begin
		Result := 0;
		S := Trim(SS.Replace(' ', ''));
		S := S.Replace('$', '');
		if Length(S) = 4 then
		begin
			S := '$' + S;
			if (TryStrToInt(S, V)) and (V > 0) and (V <= $FFFF) then
				Result := V;
		end;
	end;

const
	NonMappingSections = 'info|debug|';
var
	S, PS, Sect: String;
	i, X: Integer;
	Ini: TMemIniFile;
	Act: TDecksActionKind;
	Ctrl, Dummy: TMIDIControl;
	CC: TMidiControlKind;
	sl, Sections: TStringList;
	Mode: TAppMode;
begin
	InDevice   := -1;
	OutDevice  := -1;
	DeviceName := '';

	Parser := TFPExpressionParser.Create(nil);
	Parser.Builtins := [bcBoolean];

	Macros := TMIDIMacroList.Create(True);

	for Act in TDecksActionKind do
	begin
		WriteStr(S, Act);
		ActionNames[Act] := UpperCase(S.Replace('_', '.', [rfReplaceAll]));
	end;

	S := Config.Controller.Config;
	if S = '' then Exit;
	S := IncludeTrailingPathDelimiter(Config.Path + 'midinew') + S;
	if not FileExists(S) then Exit;

	Ini := TMemIniFile.Create(S);
	Ini.Options := [];

	// Read device info
	//
	Sect := 'info';
	Debug := Ini.ReadBool(Sect, 'debug', False);
	DeviceName := Ini.ReadString(Sect, 'devicename', '');

	if DeviceName <> '' then
	begin
		sl := TStringList.Create;
		Sections := TStringList.Create;

		Controls := TMIDIControls.Create(True);

		{$IFDEF USEMIDI}
		Input := TRtMidiIn.Create(RTMIDI_API_UNSPECIFIED, AppName, 100);
		InputDeviceList := Input.GetDeviceList;
		for i := 0 to InputDeviceList.Count-1 do
		begin
			if InputDeviceList[i] <> DeviceName then Continue;
			InDevice := i;
			Input.OpenPort(i);
			Input.SetCallback(MIDIInCallback, nil);
			Break;
		end;

		Output := TRtMidiOut.Create(RTMIDI_API_UNSPECIFIED, AppName, 100);
		OutputDeviceList := Output.GetDeviceList;
		for i := 0 to OutputDeviceList.Count-1 do
			if OutputDeviceList[i] = DeviceName then
			begin
				OutDevice := i;
				Output.OpenPort(i);
				Break;
			end;
		{$ENDIF}

		for i := 0 to MaxDecks do
			VU[i] := Default(TMIDIShortMessageRec);

		// Read the control mappings
		//
		Ini.ReadSections(Sections);

		Dummy := TMIDIControl.Create;

		for Sect in Sections do
		begin
			S := UpperCase(Sect);

			if S.StartsWith('MODE_') then
			begin
				Ini.ReadSectionRaw(Sect, Sl);

				for Mode in TAppMode do
				begin
					if ModeNames[Mode] = S then
					begin
						ReadMidiMessages(Sl, Mode);
						Break;
					end;
				end;

				Continue;
			end
			else
			if NonMappingSections.Contains(LowerCase(Sect) + '|') then Continue;

			Ini.ReadSection(Sect, sl);

			Dummy.Name := Sect;
			Dummy.Action.Kind := NO_ACTION;
			Dummy.Kind := MIDI_CTRL_BUTTON;
			Dummy.Param := PARAM_ANY;
			Dummy.Led := 0;

			S := Trim(UpperCase(Ini.ReadString(Sect, 'action', '')));
			if S = '' then Continue;

			S := S.Replace(':', ' ');
			X := S.IndexOf(' ');
			if X > 0 then
			begin
				PS := Trim(Copy(S, X+1, 4));
				S  := Trim(Copy(S, 1, X));

				if PS = '+' then
					Dummy.Param := VAL_BROWSE_NEXT
				else
				if PS = '-' then
					Dummy.Param := VAL_BROWSE_PREV
				else
					Dummy.Param := StrToIntDef(PS, 0);
			end;

			Dummy.Action.Kind := GetActionKind(S);

			if Dummy.Action.Kind = MIXER_VUMETER then
				Dummy.Kind := MIDI_CTRL_VUMETER
			else
			begin
				S := LowerCase(Ini.ReadString(Sect, 'type', ''));
				for CC in TMidiControlKind do
					if S = MidiControlKindNames[CC] then
					begin
						Dummy.Kind := CC;
						Break;
					end;
			end;

			Dummy.DeadZone := Ini.ReadInteger(Sect, 'deadzone', 0);
			if Dummy.DeadZone <= $FF then
				Dummy.DeadZone := Dummy.DeadZone * $40;

			S := Ini.ReadString(Sect, 'led', '');
			if S <> '' then
			begin
				if 'yes true 1'.Contains(LowerCase(S)) then
					Dummy.Led := 1 // forward read to below
				else
					Dummy.Led := GetHexWord(S);
			end;

			for i := 0 to MaxDecks do
			begin
				S := Trim(Ini.ReadString(Sect, IntToStr(i), ''));
				if S = '' then Continue;

				Ctrl := TMIDIControl.Create;

				Ctrl.Name := Dummy.Name;
				Ctrl.Kind := Dummy.Kind;
				Ctrl.Action := Dummy.Action;
				Ctrl.Deck := i;
				Ctrl.Param := Dummy.Param;
				Ctrl.DeadZone := Dummy.DeadZone;
				if Dummy.Led > 1 then
					Ctrl.Led := Dummy.Led;

				if not S.Contains('|') then // fixme check kind
				begin
					if Length(S) < 4 then
						S := '90' + S; // default to Note On
					Ctrl.Data := GetHexWord(S);
					Ctrl.Send := Ctrl.Data;
					if Dummy.Led = 1 then
						Ctrl.Led := Ctrl.Send;
				end
				else
				begin
					S := Trim(S.Replace(' ', ''));
					X := Pos('|', S);
					Ctrl.Data := GetHexWord(Copy(S, 1, X-1));
					Ctrl.Send := GetHexWord(Copy(S, X+1, 8));
				end;

				if Ctrl.Kind = MIDI_CTRL_VUMETER then
				begin
					if (Ctrl.Param = 0) or (Ctrl.Param > $7F) then Ctrl.Param := $7F;
					VU[i].Data[0] := Ctrl.Send shr 8;
					VU[i].Data[1] := Ctrl.Send and $FF;
					VU[i].Data[2] := Ctrl.Param;
					FreeAndNil(Ctrl);
				end
				else
					Controls.Add(Ctrl);
			end;
		end; // Sections

		if Debug then
		begin
			sl.Clear;
			for Ctrl in Controls do
			begin
				sl.Add('['+Ctrl.Name+']');
				sl.Add(Format('Kind=%s', [MidiControlKindNames[Ctrl.Kind]]));
				if Ctrl.Data > 0 then
					sl.Add(Format('Data=%s', [Ctrl.Data.ToHexString(4)]));
				if Ctrl.Send > 0 then
					sl.Add(Format('Send=%s', [Ctrl.Send.ToHexString(4)]));
				if Ctrl.Deck > 0 then
					sl.Add(Format('Deck=%d', [Ctrl.Deck]));
				if Ctrl.Param <> PARAM_ANY then
					sl.Add(Format('Param=%d', [Ctrl.Param and $FFFF]));
				if Ctrl.Led > 0 then
					sl.Add(Format('Led=%s', [Ctrl.Led.ToHexString(4)]));
				if Ctrl.Action.Kind <> NO_ACTION then
					sl.Add(Format('action=%s (%d)', [ActionNames[Ctrl.Action.Kind], Ctrl.Action.Param]));
				//sl.Add(Format('', []));
				sl.Add('');
			end;
			for i := 0 to MaxDecks do
				sl.Add(Format('VUmeter.%d=%s %s %s', [i, VU[i].Data[0].ToHexString, VU[i].Data[1].ToHexString, VU[i].Data[2].ToHexString]));
			sl.SaveToFile(Config.AppPath + 'mididebug.txt');
		end;

		Dummy.Free;
		sl.Free;
		Sections.Free;

		ClearLeds;
		SendMidiMessages(MODE_APP_INIT, True);
	end;

	Ini.Free;
end;

procedure TMIDI.Uninit;
begin
	{$IFDEF USEMIDI}
	FreeAndNil(Input);
	FreeAndNil(Output);
	{$ENDIF}
	FreeAndNil(Macros);
	FreeAndNil(InputDeviceList);
	FreeAndNil(OutputDeviceList);
	FreeAndNil(Controls);
	FreeAndNil(Parser);
end;

procedure TMIDI.ClearLeds;
var
	Ctrl: TMidiControl;
	L: Word;
begin
	{$IFDEF USEMIDI}
	// turn off all leds on controller
	for Ctrl in Controls do
		if Ctrl.Led <> 0 then
		begin
			L := Ctrl.Led;
			Output.SendMessage([L shr 8, L and $FF, $00]);
			//SetLed(Ctrl.Action.Kind, Ctrl.Deck, False);
		end;
	{$ENDIF}
end;

procedure TMIDI.SetLed(Kind: TDecksActionKind; DeckNum: Byte; LedState: Boolean = True; Param: DWord = PARAM_ANY);
{$IFDEF USEMIDI}
var
	L: Word;
	S: String;
	Ctrl: TMIDIControl;
{$ENDIF}
begin
	{$IFDEF USEMIDI}
	if Outdevice >= 0 then
	begin
		{$IFDEF DEBUG}
		if Debug then
		begin
			WriteStr(S, Kind);
			MainForm.Memo1.Lines.Add(Format('LED %s = %s', [S, LedState.ToString]));
		end;
		{$ENDIF}

		//L := Leds[Kind, IfThen(RightDeck, 2, 1)];
		L := 0;

		for Ctrl in Controls do
			if (Ctrl.Action.Kind = Kind) and (Ctrl.Deck = DeckNum) then
			begin
				if (Param = PARAM_ANY) or (Ctrl.Param = Param) then
				begin
					L := Ctrl.Led;
					Output.SendMessage([L shr 8, L and $FF, IfThen(LedState, $7F, $00)])
				end;
			end;

(*		if L > 0 then
			Output.SendMessage([L shr 8, L and $FF, IfThen(LedState, $7F, $00)])
		{$IFDEF DEBUG}
		else
			MainForm.Memo1.Lines.Add(' LED FAIL')
		{$ENDIF};*)
	end;
	{$ENDIF}
end;

procedure GetModeNames;
var
	M: TAppMode;
	S: String;
begin
	for M in TAppMode do
	begin
		WriteStr(S, M);
		ModeNames[M] := UpperCase(S);
	end;
end;

initialization

	GetModeNames;

end.

