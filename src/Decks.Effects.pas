unit Decks.Effects;

{$mode Delphi}
{$INLINE ON}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
	Classes, SysUtils, //Forms, Menus,
	FGL, BASS, BASS_FX;

type
	TEQBand = ( EQ_BAND_LOW, EQ_BAND_MID, EQ_BAND_HIGH );
	TFilterBand = ( FILTER_LOWPASS, FILTER_HIGHPASS );

const
	CenterFreqs: array[TEQBand] of Word = (125, 1000, 8000);

	STR_MIX_DRY = 'Dry';
	STR_MIX_WET = 'Wet';
	STR_MIX_DRY_DESC = 'Dry signal mix';
	STR_MIX_WET_DESC = 'Wet signal mix';
	STR_MIX_FEEDBACK = 'Output signal to feed back into input';

type
	TEQBandData = record
		CenterFreq: Word;   // Hz
		Gain,
		KilledGain: Single; // -15..+15
		Killed:     Boolean;
	end;

	TEqualizer = record
		Handle:   HFX;
		Stream:   HSTREAM;
		Band:     array[TEQBand] of TEQBandData; // low, mid, high
		FX:       BASS_BFX_PEAKEQ;

		procedure Reset;
		procedure Init(TheStream: HSTREAM);
		procedure SetEQ(BandNum: TEQBand; Gain: Single);
		procedure Apply(BandNum: Integer = -1);
	end;


	TEffectParam = class;
	TEffectParamChangeEvent = procedure (Param: TEffectParam) of Object;

	TEffectParam = class
	private
		FMin, FMax: Single;
		FValue:     PSingle;

		function    GetValue: Single;
		procedure   SetValue(AValue: Single);
	public
		Name,
		Caption:  String;
		Result:   Integer;

		OnChange: TEffectParamChangeEvent;

		constructor Create(const AName, ACaption: String; AMin, AMax: Single; var ValuePtr: Single);

		property Value: Single read GetValue write SetValue;
		property Min:   Single read FMin;
		property Max:   Single read FMax;
	end;

	TEffectParams = TFPGObjectList<TEffectParam>;

	TEffectPreset = class
	public
		Name:       String;
		Values:     array of Single;
	end;

	TEffectPresets = TFPGObjectList<TEffectPreset>;

	TBaseEffect = class
	private
		FEnabled:   Boolean;
		FxType:     DWord;
		BFXPointer: Pointer;
		FStream:    HSTREAM;

		procedure   SetEnabled(Enable: Boolean);
		procedure   SetStream(Value: HSTREAM);
		procedure   DoSetEnabled(Enable: Boolean);
	public
		FxHandle:   HFX;

		Name:       String;
		Params:     TEffectParams;
		Presets:    TEffectPresets;

		function    AddParam(var ValuePtr: Single; const AName: String;
		            AMin, AMax: Single; const ACaption: String): TEffectParam;
		procedure   AddPreset(const Values: array of Single; const AName: String);
		procedure   ApplyPreset(Preset: TEffectPreset);
		procedure   ParamChanged(Param: TEffectParam);

		property    Enabled: Boolean read FEnabled write SetEnabled;
		property	Stream:  HSTREAM read FStream  write SetStream;

		constructor Create(AFxType: DWord; BFXObject: Pointer; const AName: String);
		destructor  Destroy; override;
	end;

	TFxEcho = class(TBaseEffect)
		BFX: BASS_BFX_ECHO4;
		constructor Create;
	end;

	TFxReverb = class(TBaseEffect)
		BFX: BASS_BFX_FREEVERB;
		constructor Create;
	end;

	TFxPhaser = class(TBaseEffect)
		BFX: BASS_BFX_PHASER;
		constructor Create;
	end;

	TFxChorus = class(TBaseEffect)
		BFX: BASS_BFX_CHORUS;
		constructor Create;
	end;

	TFxDistortion = class(TBaseEffect)
		BFX: BASS_BFX_DISTORTION;
		constructor Create;
	end;

	TFxCompressor = class(TBaseEffect)
		BFX: BASS_BFX_COMPRESSOR2;
		constructor Create;
	end;



implementation

uses
	Math,
	Form.Main,
	Decks.Config, Decks.BeatGraph;

// ================================================================================================
// TEffectParam
// ================================================================================================

constructor TEffectParam.Create(const AName, ACaption: String; AMin, AMax: Single; var ValuePtr: Single);
begin
	inherited Create;

	Name := AName;
	Caption := ACaption;
	FMin := AMin;
	FMax := AMax;
	FValue := @ValuePtr;
end;

function TEffectParam.GetValue: Single;
begin
	Result := FValue^;
end;

procedure TEffectParam.SetValue(AValue: Single);
begin
	Result := 0;
	AValue := EnsureRange(AValue, FMin, FMax);
	if AValue <> FValue^ then
	begin
		FValue^ := AValue;
		Result := 1;
		if Assigned(OnChange) then
		begin
			Result := 2;
			OnChange(Self);
		end;
	end;
end;


// ================================================================================================
// TBaseEffect
// ================================================================================================

constructor TBaseEffect.Create(AFxType: DWord; BFXObject: Pointer; const AName: String);
begin
	inherited Create;

	Name := AName;
	FStream := 0;
	FxType := AFxType;
	BFXPointer := BFXObject;

	Params := TEffectParams.Create(True);
	Presets := TEffectPresets.Create(True);
end;

destructor TBaseEffect.Destroy;
begin
	Params.Free;
	Presets.Free;

	inherited Destroy;
end;

function TBaseEffect.AddParam(var ValuePtr: Single; const AName: String;
	AMin, AMax: Single; const ACaption: String): TEffectParam;
begin
	Result := TEffectParam.Create(AName, ACaption, AMin, AMax, ValuePtr);
	if Result <> nil then
	begin
		Result.OnChange := ParamChanged;
		Params.Add(Result);
	end;
end;

procedure TBaseEffect.ParamChanged(Param: TEffectParam);
begin
	if (FxHandle <> 0) and (Param <> nil) and (BFXPointer <> nil) then
	begin
		if BASS_FXSetParameters(FxHandle, BFXPointer) then
			Param.Result := 666
		else
			Param.Result := BASS_ErrorGetCode;
	end;
end;

procedure TBaseEffect.AddPreset(const Values: array of Single; const AName: String);
var
	Preset: TEffectPreset;
	i: Integer;
begin
	Preset := TEffectPreset.Create;
	Preset.Name := AName;
	SetLength(Preset.Values, Length(Values));
	for i := 0 to High(Values) do
		Preset.Values[i] := Values[i];
	Presets.Add(Preset);
end;

procedure TBaseEffect.SetEnabled(Enable: Boolean);
begin
	FEnabled := Enable;
	DoSetEnabled(Enable);
end;

procedure TBaseEffect.DoSetEnabled(Enable: Boolean);
begin
	if FStream = 0 then Exit;
	if Enable then
	begin
		if FxHandle = 0 then
		begin
			FxHandle := BASS_ChannelSetFX(FStream, FxType, 1);
			if FxHandle <> 0 then
				BASS_FXSetParameters(FxHandle, BFXPointer);
		end;
	end
	else
	if FxHandle <> 0 then
	begin
		BASS_ChannelRemoveFX(FStream, FxHandle);
		FxHandle := 0;
	end;
end;

procedure TBaseEffect.SetStream(Value: HSTREAM);
begin
	if FStream = Value then Exit;

	DoSetEnabled(False);
	FStream := Value;
	if FEnabled then
		DoSetEnabled(True);
end;

procedure TBaseEffect.ApplyPreset(Preset: TEffectPreset);
var
	i: Integer = 0;
	V: Single;
begin
	for V in Preset.Values do
	begin
		if i >= Params.Count then Break;
		Params[i].Value := V;
		Inc(i);
	end;
end;

// ================================================================================================
// Effect Constructors
// ================================================================================================

constructor TFxEcho.Create;
begin
	inherited Create(BASS_FX_BFX_ECHO4, @BFX, 'Echo');
	BFX.lChannel := BASS_BFX_CHANALL;

	AddParam(BFX.fDryMix,	STR_MIX_DRY,	+0.0,	+1.0,	STR_MIX_DRY_DESC);
	AddParam(BFX.fWetMix,	STR_MIX_WET,	+0.0,	+1.0,	STR_MIX_WET_DESC);

	AddParam(BFX.fFeedback,	'Feedback', -1.0,	+1.0,	STR_MIX_FEEDBACK);
	AddParam(BFX.fDelay,	'Delay',	+0.0,	+10.0,	'Delay in seconds');

	AddPreset([1.0,	1.0,  0.0, 0.2], 'Small Echo');
	AddPreset([1.0,	1.0,  0.7, 0.5], 'Many Echoes');
	AddPreset([1.0,	1.0, -0.7, 0.8], 'Reverse Echoes');
	AddPreset([0.5,	0.8,  0.5, 0.1], 'Robotic Voice');

	ApplyPreset(Presets.First);
end;

constructor TFxReverb.Create;
begin
	inherited Create(BASS_FX_BFX_FREEVERB, @BFX, 'Reverb');
	BFX.lChannel := BASS_BFX_CHANALL;

	AddParam(BFX.fDryMix,	STR_MIX_DRY,	+0.0,	+1.0,	STR_MIX_DRY_DESC);
	AddParam(BFX.fWetMix,	STR_MIX_WET,	+0.0,	+1.0,	STR_MIX_WET_DESC);

	AddParam(BFX.fRoomSize,	'RoomSize',	+0.0,	+1.0,	'Room size');
	AddParam(BFX.fDamp,		'Damp',		+0.0,	+1.0,	'Damping');
	AddParam(BFX.fWidth,	'Width',	+0.0,	+1.0,	'Stereo width');

	AddPreset([1.0, 1.0, 0.5, 0.5, 0.5], 'Default');

	ApplyPreset(Presets.First);
end;

constructor TFxPhaser.Create;
begin
	inherited Create(BASS_FX_BFX_PHASER, @BFX, 'Phaser');
	BFX.lChannel := BASS_BFX_CHANALL;

	AddParam(BFX.fDryMix,	STR_MIX_DRY,	+0.0,	+2.0,	STR_MIX_DRY_DESC);
	AddParam(BFX.fWetMix,	STR_MIX_WET,	+0.0,	+2.0,	STR_MIX_WET_DESC);

	AddParam(BFX.fFeedback,	'Feedback', -1.0,	+1.0,		STR_MIX_FEEDBACK);
	AddParam(BFX.fRate,		'Rate',		+0.0,	+10.0,		'Rate of sweep in cycles per second');
	AddParam(BFX.fRange,	'Range',	+0.0,	+10.0,		'Sweep range in octaves');
	AddParam(BFX.fFreq,		'Freq',		+0.0,	+1000.0,	'Base frequency of sweep range');

	AddPreset([1.0,	 1.0,	 0.0,	1.0,	4.0,	100.0],	'Phase shift');
	//AddPreset([1.0,	-1.0,	-0.6, 	0.2, 	6.0, 	100.0],	'Slow invert phase shift with feedback');
	AddPreset([1.0,	 1.0,	 0.0, 	1.0, 	4.3, 	 50.0],	'Basic phase');
	AddPreset([1.0,	 1.0,	 0.6, 	1.0, 	4.0, 	 40.0],	'Phase with feedback');
	AddPreset([1.0,	 1.0,	 0.0, 	1.0, 	7.0, 	100.0],	'Medium phase');
	AddPreset([1.0,	 1.0,	 0.0, 	1.0, 	7.0, 	400.0],	'Fast phase');
	//AddPreset([1.0,	-1.0,	-0.2, 	1.0, 	7.0, 	200.0],	'Invert with invert feedback');
	AddPreset([1.0,	 1.0,	 0.6, 	1.0, 	4.0, 	 60.0],	'Tremolo Wah');

	ApplyPreset(Presets.First);
end;

constructor TFxChorus.Create;
begin
	inherited Create(BASS_FX_BFX_CHORUS, @BFX, 'Chorus');
	BFX.lChannel := BASS_BFX_CHANALL;

	AddParam(BFX.fDryMix,	STR_MIX_DRY,	-2.0,	+2.0,	STR_MIX_DRY_DESC);
	AddParam(BFX.fWetMix,	STR_MIX_WET,	-2.0,	+2.0,	STR_MIX_WET_DESC);

	AddParam(BFX.fFeedback,	'Feedback', -1.0,	+1.0,		STR_MIX_FEEDBACK);
	AddParam(BFX.fMinSweep,	'Min',	+0.0,	+6000.0,		'Minimum delay in ms');
	AddParam(BFX.fMaxSweep,	'Max',	+0.0,	+6000.0,		'Maximum delay in ms');
	AddParam(BFX.fRate,		'Rate',		+0.0,	+1000.0,	'Rate in ms/s');

	AddPreset([1.0,	 0.35,	0.5,	1.0,	  5.0,	  1.0],	'Flanger');
	AddPreset([0.7,	 0.25,	0.5,	1.0,	200.0,	 50.0],	'Exaggerated');
	AddPreset([0.9,	 0.45,	0.5,	1.0,	100.0,	 25.0],	'Motorcycle');
	AddPreset([0.9,	 0.35,	0.5,	1.0,	 50.0,	200.0],	'Devilish');
	AddPreset([0.9,	 0.35,	0.5,	1.0,	400.0,	200.0],	'Voices');
	AddPreset([0.9,	-0.20,	0.5,	1.0,	400.0,	400.0],	'Back chipmunk');
	AddPreset([0.9,	-0.40,	0.5,	1.0,	  2.0,	  1.0],	'Water');
	AddPreset([0.3,	 0.40,	0.5,	1.0,	 10.0,	  5.0],	'Airplane');

	ApplyPreset(Presets.First);
end;

constructor TFxDistortion.Create;
begin
	inherited Create(BASS_FX_BFX_DISTORTION, @BFX, 'Distortion');
	BFX.lChannel := BASS_BFX_CHANALL;

	AddParam(BFX.fDryMix,	STR_MIX_DRY,	-5.0,	+5.0,	STR_MIX_DRY_DESC);
	AddParam(BFX.fWetMix,	STR_MIX_WET,	-5.0,	+5.0,	STR_MIX_WET_DESC);

	AddParam(BFX.fDrive,	'Drive',	+0.0,	+5.0,	'Distortion drive');
	AddParam(BFX.fFeedback,	'Feedback', -1.0,	+1.0,	STR_MIX_FEEDBACK);
	AddParam(BFX.fVolume,	'Volume',	+0.0,	+2.0,	'Distortion volume');

	AddPreset([-2.95,	-0.05,	0.0,	-0.18,	0.25],	'Soft');
	AddPreset([ 1.00,	 0.2,	1.0,	 0.10, 	1.00],	'Medium');
	AddPreset([ 0.00, 	 1.0,	1.0, 	 0.00,	1.00],	'Hard');
	AddPreset([ 0.00,	 5.0,	1.0,	 0.10,	1.00],	'Extreme');

	ApplyPreset(Presets.First);
end;

constructor TFxCompressor.Create;
begin
	inherited Create(BASS_FX_BFX_COMPRESSOR2, @BFX, 'Compressor');
	BFX.lChannel := BASS_BFX_CHANALL;

	AddParam(BFX.fGain,			'Gain',			+0.0,	+60.0,
		'Output gain of signal after compression');
	AddParam(BFX.fThreshold,	'Threshold',	-60.0,	+0.0,
		'Point at which compression begins');
	AddParam(BFX.fRatio,		'Ratio',		+1.0,	+100.0,
		'Compression ratio');
	AddParam(BFX.fAttack,		'Attack',		+0.01,	+500.0,
		'Time before compression reaches its full value');
	AddParam(BFX.fRelease,		'Release',		+0.01,	+5000.0,
		'Speed at which compression is stopped after input drops below Threshold');

	AddPreset([5, -15, 3, 20, 500],	'Default');

	ApplyPreset(Presets.First);
end;


// ================================================================================================
// TEqualizer
// ================================================================================================

procedure TEqualizer.Reset;
var
	i: TEQBand;
begin
	Stream := 0;
	for i in TEQBand do
	begin
		Band[i].CenterFreq := CenterFreqs[i];
		Band[i].Gain := 1.0;
	end;
end;

procedure TEqualizer.Init(TheStream: HSTREAM);
begin
	Stream := TheStream;
	Handle := BASS_ChannelSetFX(Stream, BASS_FX_BFX_PEAKEQ, 0);
	Assert(Handle <> 0, 'TEqualizer.Init: BASS_ChannelSetFX failed! Stream=' + TheStream.ToString);
	Apply;
end;

procedure TEqualizer.SetEQ(BandNum: TEQBand; Gain: Single);
begin
	Assert(BandNum <= High(Band), 'TEqualizer.SetEQ: Invalid BandNum');
	Band[BandNum].Gain := Gain;
	Apply(Ord(BandNum));
end;

procedure TEqualizer.Apply(BandNum: Integer = -1);
var
	i: TEQBand;
	B: Boolean;
begin
	with FX do
	begin
		fBandwidth := 2.0;
		fQ := 0;
		lChannel := BASS_BFX_CHANALL;
		for i in TEQBand do
		begin
			if (BandNum < 0) or (i = TEQBand(BandNum)) then
			begin
				lBand := Ord(i);
				fCenter := Band[i].CenterFreq;
				fGain := Band[i].Gain;
				B := BASS_FXSetParameters(Handle, @FX);
				Assert(B = True, 'TEqualizer.Apply: BASS_FXSetParameters failed! Handle=' + Handle.ToString);
			end;
		end;
	end;
	//for v := 0 to 2 do
	//	Audio_SetEqualizer(deck, v, d_fxEQ[deck].Gain[v]);
	//Audio_SetEQKill(deck, d_isEQKilled[deck]);
end;


end.

