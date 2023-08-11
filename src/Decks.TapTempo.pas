// adapted from https://codepen.io/dxinteractive/pen/bpaMMy
unit Decks.TapTempo;

{$mode Delphi}

interface

uses
	Classes, SysUtils, DateUtils;

const
	TOTAL_TAP_VALUES = 64;
	MS_UNTIL_CHAIN_RESET = 2000;
	SKIPPED_TAP_THRESHOLD_LOW  = 1.75;
	SKIPPED_TAP_THRESHOLD_HIGH = 2.75;

type
	TTempoTap = record
	private
		BeatMS: Single;
		ResetTime,
		LastTapTime: TDateTime;
		LastTapSkipped: Boolean;
		TapDurationIndex: Integer;
		TapsInChain: Integer;
		TapDurations: array of Cardinal;

		function  GetAverageTapDuration: Single;
		function  DoTap(NowTime: TDateTime): Single;
		procedure ResetTapChain(NowTime: TDateTime);
	public
		procedure Init;
		procedure Reset;
		function  Tap: Single;
	end;

var
	TempoTap: TTempoTap;

implementation

uses
	Math;

function TTempoTap.GetAverageTapDuration: Single;
var
	i, amount: Integer;
	RunningTotal: Cardinal = 0;
begin
	amount := Min(TapsInChain, TOTAL_TAP_VALUES);
	if amount < 1 then Exit(ZeroValue);
	for i := 0 to amount-1 do
		Inc(RunningTotal, TapDurations[i]);
	Result := {Math.Floor}(RunningTotal / amount);
end;

function TTempoTap.DoTap(NowTime: TDateTime): Single;
var
	duration: Cardinal;
begin
	if NowTime = ResetTime then
	begin
		LastTapTime := NowTime;
		ResetTime := 0;
		Exit(ZeroValue);
	end;

	if TapsInChain < TOTAL_TAP_VALUES then
		Inc(TapsInChain);

	duration := Abs(MilliSecondsBetween(NowTime, LastTapTime));

	// detect if last duration was unreasonable
	if (TapsInChain > 1) and (not LastTapSkipped)
		and (duration > (BeatMS * SKIPPED_TAP_THRESHOLD_LOW))
		and (duration < (BeatMS * SKIPPED_TAP_THRESHOLD_HIGH)) then
	begin
		duration := Math.Floor(duration * 0.5);
		LastTapSkipped := True;
	end
	else
		LastTapSkipped := False;

	TapDurations[TapDurationIndex] := duration;
	Inc(TapDurationIndex);
	if TapDurationIndex > High(TapDurations) then
		TapDurationIndex := 0;

	LastTapTime := NowTime;

	Result := GetAverageTapDuration;
end;

function TTempoTap.Tap: Single;
var
    NowTime: TDateTime;
	NewBeatMS: Single;
begin
	NowTime := Now;

	// start a new tap chain if last tap was over an amount of beats ago
	if MilliSecondsBetween(NowTime, LastTapTime) >= MS_UNTIL_CHAIN_RESET then
		ResetTapChain(NowTime);

	NewBeatMS := DoTap(NowTime);
	if NewBeatMS >= 10.0 then
	begin
		BeatMS := NewBeatMS;
		Result := Round((60000 / BeatMS) + 0.5);
	end
	else
		Result := ZeroValue;
end;

procedure TTempoTap.ResetTapChain(NowTime: TDateTime);
var
	i: Integer;
begin
	TapsInChain := 0;
	TapDurationIndex := 0;
	LastTapSkipped := False;
	LastTapTime := NowTime;
	ResetTime := NowTime;
	for i := 0 to High(TapDurations) do
		TapDurations[i] := 0;
end;

procedure TTempoTap.Reset;
begin
	BeatMS := 500;
	ResetTapChain(Now);
end;

procedure TTempoTap.Init;
begin
	SetLength(TapDurations, TOTAL_TAP_VALUES);
	Reset;
end;


end.

