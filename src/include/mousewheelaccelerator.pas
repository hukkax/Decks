unit MouseWheelAccelerator;

{$mode Delphi}

interface

uses
	Classes, SysUtils, DateUtils;

type
	TWheelAcceleration = record
	private
		LastTime:     TDateTime;
		LastDeltaPos: Boolean;
		PreviousID:   Integer;
		AccelVal:     Single;
	public
		Acceleration:         Single;
		MaxAcceleration:      Single;
		MaxTimeBetweenEvents: Word; // max milliseconds between wheel events

		function Process(ID, Delta: Integer; AccelerationVal: Single = 0.0): Single;
	end;

var
	WheelAcceleration: TWheelAcceleration;


implementation

uses Math;


function TWheelAcceleration.Process(ID, Delta: Integer; AccelerationVal: Single = 0.0): Single;
var
	Span: Integer;
begin
	Result := 1.0;
	if AccelerationVal < 0.1 then AccelerationVal := Acceleration;

	if (ID = PreviousID) and (LastDeltaPos = (Delta > 0)) then
	begin
		Span := MilliSecondsBetween(Now, LastTime);
		if Span <= MaxTimeBetweenEvents then
		begin
			Delta := Round(Delta * AccelVal);
			AccelVal := AccelVal * AccelerationVal;
			if AccelVal >= 0 then
				Result := Min(AccelVal, MaxAcceleration)
			else
				Result := Max(AccelVal, -MaxAcceleration);
		end
		else
			AccelVal := AccelerationVal;
	end
	else
		AccelVal := AccelerationVal;

	PreviousID := ID;
	LastDeltaPos := (Delta > 0);
	LastTime := Now;
end;


initialization

	WheelAcceleration := Default(TWheelAcceleration);
	with WheelAcceleration do
	begin
		Acceleration := 1.2;
		MaxAcceleration := 30.0;
		MaxTimeBetweenEvents := 180;
	end;

end.

