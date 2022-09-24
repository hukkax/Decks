program DecksButtonTest;

{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}
	cthreads,
	{$ENDIF}
	{$IFDEF HASAMIGA}
	athreads,
	{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, luicontrols, Unit1, Unit2
	{ you can add units after this };

{$R *.res}

begin
	RequireDerivedFormResource := True;
	Application.Scaled := True;
	Application.Initialize;
	Application.CreateForm(TForm1, Form1);
	Application.Run;
end.

