program Decks3;

{$mode Delphi}
{$INLINE ON}

uses
	{$IFDEF UNIX}cthreads, Classes,{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	{$IFDEF WINDOWS}{$IFNDEF DEBUG}uDarkStyleParams, uMetaDarkStyle, uDarkStyleSchemes,{$ENDIF}{$ENDIF}
	Forms, lazcontrols, Form.Main, Frame.Deck,
	Decks.Audio, Decks.Song, Decks.Beatgraph,
	Decks.SongInfo, Decks.TagScanner, Decks.Effects, TextInputDialog,
	Form.Tracklist, FocusRectangleUnit, Form.Config, Decks.TapTempo;

{$R *.res}

{$IFDEF UNIX}
	// on Unix we need to initialize the threading system before
	// using custom callbacks with BASS or we crash!
	type TDummyThread = class(TThread)
		procedure Execute; override;
	end;
	procedure TDummyThread.Execute; begin end;
{$ENDIF}

begin
	{$IFDEF UNIX}
	with TDummyThread.Create(False) do
	begin
		WaitFor;
		Free;
	end;
	{$ENDIF}

	{$IF declared(useHeapTrace)}
	GlobalSkipIfNoLeaks := True;
	SetHeapTraceOutput('trace.log');
	{$ENDIF}

	Application.Title := 'CaniMix';
	RequireDerivedFormResource := True;

	{$IFDEF WINDOWS}{$IFNDEF DEBUG}
	PreferredAppMode := pamAllowDark;
	DefaultDark.DrawControl.CustomDrawPushButtons := True;
	uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
	{$ENDIF}{$ENDIF}

	Application.Initialize;
	Application.CreateForm(TMainForm, MainForm);
	Application.CreateForm(TFormTracklist, FormTracklist);
	Application.CreateForm(TConfigForm, ConfigForm);
	Application.Run;
end.

