program Decks3;

{$mode Delphi}
{$INLINE ON}

uses
	{$IFDEF UNIX}cthreads, Classes,{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, Form.Main, Frame.Deck,
	Decks.Audio, Decks.Song, Decks.Beatgraph,
	Decks.SongInfo, Decks.TagScanner, Decks.Effects, TextInputDialog,
	Form.Tracklist;

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
	Application.Title := 'Decks 3';
	RequireDerivedFormResource := True;
	Application.Initialize;
	Application.CreateForm(TMainForm, MainForm);
	Application.CreateForm(TFormTracklist, FormTracklist);
	Application.Run;
end.

