unit Decks.TagScanner;

{$mode Delphi}

interface

uses
	SysUtils, Classes, //SyncObjs, LMessages,
	Decks.SongInfo,
	AudioTag, basetag, file_Wave, file_mp3, file_ogg;

type
	TTagScannerEventKind = ( tsStarted, tsItemProcessed, tsFinished );

	TTagScannerFileEvent = procedure(EventKind: TTagScannerEventKind; const Filename: String) of Object;
	TTagScannerTagsEvent = procedure(EventKind: TTagScannerEventKind; Tags: PSongTags) of Object;

	TTagScannerJob = class(TObject)
	private
		FThread:       TThread;
		FFileList:     TStringList;
		FTagsList:     TSongTags;
		Terminated,
		Queued:        Boolean;

		FCurrentFilename,
		FDirectory,
		FExtensions: String;
		FOnFileAdd:  TTagScannerFileEvent;
		FOnFileRead: TTagScannerTagsEvent;

		procedure DoExecute;
		procedure InternalFileProcessed(const Filename: String);
		procedure InternalFileAdded(const Filename: String);
		procedure FileProcessed;
		procedure FileAdded;
		function  GetCurrentFilename: String;
	public
		procedure Execute(UseThreads: Boolean = True);
		procedure Terminate;

		property Directory:  String read FDirectory  write FDirectory;
		property Extensions: String read FExtensions write FExtensions;
		property CurrentFilename: String   read GetCurrentFilename;
		property CurrentTags: TSongTags    read FTagsList;

		property OnFileAdded: TTagScannerFileEvent read FOnFileAdd  write FOnFileAdd;
		property OnTagsRead:  TTagScannerTagsEvent read FOnFileRead write FOnFileRead;

		procedure ThreadDone(Sender : TObject);
	end;

	TTagScannerThread = class(TThread)
	private
		FJob: TTagScannerJob;
	public
		constructor Create(AJob: TTagScannerJob);
		procedure Execute; override;
	end;

	function ReadTags(const Filename: String): TTagReader;
	function ReadFileTags(const Filename: String): TSongTags;


implementation

{$IFDEF DEBUG}
uses
	Dialogs;
{$ENDIF}

// ================================================================================================
// TTagScannerJob
// ================================================================================================

procedure TTagScannerJob.Execute(UseThreads: Boolean = True);
begin
	FFileList := TStringList.Create;
	Terminated := False;

	if UseThreads then
	begin
		FThread := TTagScannerThread.Create(Self);
		FThread.Start;
	end
	else
	begin
		FThread := nil;
		DoExecute;
		ThreadDone(Self);
	end;
end;

procedure TTagScannerJob.DoExecute;
var
	S, E: String;
	Info: TSearchRec;
begin
	// gather list of audio files we want to process
	if FindFirst(FDirectory + '*.*', 0, Info) = 0 then
	try
		if Assigned(FOnFileAdd) then FOnFileAdd(tsStarted, FDirectory);
		repeat
			E := LowerCase(ExtractFileExt(Info.Name));
			If Pos(E, FExtensions) > 0 then
				InternalFileAdded(FDirectory + Info.Name);
		until (FindNext(Info) <> 0) or Terminated;
	finally
		FindClose(Info);
		if Assigned(FOnFileAdd) then FOnFileAdd(tsFinished, FDirectory);
	end;

	// get tags for each file
	if Assigned(FOnFileRead) then FOnFileRead(tsStarted, nil);
	for S in FFileList do
	begin
		if Terminated then Exit;
		FTagsList := ReadFileTags(S);
		InternalFileProcessed(S);
	end;
	if Assigned(FOnFileRead) then FOnFileRead(tsFinished, nil);
end;

// signal a new file in file listing
procedure TTagScannerJob.InternalFileAdded(const Filename: String);
begin
	FFileList.Add(Filename);
	FCurrentFilename := Filename;
	TThread.Synchronize(FThread, FileAdded);
end;

// signal tag parsing finished for a file
procedure TTagScannerJob.InternalFileProcessed(const Filename: String);
begin
	FCurrentFilename := Filename;
	Queued := True;
	TThread.Synchronize(FThread, FileProcessed);
end;

procedure TTagScannerJob.FileAdded;
begin
	if Assigned(FOnFileAdd) then FOnFileAdd(tsItemProcessed, FCurrentFilename);
	Queued := False;
end;

procedure TTagScannerJob.FileProcessed;
begin
	if Assigned(FOnFileRead) then FOnFileRead(tsItemProcessed, @FTagsList);
	Queued := False;
end;

procedure TTagScannerJob.ThreadDone(Sender : TObject);
begin
	FDirectory := '';
	if Queued then
		TThread.RemoveQueuedEvents(FThread, FileProcessed);

	FThread := nil;
	FFileList.Free;
end;

procedure TTagScannerJob.Terminate;
begin
	Terminated := True;
end;

function TTagScannerJob.GetCurrentFilename: String;
begin
	Result := ExtractFileName(FCurrentFilename);
end;

// ================================================================================================
// TTagScannerThread
// ================================================================================================

constructor TTagScannerThread.Create(AJob: TTagScannerJob);
begin
	inherited Create(True);
	FJob := AJob;
	OnTerminate := FJob.ThreadDone;
end;

procedure TTagScannerThread.Execute;
begin
	FJob.DoExecute;
end;

// ================================================================================================
// Tag reading
// ================================================================================================

function Fix(const S: String): String;
var
	X: Integer;
begin
	X := Pos(#0, S);
	if X < 1 then
		Result := S
	else
		Result := Copy(S, 1, X-1);
end;

function ReadTags(const Filename: String): TTagReader;
var
	TagClass: TTagReaderClass;
begin
	Result := nil;
	if not FileExists(Filename) then Exit;
	try
		TagClass := IdentifyKind(Filename);
		Result := TagClass.Create;
		Result.LoadFromFile(Filename);
	except
	end;
end;

function ReadFileTags(const Filename: String): TSongTags;
var
	TagReader: TTagReader;
	Tags: TCommonTags;
	Info: TSongInfo;
	S: String;
	I: Integer;
begin
	Result := Default(TSongTags);

	TagReader := ReadTags(Filename);
	try
		if Assigned(TagReader) then
			Tags := TagReader.GetCommonTags
		else
			Tags := Default(TCommonTags);

		Result.Artist  := Fix(Tags.Artist);
		Result.Title   := Fix(Tags.Title);
		Result.Comment := Fix(Tags.Comment);
		Result.Genre   := Tags.Genre;
		if Length(Tags.Year) >= 4 then
		begin
			S := Copy(Tags.Year, 1, 4);
			if TryStrToInt(S, I) then
				Result.Year := Abs(I);
		end;
		Result.Info.Bitrate := TagReader.MediaProperty.Bitrate;
		Result.Info.Length := Tags.Duration / 1000; // ms->s
		Result.HasImage := Tags.HasImage;
	finally
		TagReader.Free;
	end;

	Info := GetSongInfo(ExtractFileName(Filename), nil);
	if Info.Initialized then
	begin
		if Info.Length >= 1.0 then
			Result.Info.Length  := Info.Length;
		if Info.Bitrate > 0 then
			Result.Info.Bitrate := Info.Bitrate;
		Result.Info.Amp := Info.Amp;
		Result.Info.BPM := Info.BPM;
	end;
end;


end.

