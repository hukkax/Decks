unit Decks.TagScanner;

{$mode Delphi}

interface

uses
	SysUtils, Classes, //SyncObjs, LMessages,
	AudioTag, basetag, file_Wave, file_mp3, file_ogg;

type
	TTagScannerJob = class(TObject)
	private
		FThread:       TThread;
		FFileList,
		FTagsList:     TStringList;
		Terminated,
		Queued:        Boolean;

		FCurrentFilename,
		FDirectory,
		FExtensions: String;
		FOnFileAdd,
		FOnFileRead,
		FOnDone:     TNotifyEvent;

		procedure DoExecute;
		procedure InternalFileProcessed(const Filename: String);
		procedure InternalFileAdded(const Filename: String);
		procedure FileProcessed;
		procedure FileAdded;
	public
		procedure Execute(UseThreads: Boolean = True);
		procedure Terminate;

		property Directory:  String read FDirectory  write FDirectory;
		property Extensions: String read FExtensions write FExtensions;
		property CurrentFilename: String read FCurrentFilename;
		property CurrentTags: TStringList read FTagsList;

		property OnFileAdded: TNotifyEvent read FOnFileAdd  write FOnFileAdd;
		property OnTagsRead:  TNotifyEvent read FOnFileRead write FOnFileRead;
		property OnDone:      TNotifyEvent read FOnDone     write FOnDone;

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
	function ReadFileTags(const Filename: String): TStringList;


implementation

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
		repeat
			E := LowerCase(ExtractFileExt(Info.Name));
			If Pos(E, FExtensions) > 0 then
				InternalFileAdded(FDirectory + Info.Name);
		until (FindNext(Info) <> 0) or Terminated;
	finally
		FindClose(Info);
	end;

	for S in FFileList do
	begin
		if Terminated then Exit;
		FTagsList := ReadFileTags(S);
		InternalFileProcessed(S);
		FTagsList.Free;
	end;
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
	if Assigned(FOnFileAdd) then FOnFileAdd(Self);
	Queued := False;
end;

procedure TTagScannerJob.FileProcessed;
begin
	if Assigned(FOnFileRead) then FOnFileRead(Self);
	Queued := False;
end;

procedure TTagScannerJob.ThreadDone(Sender : TObject);
begin
	FDirectory := '';
	if Queued then
		TThread.RemoveQueuedEvents(FThread, FileProcessed);

	FThread := nil;
	FFileList.Free;

	if Assigned(FOnDone) then FOnDone(Self);
end;

procedure TTagScannerJob.Terminate;
begin
	Terminated := True;
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

function ReadTags(const Filename: String): TTagReader;
var
	TagClass: TTagReaderClass;
begin
	if not FileExists(Filename) then Exit(nil);
	TagClass := IdentifyKind(Filename);
	Result := TagClass.Create;
	try
		Result.LoadFromFile(Filename);
	except
	end;
end;

function ReadFileTags(const Filename: String): TStringList;
var
	TagReader: TTagReader;
	Tags: TCommonTags;
begin
	Result := TStringList.Create;
	TagReader := ReadTags(Filename);

	if Assigned(TagReader) then
	begin
		Tags := TagReader.GetCommonTags;

		if Tags.Duration > 0 then
			Result.Add(
				FormatDateTime('nn:ss', Tags.Duration / MSecsPerDay).Replace('.',':'))
		else
			Result.Add('');

		if TagReader.MediaProperty.BitRate > 0 then
			Result.Add('%d', [TagReader.MediaProperty.Bitrate])
		else
			Result.Add('');
		Result.Add(Tags.Artist);
		Result.Add(Tags.Title);

		TagReader.Free;
	end;
end;

end.

