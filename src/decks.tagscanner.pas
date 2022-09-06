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
		FFileList:     TStringList;
		FTagsList:     TCommonTags;
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
		property CurrentFilename: String   read FCurrentFilename;
		property CurrentTags: TCommonTags  read FTagsList;

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
	function ReadFileTags(const Filename: String): TCommonTags;
	function TagsToStringList(const Tags: TCommonTags): TStringList;


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
	try
		TagClass := IdentifyKind(Filename);
	except
		Exit(nil);
	end;
	Result := TagClass.Create;
	try
		Result.LoadFromFile(Filename);
	except
	end;
end;

function ReadFileTags(const Filename: String): TCommonTags;
var
	TagReader: TTagReader;
begin
	TagReader := ReadTags(Filename);
	try
		if Assigned(TagReader) then
		begin
			Result.Index := 0;
			Result := TagReader.GetCommonTags;
			Result.Index := TagReader.MediaProperty.Bitrate;
			if (TagReader is TMP3Reader) then
				if TMP3Reader(TagReader).isVBR then
					Result.Index := -Result.Index;
		end
		else
			Result := Default(TCommonTags);
	finally
		TagReader.Free;
	end;
end;

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

function TagsToStringList(const Tags: TCommonTags): TStringList;
var
	S: String;
begin
	Result := TStringList.Create;

	if Tags.Duration > 0 then
		Result.Add(
			FormatDateTime('nn:ss', Tags.Duration / MSecsPerDay).Replace('.',':'))
	else
		Result.Add('');

	if Tags.Index <> 0 then // bitrate
	begin
		S := Abs(Tags.Index).ToString;
		if Abs(Tags.Index) < 100 then
			S := ' ' + S;
		if Tags.Index < 0 then
			S := S + ' *';
		Result.Add(S);
	end
	else
		Result.Add('');

	if Length(Tags.Year) < 4 then
		Result.Add('')
	else
		Result.Add(Copy(Tags.Year, 1, 4));

	Result.Add(Tags.Genre);

	Result.Add(Fix(Tags.Artist));
	Result.Add(Fix(Tags.Title));
	Result.Add(Fix(Tags.Comment));
end;


end.

