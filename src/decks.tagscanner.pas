unit Decks.TagScanner;

{$mode Delphi}

interface

uses
	SysUtils, Classes, //SyncObjs, LMessages,
	Decks.SongInfo,
	AudioTag, basetag, file_Wave, file_mp3, file_ogg, file_flac, file_wma;

type
	TTagScannerEventKind = (
		tsFileScanStarted, tsDirAdded, tsFileAdded, tsFileScanFinished,
		tsTagScanStarted,  tsTagsRead,  tsTagScanFinished );

	TTagScannerEvent = procedure(EventKind: TTagScannerEventKind; const Filename: String; Tags: PSongTags) of Object;

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
		FOnEvent: TTagScannerEvent;

		procedure DoExecute;
		procedure InternalFileProcessed(const Filename: String);
		procedure InternalFileAdded(const Filename: String);
		procedure FileProcessed;
		procedure FileAdded;
		procedure InternalDirAdded(const Filename: String);
		procedure DirAdded;
	public
		procedure Execute(UseThreads: Boolean = True);
		procedure Terminate;

		property Directory:  String read FDirectory  write FDirectory;
		property Extensions: String read FExtensions write FExtensions;
		property OnEvent: TTagScannerEvent read FOnEvent write FOnEvent;

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
	function ReadFileTags(const Filename: String; DoGetSongInfo: Boolean = True): TSongTags;


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
	if FindFirst(FDirectory + '*.*', faDirectory, Info) = 0 then
	try
		if Assigned(FOnEvent) then FOnEvent(tsFileScanStarted, FDirectory, nil);
		repeat
			if (Info.Attr and faDirectory) = faDirectory then
			begin
				if Info.Name <> '.' then
					InternalDirAdded(FDirectory + Info.Name);
			end
			else
			begin
				E := LowerCase(ExtractFileExt(Info.Name));
				If Pos(E, FExtensions) > 0 then
					InternalFileAdded(FDirectory + Info.Name);
			end;

		until (FindNext(Info) <> 0) or Terminated;
	finally
		FindClose(Info);
		if Assigned(FOnEvent) then FOnEvent(tsFileScanFinished, FDirectory, nil);
	end;

	// get tags for each file
	if Assigned(FOnEvent) then FOnEvent(tsTagScanStarted, '', nil);
	for S in FFileList do
	begin
		if Terminated then Break;
		FTagsList := ReadFileTags(S);
		InternalFileProcessed(S);
	end;
	if Assigned(FOnEvent) then FOnEvent(tsTagScanFinished, '', nil);
end;

procedure TTagScannerJob.InternalDirAdded(const Filename: String);
begin
	FCurrentFilename := Filename;
	TThread.Synchronize(FThread, DirAdded);
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

procedure TTagScannerJob.DirAdded;
begin
	if Assigned(FOnEvent) then FOnEvent(tsDirAdded, FCurrentFilename, nil);
	Queued := False;
end;

procedure TTagScannerJob.FileAdded;
begin
	if Assigned(FOnEvent) then FOnEvent(tsFileAdded, FCurrentFilename, nil);
	Queued := False;
end;

procedure TTagScannerJob.FileProcessed;
begin
	if Assigned(FOnEvent) then FOnEvent(tsTagsRead, ExtractFileName(FCurrentFilename), @FTagsList);
	Queued := False;
end;

procedure TTagScannerJob.ThreadDone(Sender: TObject);
begin
	FDirectory := '';
	if Queued then
		TThread.RemoveQueuedEvents(FThread, FileProcessed);

	FreeAndNil(FFileList);
	FThread := nil;
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
	FreeOnTerminate := True;
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

function ReadFileTags(const Filename: String; DoGetSongInfo: Boolean = True): TSongTags;
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
		Result.Info.Amp := 1.0;
		Result.Info.Initialized := True;
	finally
		TagReader.Free;
	end;

	if DoGetSongInfo then
	begin
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
end;


end.

