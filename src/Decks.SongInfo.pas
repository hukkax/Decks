unit Decks.SongInfo;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils, IniFiles;

type
	TInfoKeyword = ( IKW_BPM, IKW_CUE, IKW_ZONE, IKW_ZONEDATA, IKW_ZONE_OLD, IKW_BPM_OLD );
	TInfoParams  = array of Int64;

	TInfoKeywordHandler  = procedure(Keyword: TInfoKeyword; Params: TInfoParams) of object;

	TInfoFileAccessEvent = procedure(const Filename: String; Ini: TIniFile) of object;

	TSongInfo = record
		Initialized:   Boolean;  // loaded
		OldVersion:    Boolean;  // true=Decks 2, false=Decks 3
		BPM:           Single;
		Amp:           Single;
		NormalizedAmp: Single;
		LUFS:          Single;
		Length:        Single;   // song length in seconds
		Bitrate:       Word;     // song bitrate in kbps
		FileSize:      QWord;
		FileDate:      TDateTime;
		//StartPos:    QWord;    // graph start offset in songdata bytes
	end;

	TSongTags = record
	private
		function GetDuration: String;
	public
		Artist,
		Title,
		Genre,
		Comment:  String;
		Year:     Word;
		HasImage: Boolean;
		Info:     TSongInfo;
		property Duration: String read GetDuration;
	end;
	PSongTags = ^TSongTags;

	// todo move elsewhere
	function  Split(const S: String; out L, R: String): Boolean;
	function  SplitInt(const S: String; out L, R: Int64): Boolean;

	function FileDate(const FileName: String): TDateTime;
	function FileSize(const FileName: String): QWord;

	procedure SetSongInfoPath(const Path: String);
	function  GetSongInfoNew(const BPMFilename: String;
		OnLoadInfo: TInfoFileAccessEvent;
		KeywordHandler: TInfoKeywordHandler = nil): TSongInfo;
	function  GetSongInfo(const Filename: String;
		OnLoadInfo: TInfoFileAccessEvent;
		KeywordHandler: TInfoKeywordHandler = nil): TSongInfo;
	function  GetBPMFile(const AudioFileName: String): String; inline;


implementation

uses
	Decks.Beatgraph,
	Decks.Config;


function Split(const S: String; out L, R: String): Boolean;
var
	X: Integer;
begin
	X := Pos(' ', S);
	Result := (X > 1);
	if Result then
	begin
		L := Trim(Copy(S, 1, X));
		R := Trim(Copy(S, X+1, Length(S)));
	end
	else
	begin
		L := Trim(S);
		R := '';
	end;
end;

function SplitInt(const S: String; out L, R: Int64): Boolean;
var
	SL, SR: String;
begin
	Result := Split(S, SL, SR);
	if Result then
	begin
		L := StrToInt64(SL);
		R := StrToInt64(SR);
	end;
end;

function FileDate(const FileName: String): TDateTime;
begin
	try
		SysUtils.FileAge(FileName, Result, False);
	except
		Result := 0;
	end;
end;

function FileSize(const FileName: String): QWord;
var
	F: file of Byte;
begin
	Result := 0;
	if not FileExists(FileName) then Exit;
	try
		{$I-}
		AssignFile(F, FileName);
		Reset(F);
		{$I+}
		if IOResult = 0 then
			Result := System.FileSize(F)
		else
			Result := 0;
	finally
		{$I-}CloseFile(F);{$I+}
	end;
end;

procedure SetSongInfoPath(const Path: String);
begin
	Config.Directory.BPM := IncludeTrailingPathDelimiter(Path);
end;

(*procedure TSongInfo.LoadFromFile(const Filename: String);
begin
end;*)

{
procedure TSongInfo.SaveToFile(const Filename: String);
var
	S: String;
	i: Integer;
	Sl: TStringList;
begin
(*
Decks 2.0a
FILENAME X:\mix\house\475_ Bob Sinclair - Gymtonic.mp3
BPM 133 716
AMP 1 0
CUE 0 66486
ZONE 133 657 6082036
*)
	S := InfoFilePath + Filename + '.bpm';
	if not FileExists(S) then Exit;
end;
}

function GetBPMFile(const AudioFileName: String): String;
begin
	Result := Config.Directory.BPM + AudioFilename + '.bpm';
end;

function GetSongInfoNew(const BPMFilename: String;
	OnLoadInfo: TInfoFileAccessEvent;
	KeywordHandler: TInfoKeywordHandler = nil): TSongInfo;
var
	P, Sect: String;
	i: Integer;
	iL, iR: Int64;
	Ini: TIniFile;
	zk: TZoneKind;

	function GetValue(const S: String): String;
	begin
		Result := Ini.ReadString(Sect, S, '').Replace('.', ' ');
	end;

begin
	Ini := TIniFile.Create(BPMFilename);
	try
		Sect := 'song';

		P := GetValue('bpm');
		if P <> '' then
		begin
			SplitInt(P, iL, iR);
			if iL >= 60 then
			begin
				Result.BPM := iL + (iR / 1000);
				if Assigned(KeywordHandler) then
					KeywordHandler(IKW_BPM, TInfoParams.Create(iL, iR));
			end;
		end;

		P := GetValue('gain');
		if P <> '' then
		begin
			SplitInt(P, iL, iR);
			Result.Amp := iL + (iR / 1000);
		end
		else
			Result.Amp := 1.0;

		P := GetValue('lufs');
		if P <> '' then
		begin
			SplitInt(P, iL, iR);
			Result.LUFS := iL + (iR / 1000);
		end
		else
			Result.LUFS := 0.0;

		Result.Bitrate := Ini.ReadInteger(Sect, 'bitrate', 0);

		P := GetValue('duration');
		if P <> '' then
		begin
			SplitInt(P, iL, iR);
			Result.Length := iL + (iR / 1000);
		end;

		if Assigned(KeywordHandler) then
		begin
			sect := 'cue';
			for i := 0 to 9 do
			begin
				iR := Ini.ReadInt64(sect, IntToStr(i), -1);
				if iR >= 0 then
					KeywordHandler(IKW_CUE, TInfoParams.Create(i, iR));
			end;
		end;

		i := 0;
		while Assigned(KeywordHandler) do
		begin
			sect := Format('zone.%d', [i]);
			if not Ini.SectionExists(sect) then Break;

			P := GetValue('bpm');
			if P = '' then Break;

			SplitInt(P, iL, iR);
			KeywordHandler(IKW_ZONE, TInfoParams.Create(
				iL, iR, Ini.ReadInt64(sect, 'bar', 0)));

			P := Ini.ReadString(sect, 'kind', '');
			for zk in TZoneKind do
				if ZoneKindNames[zk] = P then
				begin
					KeywordHandler(IKW_ZONEDATA, TInfoParams.Create(
						Ord(zk), Ini.ReadInt64(sect, 'data', 0)));
					Break;
				end;

			Inc(i);
		end;

		if Assigned(OnLoadInfo) then
			OnLoadInfo(BPMFilename, Ini);

	finally
		Ini.Free;
		Result.Initialized := True;
	end;
end;


function GetSongInfo(const Filename: String;
	OnLoadInfo: TInfoFileAccessEvent;
	KeywordHandler: TInfoKeywordHandler = nil): TSongInfo;
var
	Ver, K, P, Fn: String;
	i: Integer;
	iL, iR: Int64;
	KW: TInfoKeyword;
	Sl: TStringList;
	sarr: TStringArray;

	function GetLine(const S: String): String;
	var
		L: String;
	begin
		for L in Sl do
		begin
			if L.StartsWith(S) then
			begin
				Sl.Delete(Sl.IndexOf(L));
				Exit(L);
			end;
		end;
		Result := '';
	end;

	function GetValue(const S: String): String;
	begin
		Result := GetLine(S);
		if Result <> '' then
		begin
			if Split(Result, K, P) then
				Result := P;
		end;
	end;

begin
	Result := Default(TSongInfo);
	Result.Initialized := False;
	Result.OldVersion := False;
	Result.Amp := 1.0;

	Fn := GetBPMFile(Filename);
	if not FileExists(Fn) then Exit;

	Sl := TStringList.Create;
	try
		try
			Sl.LoadFromFile(Fn);

			if (Sl.Count > 0) and (Sl[0] = '[Decks 3]') then
			begin
				Result := GetSongInfoNew(Fn, OnLoadInfo, KeywordHandler);
				Exit;
			end;

			Ver := GetValue('Decks');
			Result.OldVersion := Ver.StartsWith('2.', True);
			if Result.OldVersion then
				KW := IKW_ZONE_OLD
			else
				KW := IKW_ZONE;

			if Assigned(KeywordHandler) then
			begin
				for i := 0 to 9 do
				begin
					P := GetValue('CUE ' + IntToStr(i));
					if P <> '' then
					begin
						SplitInt(P, iL, iR);
						KeywordHandler(IKW_CUE, TInfoParams.Create(iL, iR))
					end;
				end;
			end;

			if (not Result.OldVersion) and (Ver <> '3.0a') then
			begin
				P := GetValue('AMP');
				if P <> '' then
				begin
					SplitInt(P, iL, iR);
					Result.Amp := iL + (iR / 1000);
				end;
			end;

			P := GetValue('BPM');
			if P <> '' then
			begin
				SplitInt(P, iL, iR);
				Result.BPM := iL + (iR / 1000);
				if Assigned(KeywordHandler) then
					KeywordHandler(IKW_BPM_OLD, TInfoParams.Create(iL, iR));
			end;

			while Assigned(KeywordHandler) do
			begin
				if Result.OldVersion then
					P := GetValue('ZONE')
				else
					P := GetValue('!ZONE');
				if P = '' then Break;
				sarr := P.Split(' ');
				if High(sarr) >= 2 then
					KeywordHandler(KW, TInfoParams.Create(
						StrToInt(sarr[0]), StrToInt(sarr[1]), StrToInt(sarr[2])));
			end;

		finally
			Sl.Free;
			Result.Initialized := True;
		end;
	except
	end;
end;

{ TSongTags }

function TSongTags.GetDuration: String;
begin
	if Info.Length > 0 then
		Result := FormatDateTime('nn:ss', Info.Length / SecsPerDay).Replace('.',':')
	else
		Result := '';
end;

end.

