unit Decks.SongInfo;

{$mode Delphi}
{$INLINE ON}

interface

uses
	Classes, SysUtils;

type
	TInfoKeyword = ( IKW_BPM, IKW_CUE, IKW_ZONE, IKW_ZONE_OLD );
	TInfoParams  = array of Integer;

	TInfoKeywordHandler = procedure(Keyword: TInfoKeyword; Params: TInfoParams) of object;

	TSongInfo = record
		Initialized: Boolean;  // loaded
		OldVersion:  Boolean;  // true=Decks 2, false=Decks 3
		BPM:         Single;
		Amp:         Single;
		Length:      Single;   // song length in seconds
		//StartPos:    QWord;    // graph start offset in songdata bytes
	end;

	// todo move elsewhere
	function  Split(const S: String; out L, R: String): Boolean;
	function  SplitInt(const S: String; out L, R: Integer): Boolean;

	procedure SetSongInfoPath(const Path: String);
	function  GetSongInfo(const Filename: String; KeywordHandler: TInfoKeywordHandler = nil): TSongInfo;
	function  GetBPMFile(const AudioFileName: String): String; inline;


implementation

uses
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

function SplitInt(const S: String; out L, R: Integer): Boolean;
var
	SL, SR: String;
begin
	Result := Split(S, SL, SR);
	if Result then
	begin
		L := StrToInt(SL);
		R := StrToInt(SR);
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

function GetSongInfo(const Filename: String;
	KeywordHandler: TInfoKeywordHandler = nil): TSongInfo;
var
	Ver, K, P, Fn: String;
	i, iL, iR: Integer;
	KW: TInfoKeyword;
	Sl: TStringList;
	sarr: TStringArray;
	Params: TInfoParams;

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
	Result.Initialized := False;
	Result.OldVersion := False;
	Result.BPM := 0.0;
	Result.Amp := 1.0;
	//Result.StartPos := 0;

	Fn := GetBPMFile(Filename);
	if not FileExists(Fn) then Exit;

	Sl := TStringList.Create;
	try
		try
			Sl.LoadFromFile(Fn);

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
					KeywordHandler(IKW_BPM, TInfoParams.Create(iL, iR));
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

end.

