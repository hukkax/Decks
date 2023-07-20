unit Decks.Config;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Graphics;

const
	AppName = 'Decks 3';
	Version = '0.6b';
	ReleaseType = {$IFDEF DEBUG} ' [DEBUG]' {$ELSE} '' {$ENDIF};
	ReleaseDate = {$I %DATE%};
	AppVersionString = Appname + ' v' + Version + ' by hukka (' + ReleaseDate + ')' + ReleaseType;

	WHEEL_FILELIST = 1;
	WHEEL_PB = 10;
	WHEEL_PBZONES = 11;

	COLUMN_FILENAME  = 0;
	COLUMN_BPM       = 1;
	COLUMN_DURATION  = 2;
	COLUMN_BITRATE   = 3;
	COLUMN_YEAR      = 4;
	COLUMN_GENRE     = 5;
	COLUMN_ARTIST    = 6;
	COLUMN_TITLE     = 7;
	COLUMN_COMMENT   = 8;
	COLUMN_FILESIZE  = 9;
	COLUMN_FILEDATE  = 10;
	COLUMN_LAST = COLUMN_FILEDATE;

type
	TDecksColor = TColor;
	//TDecksColorPair = array[Boolean] of TDecksColor;

	TDecksVisualTheme = record

		Strings: record
			FileList: record
				Directory,
				ParentDirectory,
				Drives: String;
			end;
		end;

		Colors: record
			FileList: record
				GridLines,
				BgHover,
				FgSelection,
				BgSelection,
				FgHeader,
				BgHeader: TDecksColor;

				Background: record
					Normal,
					Focused: TDecksColor;
				end;
				FileItem: record
					FgDefault,
					FgHasBPM,
					FgPlayed: TDecksColor;
				end;
				DirectoryItem: record
					FgParent,
					BgParent,
					FgDirectory,
					BgDirectory,
					FgDrive,
					BgDrive: TDecksColor;
				end;
			end;
		end;

	end;

	TDecksConfig = record
		Filename,
		AppPath,
		Path,
		PluginPath,
		ThemePath,
		ThemeName:	String;

		Window: record
			DirList: record
				Enabled: Boolean;
			end;
			FileList: record
				ColumnVisible: array[0..COLUMN_LAST] of Boolean;
			end;
			Tracklist: record
				Visible: Boolean;
			end;
			DeckPanelHeight: Word;
			Zoom: Word;
			ShowTitlebar: Boolean;
		end;

		Directory: record
			BPM,
			Audio: String;
			AutoUpdate: Boolean;
		end;

		Audio: record
			Hz: Word;
			Buffer: Integer;
			UpdatePeriod: Word;
			Threads: Byte;
			Device: array[1..4] of Byte;
			SubDevice: array[1..4] of Byte;
		end;

		Controller: record
			Config: String;
		end;

		Effects: record
			Enabled: Boolean;
		end;

		Mixer: record
			Enabled,
			Enable_Crossfader,
			Enable_Equalizer: Boolean;
			EQ: record
				Low, Mid, High: Integer;
			end;
		end;

		Deck: record
			Bend: record
				Normal,
				JogWheel,
				Max: Integer;
			end;
			BeatGraph: record
				ShowHorizontalLines: Boolean;
			end;
			Waveform: record
				ShowDual: Boolean;
			end;
			FirstSetsMasterBPM: Boolean;
		end;

		Theme: TDecksVisualTheme;

		function  GetConfigFilePath: String;
		function  GetAppPath: String;
		function  GetPath: String;
		function  GetThemePath: String;
		function  GetPluginPath: String;

		function  Load: Boolean;
		procedure Save;
	end;

	{function ColorToString(C: TColor): String;
	function StringToColor(const S: String): TColor;}

var
	Config: TDecksConfig;

implementation

uses
	IniFiles;

{function ColorToString(C: TColor): String;
var
	N: LongInt;
begin
	N := ColorToRGB(C);
	Result := '$' + Format('%.2x%.2x%.2x', [ Red(N), Green(N), Blue(N) ]);
end;

function StringToColor(const S: String): TColor;
var
	C: Cardinal;
begin
	if S.IsEmpty then Exit(clNone);
	C := Abs(S.ToInteger);
	Result := RGBToColor(C shr 16 and $FF, C shr 8 and $FF, C and $FF);
end;}

{ TDecksConfig }

function TDecksConfig.GetAppPath: String;
begin
	AppPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
	Result := AppPath;
end;

function TDecksConfig.GetPath: String;
begin
	if AppPath = '' then GetAppPath;
	Path := IncludeTrailingPathDelimiter(AppPath + 'data');
	Result := Path;
end;

function TDecksConfig.GetThemePath: String;
begin
	if Path = '' then GetPath;
	if ThemeName = '' then ThemeName := 'default';
	ThemePath := IncludeTrailingPathDelimiter(Path + 'themes' + PathDelim + ThemeName);
	Result := ThemePath;
end;

function TDecksConfig.GetPluginPath: String;
begin
	if AppPath = '' then GetAppPath;
	PluginPath := IncludeTrailingPathDelimiter(AppPath + 'plugins');
	Result := PluginPath;
end;

function TDecksConfig.GetConfigFilePath: String;
begin
	Result := GetPath + Filename;
end;

function ReadPath(const S: String): String;
begin
	Result := S.Replace('/', PathDelim, [rfReplaceAll]);
	Result := IncludeTrailingPathDelimiter(Result);
end;

function WritePath(const S: String): String;
begin
	Result := IncludeTrailingPathDelimiter(S);
	Result := Result.Replace('\', '/', [rfReplaceAll]);
end;

function TDecksConfig.Load: Boolean;
var
	Ini: TIniFile;
	Sect: String;
	i: Integer;

	procedure ReadColor(Color: PColor; const Item: String);
	var
		S: String;
	begin
		S := Ini.ReadString(Sect, Item, '');
		if (not S.IsEmpty) then
			Color^ := StringToColor(S);
	end;

begin
	Sect := GetConfigFilePath;
	ForceDirectories(ExtractFilePath(Sect));
	//if not FileExists(Sect) then Exit(False);

	Ini := TIniFile.Create(Sect);

	Sect := 'directory';
	Directory.BPM   := ReadPath(Ini.ReadString(Sect, 'bpm',   Directory.BPM));
	Directory.Audio := ReadPath(Ini.ReadString(Sect, 'audio', Directory.Audio));
	Directory.AutoUpdate := Ini.ReadBool(Sect, 'autoupdate', True);

	Sect := 'window';
	Window.DirList.Enabled := Ini.ReadBool(Sect, 'dirlist.enabled', True);
	for i := 0 to High(Window.FileList.ColumnVisible) do
		Window.FileList.ColumnVisible[i] :=
			Ini.ReadBool(Sect, Format('filelist.columns.%d.visible', [i]), True);
	Window.Tracklist.Visible := Ini.ReadBool(Sect, 'tracklist.visible', False);
	Window.DeckPanelHeight := Ini.ReadInteger(Sect, 'deckheight', 0);
	Window.Zoom := Ini.ReadInteger(Sect, 'zoom', 100);
	Window.ShowTitlebar := Ini.ReadBool(Sect, 'titlebar.enabled', True);

	Sect := 'audio';
	Audio.Hz           := Ini.ReadInteger(Sect, 'hz', 44100);
	Audio.Buffer       := Ini.ReadInteger(Sect, 'buffer', 20);
	Audio.UpdatePeriod := Ini.ReadInteger(Sect, 'updateperiod', 60);
	Audio.Threads      := Ini.ReadInteger(Sect, 'threads', 1);
	for i := 1 to High(Audio.Device) do
		Audio.Device[i] := Ini.ReadInteger(Sect, 'device.' + IntToStr(i), 0);
	for i := 1 to High(Audio.SubDevice) do
		Audio.SubDevice[i] := Ini.ReadInteger(Sect, 'subdevice.' + IntToStr(i), 0);

	Sect := 'controller';
	Controller.Config := Ini.ReadString(Sect, 'config', '');

	Sect := 'mixer';
	Mixer.Enabled := Ini.ReadBool(Sect, 'enabled', True);
	Mixer.Enable_Crossfader := Ini.ReadBool(Sect, 'crossfader', True);
	Mixer.Enable_Equalizer  := Ini.ReadBool(Sect, 'eq',         True);
	Mixer.EQ.Low := Ini.ReadInteger(Sect, 'low',  125);
	Mixer.EQ.Mid := Ini.ReadInteger(Sect, 'mid',  1000);
	Mixer.EQ.High:= Ini.ReadInteger(Sect, 'high', 8000);

	Sect := 'effects';
	Effects.Enabled := Ini.ReadBool(Sect, 'enabled', False);

	Sect := 'deck';
	Deck.Bend.Normal   := Ini.ReadInteger(Sect, 'bend.normal',   800);
	Deck.Bend.JogWheel := Ini.ReadInteger(Sect, 'bend.jogwheel', 100);
	Deck.Bend.Max      := Ini.ReadInteger(Sect, 'bend.max',      4000);

	Deck.BeatGraph.ShowHorizontalLines := Ini.ReadBool(Sect, 'graph.horizontallines', False);
	Deck.Waveform.ShowDual := Ini.ReadBool(Sect, 'wave.showdual', False);

	Deck.FirstSetsMasterBPM := Ini.ReadBool(Sect, 'setmasterbpm', True);

	Sect := 'theme.strings';
	with Theme.Strings.FileList do
	begin
		Directory := Ini.ReadString(Sect, 'filelist.directory', Directory);
		ParentDirectory := Ini.ReadString(Sect, 'filelist.parent', ParentDirectory);
		Drives := Ini.ReadString(Sect, 'filelist.drives', Drives);
	end;

	Sect := 'theme.colors';
	with Theme.Colors.FileList do
	begin
		ReadColor(@GridLines, 'list.gridlines');
		ReadColor(@FgHeader,  'list.fg.header');
		ReadColor(@BgHeader,  'list.bg.header');
		ReadColor(@BgHover,   'list.hover');
		ReadColor(@FgSelection, 'list.fg.selected');
		ReadColor(@BgSelection, 'list.bg.selected');

		ReadColor(@Background.Normal,  'filelist.background');
		ReadColor(@Background.Focused, 'filelist.background.focused');

		ReadColor(@FileItem.FgDefault, 'filelist.fg');
		ReadColor(@FileItem.FgHasBPM,  'filelist.fg.hasbpm');
		ReadColor(@FileItem.FgPlayed,  'filelist.fg.played');

		ReadColor(@DirectoryItem.FgParent, 'filelist.fg.parent');
		ReadColor(@DirectoryItem.BgParent, 'filelist.bg.parent');
		ReadColor(@DirectoryItem.FgDirectory, 'filelist.fg.dir');
		ReadColor(@DirectoryItem.BgDirectory, 'filelist.bg.dir');
		ReadColor(@DirectoryItem.FgDrive, 'filelist.fg.drive');
		ReadColor(@DirectoryItem.BgDrive, 'filelist.bg.drive');
	end;

	Ini.Free;
	Result := True;
end;

procedure TDecksConfig.Save;
var
	Ini: TIniFile;
	Sect: String;
	i: Integer;
begin
	Ini := TIniFile.Create(GetConfigFilePath);

	Sect := 'directory';
//	Ini.WriteString(Sect, 'bpm',   WritePath(Directory.BPM));
//	Ini.WriteString(Sect, 'audio', WritePath(Directory.Audio));
	Ini.WriteBool(Sect, 'autoupdate', Directory.AutoUpdate);

	Sect := 'window';
	Ini.WriteInteger(Sect, 'zoom', Window.Zoom);
	Ini.WriteInteger(Sect, 'deckheight', Window.DeckPanelHeight);
	Ini.WriteBool(Sect, 'titlebar.enabled', Window.ShowTitlebar);
	Ini.WriteBool(Sect, 'dirlist.enabled', Window.DirList.Enabled);
	for i := 0 to High(Window.FileList.ColumnVisible) do
		Ini.WriteBool(Sect, Format('filelist.columns.%d.visible', [i]),
			Window.FileList.ColumnVisible[i]);
	Ini.WriteBool(Sect, 'tracklist.visible', Window.Tracklist.Visible);

	Sect := 'audio';
	Ini.WriteInteger(Sect, 'hz',           Audio.Hz);
	Ini.WriteInteger(Sect, 'buffer',       Audio.Buffer);
	Ini.WriteInteger(Sect, 'updateperiod', Audio.UpdatePeriod);
	Ini.WriteInteger(Sect, 'threads',      Audio.Threads);
	for i := 1 to High(Audio.Device) do
		Ini.WriteInteger(Sect, 'device.' + IntToStr(i), Audio.Device[i]);
	for i := 1 to High(Audio.SubDevice) do
		Ini.WriteInteger(Sect, 'subdevice.' + IntToStr(i), Audio.SubDevice[i]);

	Sect := 'mixer';
	Ini.WriteBool(Sect, 'enabled', Mixer.Enabled);

	Sect := 'effects';
	Ini.WriteBool(Sect, 'enabled', Effects.Enabled);

	Sect := 'deck';
	Ini.WriteBool(Sect, 'graph.horizontallines', Deck.BeatGraph.ShowHorizontalLines);
	Ini.WriteBool(Sect, 'wave.showdual', Deck.Waveform.ShowDual);
	Ini.WriteBool(Sect, 'setmasterbpm', Deck.FirstSetsMasterBPM);

	Sect := 'theme.strings';
	with Theme.Strings.FileList do
	begin
		Ini.WriteString(Sect, 'filelist.directory', Directory);
		Ini.WriteString(Sect, 'filelist.parent', ParentDirectory);
		Ini.WriteString(Sect, 'filelist.drives', Drives);
	end;
{
	Sect := 'theme.colors';
	with Theme.Colors.FileList do
	begin
		Ini.WriteString(Sect, 'filelist.background',
			ColorToString(Background.Normal));
		Ini.WriteString(Sect, 'filelist.background.focused',
			ColorToString(Background.Focused));

		Ini.WriteString(Sect, 'filelist.fg',
			ColorToString(FileItem.FgDefault));
		Ini.WriteString(Sect, 'filelist.fg.hasbpm',
			ColorToString(FileItem.FgHasBPM));
		Ini.WriteString(Sect, 'filelist.fg.played',
			ColorToString(FileItem.FgPlayed));

		Ini.WriteString(Sect, 'filelist.fg.parent',
			ColorToString(DirectoryItem.FgParent));
		Ini.WriteString(Sect, 'filelist.bg.parent',
			ColorToString(DirectoryItem.BgParent));
		Ini.WriteString(Sect, 'filelist.fg.dir',
			ColorToString(DirectoryItem.FgDirectory));
		Ini.WriteString(Sect, 'filelist.bg.dir',
			ColorToString(DirectoryItem.BgDirectory));
		Ini.WriteString(Sect, 'filelist.fg.drive',
			ColorToString(DirectoryItem.FgDrive));
		Ini.WriteString(Sect, 'filelist.bg.drive',
			ColorToString(DirectoryItem.BgDrive));
	end;
}
	Ini.Free;
end;

initialization

	Config.Filename := 'decks.ini';

	Config.GetAppPath;
	Config.GetPluginPath;
	Config.GetThemePath;

	Config.Directory.Audio := Config.AppPath;
	Config.Directory.BPM   := IncludeTrailingPathDelimiter(Config.GetPath + 'bpm');

end.

