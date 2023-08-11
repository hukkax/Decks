unit Decks.Config;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Graphics, Generics.Collections;

const
	AppName = 'CaniMix';
	Version = '0.7a';
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
	TConfigItemType = ( cfgBoolean, cfgByte, cfgWord, cfgInteger, cfgFloat, cfgString );
	TConfigItemAccess = ( cfgRead, cfgWrite, cfgReadWrite );

	TConfigItem = class
	public
		Kind:   TConfigItemType;
		Access: TConfigItemAccess;
		Data:   Pointer;
		Section,
		Name:   String;
	end;

	TConfigManager = class
	public
		Items: TObjectList<TConfigItem>;

		procedure Clear;
		procedure Add(aKind: TConfigItemType; const aSection, aName: String; aData: Pointer;
		          Access: TConfigItemAccess = cfgReadWrite);

		procedure Load(const Filename: String);
		procedure Save(const Filename: String);

		constructor Create;
		destructor  Destroy; override;
	end;

	// ------------------------------------------------------------------------

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
		Manager: TConfigManager;
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
			TargetLUFS: Integer;
			GraphLUFS: Integer;
			UpdatePeriod: Word;
			Threads: Byte;
			Device:    array[1..4] of Byte;
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
				Height:   Byte;
			end;
			FirstSetsMasterBPM: Boolean;
			WarnTime:  Word;
			WarnSpeed: Word;
		end;

		Theme: TDecksVisualTheme;

		function  GetConfigFilePath: String;
		function  GetAppPath: String;
		function  GetPath: String;
		function  GetThemePath: String;
		function  GetPluginPath: String;

		procedure Setup;
		function  Load: Boolean;
		procedure Save;
 end;

	{function ColorToString(C: TColor): String;
	function StringToColor(const S: String): TColor;}

var
	Config: TDecksConfig;
	SupportedFormats: String;

implementation

uses
dialogs,
	IniFiles;

{ TConfigManager }

constructor TConfigManager.Create;
begin
	inherited Create;
	Items := TObjectList<TConfigItem>.Create(True);
end;

destructor TConfigManager.Destroy;
begin
	Items.Free;
	inherited Destroy;
end;

procedure TConfigManager.Clear;
begin
	Items.Clear;
end;

procedure TConfigManager.Add(aKind: TConfigItemType; const aSection, aName: String; aData: Pointer;
	Access: TConfigItemAccess = cfgReadWrite);
var
	Item: TConfigItem;
begin
	Item := TConfigItem.Create;
	Item.Kind    := aKind;
	Item.Access  := Access;
	Item.Data    := aData;
	Item.Section := aSection;
	Item.Name    := aName;
	Items.Add(Item);
end;

procedure TConfigManager.Load(const Filename: String);
var
	Ini: TIniFile;
	Item: TConfigItem;
	S: String;
	PS: PString;
begin
	Ini := TIniFile.Create(Filename);
	try
		for Item in Items do
		begin
			if Item.Access in [cfgRead, cfgReadWrite] then
			case Item.Kind of
				cfgBoolean:		PBoolean(Item.Data)^ := Ini.ReadBool   (Item.Section, Item.Name, Boolean(Item.Data^));
				cfgByte:		PByte(Item.Data)^    := Byte(Ini.ReadInteger(Item.Section, Item.Name, Byte(Item.Data^)));
				cfgWord:		PWord(Item.Data)^    := Word(Ini.ReadInteger(Item.Section, Item.Name, Word(Item.Data^)));
				cfgInteger:		PInteger(Item.Data)^ := Ini.ReadInteger(Item.Section, Item.Name, Integer(Item.Data^));
				cfgFloat:		PDouble(Item.Data)^  := Ini.ReadFloat  (Item.Section, Item.Name, Double(Item.Data^));
				cfgString:
				begin
					PS := Item.Data;
					S := Ini.ReadString(Item.Section, Item.Name, PS^);
					PS^ := S;
				end;
			end;
		end;
	finally
		Ini.Free;
	end;
end;

procedure TConfigManager.Save(const Filename: String);
var
	Ini: TIniFile;
	Item: TConfigItem;
begin
	Ini := TIniFile.Create(Filename);
	try
		for Item in Items do
		begin
			if Item.Access in [cfgWrite, cfgReadWrite] then
			case Item.Kind of
				cfgBoolean:		Ini.WriteBool   (Item.Section, Item.Name, Boolean(Item.Data^));
				cfgByte:		Ini.WriteInteger(Item.Section, Item.Name, Byte(Item.Data^));
				cfgWord:		Ini.WriteInteger(Item.Section, Item.Name, Word(Item.Data^));
				cfgInteger:		Ini.WriteInteger(Item.Section, Item.Name, Integer(Item.Data^));
				cfgFloat:		Ini.WriteFloat  (Item.Section, Item.Name, Double(Item.Data^));
				cfgString:		Ini.WriteString (Item.Section, Item.Name, String(Item.Data^));
			end;
		end;
	finally
		Ini.Free;
	end;
end;



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
	PluginPath := IncludeTrailingPathDelimiter(GetPath + 'plugins');
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
	Fn: String;

	{procedure ReadColor(Color: PColor; const Item: String);
	var
		S: String;
	begin
		S := Ini.ReadString(Sect, Item, '');
		if (not S.IsEmpty) then
			Color^ := StringToColor(S);
	end;}

begin
	Fn := GetConfigFilePath;
	ForceDirectories(ExtractFilePath(Fn));
	//if not FileExists(Sect) then Exit(False);

	Manager.Load(Fn);
	Directory.BPM   := ReadPath(Directory.BPM);
	Directory.Audio := ReadPath(Directory.Audio);

{	Sect := 'theme.colors';
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
}
	Result := True;
end;

procedure TDecksConfig.Save;
begin
	Manager.Save(GetConfigFilePath);
end;

procedure TDecksConfig.Setup;
var
	Cfg: TConfigManager;
	Sect: String;
	i: Integer;
begin
	Cfg := Config.Manager;
	Cfg.Clear;

	Sect := 'directory';
	Cfg.Add(cfgString, Sect, 'bpm',   @Directory.BPM,   cfgRead);
	Cfg.Add(cfgString, Sect, 'audio', @Directory.Audio, cfgRead);
	Cfg.Add(cfgBoolean, Sect, 'autoupdate', @Directory.AutoUpdate);

	Sect := 'window';
	Cfg.Add(cfgWord,    Sect, 'zoom',             @Window.Zoom);
	Cfg.Add(cfgWord,    Sect, 'deckheight',       @Window.DeckPanelHeight);
	Cfg.Add(cfgBoolean, Sect, 'titlebar.enabled', @Window.ShowTitlebar);
	Cfg.Add(cfgBoolean, Sect, 'dirlist.enabled',  @Window.DirList.Enabled);
	for i := 0 to High(Window.FileList.ColumnVisible) do
		Cfg.Add(cfgBoolean, Sect, Format('filelist.columns.%d.visible', [i]),
			@Window.FileList.ColumnVisible[i]);
	Cfg.Add(cfgBoolean, Sect, 'tracklist.visible', @Window.Tracklist.Visible);

	Sect := 'audio';
	Cfg.Add(cfgWord,    Sect, 'hz',             @Audio.Hz);
	Cfg.Add(cfgInteger, Sect, 'buffer',         @Audio.Buffer);
	Cfg.Add(cfgWord,    Sect, 'updateperiod',   @Audio.UpdatePeriod);
	Cfg.Add(cfgByte,    Sect, 'threads',        @Audio.Threads);
	Cfg.Add(cfgInteger, Sect, 'normalize',      @Audio.TargetLUFS);
	Cfg.Add(cfgInteger, Sect, 'normalizegraph', @Audio.GraphLUFS);
	for i := 1 to High(Audio.Device) do
		Cfg.Add(cfgByte, Sect, 'device.' + IntToStr(i), @Audio.Device[i]);
	for i := 1 to High(Audio.SubDevice) do
		Cfg.Add(cfgByte, Sect, 'subdevice.' + IntToStr(i), @Audio.SubDevice[i]);

	Sect := 'mixer';
	Cfg.Add(cfgBoolean, Sect, 'enabled', @Mixer.Enabled);

	Sect := 'effects';
	Cfg.Add(cfgBoolean, Sect, 'enabled', @Effects.Enabled);

	Sect := 'deck';
	Cfg.Add(cfgBoolean, Sect, 'setmasterbpm',  @Deck.FirstSetsMasterBPM);

	Cfg.Add(cfgBoolean, Sect, 'graph.horizontallines', @Deck.BeatGraph.ShowHorizontalLines);

	Cfg.Add(cfgBoolean, Sect, 'wave.showdual', @Deck.Waveform.ShowDual);
	Cfg.Add(cfgByte,    Sect, 'wave.height',   @Deck.Waveform.Height);

	Cfg.Add(cfgWord,    Sect, 'warn.time',     @Deck.WarnTime);
	Cfg.Add(cfgWord,    Sect, 'warn.speed',    @Deck.WarnSpeed);

	Sect := 'controller';
	Cfg.Add(cfgString, Sect, 'config', @Controller.Config);

	Sect := 'theme.strings';
	with Theme.Strings.FileList do
	begin
		Cfg.Add(cfgString, Sect, 'filelist.directory', @Directory);
		Cfg.Add(cfgString, Sect, 'filelist.parent',    @ParentDirectory);
		Cfg.Add(cfgString, Sect, 'filelist.drives',    @Drives);
	end;
end;


initialization

	SupportedFormats := '.mp3 .ogg .wav' {$IFDEF MSWINDOWS} + ' .wma' {$ENDIF}; //' .it .s3m .xm .mod .sid .nsf';
	Config.Filename := AppName + '.ini';

	Config.Manager := TConfigManager.Create;

	Config.GetAppPath;
	Config.GetPluginPath;
	Config.GetThemePath;

	Config.Directory.Audio := Config.AppPath;
	Config.Directory.BPM   := IncludeTrailingPathDelimiter(Config.GetPath + 'bpm');

	Config.Setup;

finalization

	FreeAndNil(Config.Manager);

end.

