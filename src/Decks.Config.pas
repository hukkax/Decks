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

	PARAM_ANY       = $FFFF0000;
	VAL_BROWSE_PREV = $FFFF0001;
	VAL_BROWSE_NEXT = $FFFF0002;

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
	TAppMode = (
		MODE_APP_INIT,

		MODE_LOAD_START,
		MODE_LOAD_SUCCESS,
		MODE_LOAD_FAILURE,
		MODE_LOAD_GRAPH,
		MODE_LOAD_FINISH,

		MODE_PLAY_STOP,
		MODE_PLAY_START,
		MODE_PLAY_PAUSE,
		MODE_PLAY_FAILURE,
		MODE_PLAY_WAITSYNC,
		MODE_CUE,
		MODE_TEMPOCHANGE,
		MODE_SYNC,
		MODE_BEND,
		MODE_EQ_KILL,
		MODE_FX_ENABLE,
		MODE_HOTCUE,
		MODE_LOOP, MODE_LOOP_SONG, MODE_LOOP_ZONE,
		MODE_PADS_PAGE
	);

	TConfigItemType   = ( cfgBoolean, cfgByte, cfgWord, cfgInteger, cfgFloat, cfgString, cfgColor );
	TConfigItemAccess = ( cfgRead, cfgWrite, cfgReadWrite );

	TConfigItem = class
	public
		Kind:       TConfigItemType;
		Access:     TConfigItemAccess;
		Data:       Pointer;
		DefaultVal: Variant;
		Section,
		Name:       String;
	end;

	TConfigManager = class
	public
		Items: TObjectList<TConfigItem>;

		procedure Clear;
		procedure Add(aKind: TConfigItemType; const aSection, aName: String; aData: Pointer;
		          aDefaultVal: Variant; Access: TConfigItemAccess = cfgReadWrite);

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

			Base: record
				Background: TDecksColor;
			end;

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
			CueDevice: Byte;
			CueOnFront: Boolean;
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
			CueMode:   Byte;
			CueMix:    Byte;
			CueMaster: Boolean;
			CuePostFader: Boolean;
			CueUsesHardware: Boolean;
			MasterVolume: Byte;
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
			ShowRemainingTime: Boolean;
			WarnTime:  Word;
			WarnSpeed: Word;
			SpinDownSpeed: Word;
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
	aDefaultVal: Variant; Access: TConfigItemAccess = cfgReadWrite);
var
	Item: TConfigItem;
begin
	Item := TConfigItem.Create;
	Item.Kind    := aKind;
	Item.Access  := Access;
	Item.Data    := aData;
	Item.Section := aSection;
	Item.Name    := aName;
	Item.DefaultVal := aDefaultVal;
	Items.Add(Item);
end;

function SetupIni(const Filename: String): TIniFile;
begin
	Result := TIniFile.Create(Filename);
	Result.Options := Result.Options + [ifoWriteStringBoolean];
	Result.BoolTrueStrings  := ['true',  'yes', '1'];
	Result.BoolFalseStrings := ['false', 'no',  '0'];
end;

// adapted from https://wiki.freepascal.org/Convert_color_to/from_HTML

const
	HTML_COLOR_PREFIX = '$';

function ColorToHTML(Color: TColor): String;
var
  N: Longint;
begin
	if Color = clNone then Exit('');

	N := ColorToRGB(Color);
	Result := HTML_COLOR_PREFIX +
		IntToHex(Red(N), 2) + IntToHex(Green(N), 2) + IntToHex(Blue(N), 2);
end;

function HTMLToColor(S: String; Default: TColor = clNone): TColor;

	function IsCharWord(C: Char): Boolean; inline;
	begin
		Result := C in ['a'..'z', 'A'..'Z', '_', '0'..'9'];
	end;

	function IsCharHex(C: Char): Boolean; inline;
	begin
		Result := C in ['0'..'9', 'a'..'f', 'A'..'F'];
	end;

var
	i, N1, N2, N3, Len: Integer;
begin
	Result := Default;
	Len := 0;
	if S.StartsWith(HTML_COLOR_PREFIX) then
		Delete(S, 1, Length(HTML_COLOR_PREFIX));
	if S.IsEmpty then Exit;

	// delete after first nonword char
	i := 1;
	while (i <= Length(S)) and IsCharWord(S[i]) do Inc(i);
	Delete(S, i, Maxint);

	// allow only #rgb, #rrggbb
	Len := Length(S);
	if (Len <> 3) and (Len <> 6) then Exit;

	for i := 1 to Len do
		if not IsCharHex(S[i]) then Exit;

	if Len = 6 then
	begin
		N1 := StrToInt('$' + Copy(S, 1, 2));
		N2 := StrToInt('$' + Copy(S, 3, 2));
		N3 := StrToInt('$' + Copy(S, 5, 2));
	end
	else
	begin
		N1 := StrToInt('$' + S[1] + S[1]);
		N2 := StrToInt('$' + S[2] + S[2]);
		N3 := StrToInt('$' + S[3] + S[3]);
	end;

	Result:= RGBToColor(N1, N2, N3);
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

procedure TConfigManager.Load(const Filename: String);
var
	Ini: TIniFile;
	Item: TConfigItem;
	S: String;
	PS: PString;
begin
	Ini := SetupIni(Filename);
	try
		for Item in Items do
		begin
			if Item.Access in [cfgRead, cfgReadWrite] then
			case Item.Kind of
				cfgBoolean:		PBoolean(Item.Data)^ := Ini.ReadBool   (Item.Section, Item.Name, Item.DefaultVal);
				cfgByte:		PByte(Item.Data)^    := Byte(Ini.ReadInteger(Item.Section, Item.Name, Item.DefaultVal));
				cfgWord:		PWord(Item.Data)^    := Word(Ini.ReadInteger(Item.Section, Item.Name, Item.DefaultVal));
				cfgInteger:		PInteger(Item.Data)^ := Ini.ReadInteger(Item.Section, Item.Name, Item.DefaultVal);
				cfgFloat:		PDouble(Item.Data)^  := Ini.ReadFloat  (Item.Section, Item.Name, Item.DefaultVal);
				cfgString:
				begin
					PS := Item.Data;
					S := Ini.ReadString(Item.Section, Item.Name, PS^);
					PS^ := S;
				end;
				cfgColor:
				begin
					S := Ini.ReadString(Item.Section, Item.Name, '');
					if not S.IsEmpty then
						PColor(Item.Data)^ := HTMLToColor(S);
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
	Ini := SetupIni(Filename);
	try
		for Item in Items do
		begin
			if Item.Access in [cfgWrite, cfgReadWrite] then
			case Item.Kind of
				cfgBoolean:     Ini.WriteBool   (Item.Section, Item.Name, Boolean(Item.Data^));
				cfgByte:        Ini.WriteInteger(Item.Section, Item.Name, Byte(Item.Data^));
				cfgWord:        Ini.WriteInteger(Item.Section, Item.Name, Word(Item.Data^));
				cfgInteger:     Ini.WriteInteger(Item.Section, Item.Name, Integer(Item.Data^));
				cfgFloat:       Ini.WriteFloat  (Item.Section, Item.Name, Double(Item.Data^));
				cfgString:      Ini.WriteString (Item.Section, Item.Name, String(Item.Data^));
				cfgColor:       Ini.WriteString (Item.Section, Item.Name, ColorToHTML(TColor(Item.Data^)));
			end;
		end;
	finally
		Ini.Free;
	end;
end;


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
begin
	Fn := GetConfigFilePath;
	ForceDirectories(ExtractFilePath(Fn));

	Manager.Load(Fn);
	Directory.BPM   := ReadPath(Directory.BPM);
	Directory.Audio := ReadPath(Directory.Audio);

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
	Cfg.Add(cfgString, Sect, 'bpm',   @Directory.BPM,   '', cfgRead);
	Cfg.Add(cfgString, Sect, 'audio', @Directory.Audio, '', cfgRead);
	Cfg.Add(cfgBoolean, Sect, 'autoupdate', @Directory.AutoUpdate, False);

	Sect := 'window';
	Cfg.Add(cfgWord,    Sect, 'zoom',             @Window.Zoom,            100);
	Cfg.Add(cfgWord,    Sect, 'deckheight',       @Window.DeckPanelHeight, 400);
	Cfg.Add(cfgBoolean, Sect, 'titlebar.enabled', @Window.ShowTitlebar,    True);
	Cfg.Add(cfgBoolean, Sect, 'dirlist.enabled',  @Window.DirList.Enabled, True);
	for i := 0 to High(Window.FileList.ColumnVisible) do
		Cfg.Add(cfgBoolean, Sect, Format('filelist.columns.%d.visible', [i]),
			@Window.FileList.ColumnVisible[i], True);
	Cfg.Add(cfgBoolean, Sect, 'tracklist.visible', @Window.Tracklist.Visible, False);

	Sect := 'audio';
	Cfg.Add(cfgWord,    Sect, 'hz',             @Audio.Hz,           44100);
	Cfg.Add(cfgInteger, Sect, 'buffer',         @Audio.Buffer,       0);
	Cfg.Add(cfgWord,    Sect, 'updateperiod',   @Audio.UpdatePeriod, 0);
	Cfg.Add(cfgByte,    Sect, 'threads',        @Audio.Threads,      1);
	Cfg.Add(cfgInteger, Sect, 'normalize',      @Audio.TargetLUFS,   -10);
	Cfg.Add(cfgInteger, Sect, 'normalizegraph', @Audio.GraphLUFS,    -6);
	for i := 1 to High(Audio.Device) do
		Cfg.Add(cfgByte, Sect, 'device.' + IntToStr(i), @Audio.Device[i], 0);
	for i := 1 to High(Audio.SubDevice) do
		Cfg.Add(cfgByte, Sect, 'subdevice.' + IntToStr(i), @Audio.SubDevice[i], 0);

	Cfg.Add(cfgByte,    Sect, 'device.cue',     @Audio.CueDevice,  0);
	Cfg.Add(cfgBoolean, Sect, 'cueonfront',     @Audio.CueOnFront, False);

	Sect := 'mixer';
	Cfg.Add(cfgBoolean, Sect, 'enabled',   @Mixer.Enabled,   True);
	Cfg.Add(cfgByte,    Sect, 'mastervol', @Mixer.MasterVolume, 100);
	Cfg.Add(cfgByte,    Sect, 'cuemode',   @Mixer.CueMode,   0);
	Cfg.Add(cfgByte,    Sect, 'cuemix',    @Mixer.CueMix,    50);
	Cfg.Add(cfgBoolean, Sect, 'cuemaster', @Mixer.CueMaster, True);
	Cfg.Add(cfgBoolean, Sect, 'cuepostfader', @Mixer.CuePostFader, False);
	Cfg.Add(cfgBoolean, Sect, 'cuehardware',  @Mixer.CueUsesHardware, False);

	Sect := 'effects';
	Cfg.Add(cfgBoolean, Sect, 'enabled', @Effects.Enabled, False);

	Sect := 'deck';
	Cfg.Add(cfgBoolean, Sect, 'setmasterbpm',      @Deck.FirstSetsMasterBPM, True);
	Cfg.Add(cfgBoolean, Sect, 'showremainingtime', @Deck.ShowRemainingTime,  False);

	Cfg.Add(cfgBoolean, Sect, 'graph.horizontallines', @Deck.BeatGraph.ShowHorizontalLines, False);

	Cfg.Add(cfgBoolean, Sect, 'wave.showdual', @Deck.Waveform.ShowDual, False);
	Cfg.Add(cfgByte,    Sect, 'wave.height',   @Deck.Waveform.Height,   58);

	Cfg.Add(cfgWord,    Sect, 'warn.time',     @Deck.WarnTime,  60);
	Cfg.Add(cfgWord,    Sect, 'warn.speed',    @Deck.WarnSpeed, 20);

	Cfg.Add(cfgWord,    Sect, 'speed.spindown', @Deck.SpinDownSpeed, 250);

	Sect := 'controller';
	Cfg.Add(cfgString, Sect, 'config', @Controller.Config, '');

	Sect := 'theme.strings';
	with Theme.Strings.FileList do
	begin
		Cfg.Add(cfgString, Sect, 'filelist.directory', @Directory, '');
		Cfg.Add(cfgString, Sect, 'filelist.parent',    @ParentDirectory, '');
		Cfg.Add(cfgString, Sect, 'filelist.drives',    @Drives, '');
	end;

	Sect := 'theme.colors';

	with Theme.Colors.Base do
	begin
		Cfg.Add(cfgColor, Sect, 'base.background',     @Background,  Background);
	end;

	with Theme.Colors.FileList do
	begin
		Cfg.Add(cfgColor, Sect, 'list.gridlines',      @GridLines,   GridLines);
		Cfg.Add(cfgColor, Sect, 'list.fg.header',      @FgHeader,    FgHeader);
		Cfg.Add(cfgColor, Sect, 'list.bg.header',      @BgHeader,    BgHeader);
		Cfg.Add(cfgColor, Sect, 'list.hover',          @BgHover,     FgSelection);
		Cfg.Add(cfgColor, Sect, 'list.fg.selected',    @FgSelection, FgSelection);
		Cfg.Add(cfgColor, Sect, 'list.bg.selected',    @BgSelection, BgSelection);

		Cfg.Add(cfgColor, Sect, 'filelist.background', @Background.Normal, Background.Normal);
		Cfg.Add(cfgColor, Sect, 'filelist.background.focused', @Background.Focused, Background.Focused);

		Cfg.Add(cfgColor, Sect, 'filelist.fg',         @FileItem.FgDefault, FileItem.FgDefault);
		Cfg.Add(cfgColor, Sect, 'filelist.fg.hasbpm',  @FileItem.FgHasBPM,  FileItem.FgHasBPM);
		Cfg.Add(cfgColor, Sect, 'filelist.fg.played',  @FileItem.FgPlayed,  FileItem.FgPlayed);

		Cfg.Add(cfgColor, Sect, 'filelist.fg.parent',  @DirectoryItem.FgParent,    DirectoryItem.FgParent);
		Cfg.Add(cfgColor, Sect, 'filelist.bg.parent',  @DirectoryItem.BgParent,    DirectoryItem.BgParent);
		Cfg.Add(cfgColor, Sect, 'filelist.fg.dir',     @DirectoryItem.FgDirectory, DirectoryItem.FgDirectory);
		Cfg.Add(cfgColor, Sect, 'filelist.bg.dir',     @DirectoryItem.BgDirectory, DirectoryItem.BgDirectory);
		Cfg.Add(cfgColor, Sect, 'filelist.fg.drive',   @DirectoryItem.FgDrive,     DirectoryItem.FgDrive);
		Cfg.Add(cfgColor, Sect, 'filelist.bg.drive',   @DirectoryItem.BgDrive,     DirectoryItem.BgDrive);
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

