unit Form.Config;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type
	TConfigForm = class(TForm)
		PageControl1: TPageControl;
		TabSheet1: TTabSheet;
		TabSheet2: TTabSheet;
		TabSheet3: TTabSheet;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		ScrollBar1: TScrollBar;
		ScrollBar2: TScrollBar;
		ScrollBar3: TScrollBar;
		ScrollBar4: TScrollBar;
		ScrollBar5: TScrollBar;
		Label6: TLabel;
		Label7: TLabel;
		Label8: TLabel;
		Label9: TLabel;
		Label11: TLabel;
		Label10: TLabel;
		Label12: TLabel;
		Label13: TLabel;
		Label14: TLabel;
		Label15: TLabel;
		Label16: TLabel;
		GroupBox1: TGroupBox;
		RadioButton1: TRadioButton;
		RadioButton2: TRadioButton;
		CheckBox1: TCheckBox;
		CheckBox2: TCheckBox;
		Label17: TLabel;
		ComboBox1: TComboBox;
		GroupBox2: TGroupBox;
		Label18: TLabel;
		Label19: TLabel;
		cmbController: TComboBox;
		cbHardwareCue: TCheckBox;
		cmbCue: TComboBox;
		cbCueOnFront: TCheckBox;
		Label20: TLabel;
		lbDevices: TListBox;
		Button1: TButton;
		procedure cmbCueChange(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
	private
		DevCue, DevNoCue: Integer;
	public
	end;

var
	ConfigForm: TConfigForm;

implementation

{$R *.lfm}

uses
	BASS,
	FileUtil,
	Decks.Config;

{ TConfigForm }

procedure TConfigForm.FormShow(Sender: TObject);
var
	Info: BASS_DEVICEINFO;
	S: String;
	i: Integer;
	ctrls: TStringList;
begin
	// populate list of available audio devices
	i := 1; // first real audio output device (0 = no sound)
	lbDevices.Clear;
	while BASS_GetDeviceInfo(i, Info) do
	begin
		if (Info.flags and BASS_DEVICE_ENABLED) <> 0 then
		begin
			S := Info.name;
			if (i > 1) and ((Info.flags and BASS_DEVICE_DEFAULT) <> 0) then
				S := S + ' (Default)';
			lbDevices.Items.Add(S);
		end;
		Inc(i);
	end;

	// populate list of available controller configs
	ctrls := FindAllFiles(Config.GetPath + 'midinew', '*.ini', False);
	for S in ctrls do
		cmbController.Items.Add(ExtractFileName(S));
	ctrls.Free;

	DevNoCue := Config.Audio.Device[1];
	DevCue   := Config.Audio.CueDevice;
	cmbCue.ItemIndex := Config.Mixer.CueMode;
	cbCueOnFront.Checked := Config.Audio.CueOnFront;
	cbHardwareCue.Checked := Config.Mixer.CueUsesHardware;
	S := Config.Controller.Config;

	cmbController.ItemIndex := 0;
	for i := 0 to cmbController.Items.Count-1 do
		if cmbController.Items[i] = S then
		begin
			cmbController.ItemIndex := i;
			Break;
		end;

	cmbCueChange(Self);
end;

procedure TConfigForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
	i, V: Integer;
begin
	V := lbDevices.ItemIndex;
	if cmbCue.ItemIndex = 0 then
	begin
		for i := 1 to 4 do
			Config.Audio.Device[i] := V;
	end
	else
	begin
		Config.Audio.CueDevice := V;
	end;

	Config.Mixer.CueMode := cmbCue.ItemIndex;
	Config.Audio.CueOnFront := cbCueOnFront.Checked;
	Config.Mixer.CueUsesHardware := cbHardwareCue.Checked;
	if cmbController.ItemIndex = 0 then
		Config.Controller.Config := ''
	else
		Config.Controller.Config := cmbController.Items[cmbController.ItemIndex];
end;

procedure TConfigForm.cmbCueChange(Sender: TObject);
var
	i: Integer;
begin
	if cmbCue.ItemIndex = 0 then
		i := DevNoCue
	else
		i := DevCue;

	if (i >= 0) and (i < lbDevices.Count) then
		lbDevices.ItemIndex := i;
end;

end.

