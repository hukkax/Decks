unit TextInputDialog;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
	TFormTextInput = class(TForm)
		Edit: TEdit;
		bOK: TButton;
		bCancel: TButton;
		bTextTitleCase: TButton;
		bTextTrim: TButton;
		procedure bTextTitleCaseClick(Sender: TObject);
		procedure bTextTrimClick(Sender: TObject);
		procedure bTextTrimMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	private
	public
	end;

	function ToTitleCase(const S: String): String;
	function AskString(const Caption: String; var Text: String): Boolean;

implementation

{$R *.lfm}

uses
	StrUtils;

function ToTitleCase(const S: String): String;
begin
	Result := Trim(S);
	Result := Result.Replace('_', ' ', [rfReplaceAll]);
	while Result.Contains('  ') do
		Result := Result.Replace('  ', ' ', [rfReplaceAll]);
	Result := AnsiProperCase(Result, [' ', '-', '(', '[']);
end;

function AskString(const Caption: String; var Text: String): Boolean;
var
	Form: TFormTextInput;
begin
	Form := TFormTextInput.Create(nil);
	Form.Caption := Caption;
	Form.Edit.Text := Text;
	Form.Edit.SelStart := Length(Text);
	Form.ActiveControl := Form.Edit;
	Result := Form.ShowModal = mrOK;
	if Result then
		Text := Form.Edit.Text;
	Form.Free;
end;

{ TFormTextInput }

procedure TFormTextInput.bTextTitleCaseClick(Sender: TObject);
begin
	Edit.Text := ToTitleCase(Edit.Text);
end;

procedure TFormTextInput.bTextTrimClick(Sender: TObject);
begin
	Edit.Text := Trim(Edit.Text);
end;

procedure TFormTextInput.bTextTrimMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbMiddle then
	begin
		(Sender as TButton).Click;
		ModalResult := mrOK;
	end;
end;

procedure TFormTextInput.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
	X: Integer;
begin
	case Key of
		13: // Enter
			ModalResult := mrOK;
		27: // Escape
			ModalResult := mrCancel;
		8:  // Backspace
			if (ssCtrl in Shift) then
			begin
				X := RPos(' ', Edit.Text);
				if X > 0 then
				begin
					Edit.Text := Copy(Edit.Text, 1, X-1);
					Edit.SelStart := X;
				end;
				Key := 0;
			end;
	end;
end;

end.

