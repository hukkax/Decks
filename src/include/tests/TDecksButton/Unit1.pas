unit Unit1;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
	DecksButton, hKnob, DecksPanel, DecksLabel, DecksComboBox, Unit2;

type
	TForm1 = class(TForm)
		Panel1: TPanel;
		Panel: TDecksPanel;
		DecksButton1: TDecksButton;
		DecksButton2: TDecksButton;
		hKnob1: ThKnob;
		Panel2: TDecksPanel;
		Label1: TDecksLabel;
		ComboBox1: TDecksComboBox;
		procedure FrameUnfocused(Sender: TObject);
		procedure hKnob1Change(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure DecksButton1Click(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
	private

	public

	end;

var
	Form1: TForm1;
	Popup: TFrame1 = nil;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
	hKnob1Change(Self);
end;

procedure TForm1.FrameUnfocused(Sender: TObject);
begin
	FreeAndNil(Popup);
end;

procedure TForm1.DecksButton1Click(Sender: TObject);
begin
	if Popup <> nil then
	begin
		FreeAndNil(Popup);
	end
	else
	begin
		Popup := TFrame1.Create(Self);
		Popup.Parent := DecksButton1.Parent;
		Popup.SetBounds(DecksButton1.Left, DecksButton1.Top + DecksButton1.Height,
			DecksButton1.Width, 200);
		Popup.OnExit := @FrameUnfocused;
		Popup.Show;
	end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	if Popup <> nil then
		Popup.Free;
end;

procedure TForm1.hKnob1Change(Sender: TObject);
begin
	Panel.Bevel.Opacity := hKnob1.Position;
	Panel2.Bevel.Opacity := hKnob1.Position;
	DecksButton1.Bevel.Opacity := hKnob1.Position;
	DecksButton2.Bevel.Opacity := hKnob1.Position;
end;


end.

