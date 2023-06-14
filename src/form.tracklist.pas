unit Form.Tracklist;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, hListView, hSlider;

type
	TFormTracklist = class(TForm)
		SongList: ThListView;
		Scrollbar: TDecksRangeBar;
		procedure FormShow(Sender: TObject);
	private

	public

	end;

var
	FormTracklist: TFormTracklist;

implementation

{$R *.lfm}

{ TFormTracklist }

procedure TFormTracklist.FormShow(Sender: TObject);
begin
	ClientHeight := SongList.ItemHeight * 2;
end;

end.

