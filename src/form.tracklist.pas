unit Form.Tracklist;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, hListView, hSlider;

type
	TFormTracklist = class(TForm)
		SongList: ThListView;
		Scrollbar: ThRangeBar;
	private

	public

	end;

var
	FormTracklist: TFormTracklist;

implementation

{$R *.lfm}

end.

