{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit decksgui;

{$warn 5023 off : no warning about unused units}
interface

uses
    DecksButton, hSlider, hListView, hShellTree, hKnob, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DecksButton', @DecksButton.Register);
  RegisterUnit('hSlider', @hSlider.Register);
  RegisterUnit('hListView', @hListView.Register);
  RegisterUnit('hShellTree', @hShellTree.Register);
  RegisterUnit('hKnob', @hKnob.Register);
end;

initialization
  RegisterPackage('decksgui', @Register);
end.
