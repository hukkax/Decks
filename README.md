# CaniMix

A crossplatform deejaying software written in Free Pascal + Lazarus.

- Utilizes powerful beat graphs for tempo determination, navigation and syncing
- Crossfader/EQ/filter controls and simple effects
- Headphone cueing (requires sound device with quadraphonic channel support)
- Each deck can be assigned to any audio output device or speaker on the fly
- Support for MIDI controllers (configs included for Pioneer DDJ-FLX4 and Numark DJ2Go; customizable via ini files)

![Screenshot](https://github.com/hukkax/Decks/blob/main/docs/images/main.png)

## Status

- I've used this in my DJ sets for years now
- No documentation yet, UI not finalized (TODO: Tutorial)
- Linux Qt5/Qt6 builds need fixing
- Lots of work left regarding code quality, GUI, features

## Build info

I use FPC 3.2.2/Lazarus 4.0 to build currently. Small changes required to build with older versions.

- Windows: Primary target, stable
- Linux: Fully functional on Gtk2; Qt5/Qt6 crash on subsequent deck frame creation; Gtk3 broken UI
- Mac: Untested, please report findings

Required Lazarus packages:
- BGRAControls (can be installed via the online package manager)
- DecksGUI (included)

## Libraries used (included)

- BASS, BASS_FX, BASSmix (on Linux get the libs from un4seen.com)
- OvoTag
- RtMidi (if compiling with MIDI support - see rtmidi.pas for the required compiler flag depending on RtMidi version used)

## Partial TODO list

- Configuration dialogs (underway)
- Divide graph Zones into layers: Tempo vs. song structure
- Improve/fix Zone system in beatgraph
- Fix/improve/speedup various visuals (esp. waveform)
- Scrolling multi-waveform display
- Improve/optimize list component
- Improve resolution of Zones: bar -> beat

A full rewrite has been started, and most TODO items will be implemented there instead.
