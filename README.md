# CaniMix

A crossplatform deejaying software written in Free Pascal.

- Utilizes beat graphs for tempo determination, navigation and syncing
- Crossfader, EQ controls and simple effects are provided
- Headphone cueing (requires sound card with quadraphonic channel support)
- Each deck can be assigned to any audio output device or speaker on the fly
- Support for MIDI controllers (config included only for Numark DJ2Go as that's the only controller I own)

![Screenshot](https://github.com/hukkax/Decks/blob/main/docs/images/main.png)

## Status

- I already use this in my DJ sets
- No documentation yet
- Linux Qt5/Qt6 builds need fixing
- Lots of work to be done regarding code quality, stability, GUI, features

## Build info

Use Lazarus to build. I use 3.0 RC1.

- Windows: Stable
- Linux: Fully functional on Gtk2; Qt5/Qt6 crash on subsequent deck frame creation; Gtk3 broken UI
- Mac: Untested

Required Lazarus packages:
- BGRAControls (can be installed via the online package manager)
- DecksGUI (included)

## Libraries used (included)

- BASS, BASS_FX, BASSmix (on Linux get the libs from un4seen.com)
- OvoTag
- RtMidi (if compiling with MIDI support - see rtmidi.pas for the required compiler flag depending on RtMidi version used)

## Keyboard controls

TBD (being reworked)

## Partial TODO list

- Configuration dialogs
- Fix/improve/speedup various visuals (esp. waveform)
- Scrolling multi-waveform display
- Improve list component
- Improve/fix Zone system in beatgraph
- Improve resolution of Zones: bar -> beat
- Divide graph Zones into layers: Tempo vs. song structure
