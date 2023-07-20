# Decks

A crossplatform deejaying software written in Free Pascal.

- Utilizes beat graphs for tempo determination, navigation and syncing
- Meant to be used with an external mixer, but it's possible to mix by visual cues only
- Crossfader, EQ controls and simple effects are provided, but no builtin headphone cueing
- Each deck can be assigned to any audio output device or speaker on the fly
- Support for MIDI controllers (config included only for Numark DJ2Go as that's the only controller I own)

![Screenshot](https://github.com/hukkax/Decks/blob/main/docs/images/main.png)

## Status

- I already use this in my DJ sets
- No documentation yet
- Linux Qt5/Qt6 builds need fixing
- Lots of work to be done regarding code quality, stability, GUI, features

## Build info

Use Lazarus to build.

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

- Left/Right: Browse decks
- Delete:     Close deck
- Space:      Play/Pause
- Control:    Cue
- Num+/Up:    Bend up
- Num-/Down:  Bend down
- Plus:       Add zone
- Minus:      Remove zone
