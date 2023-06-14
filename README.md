# Decks

A deejaying software written in Free Pascal.

- Utilizes beat graphs for tempo determination and navigation
- Meant to be used with an external mixer, but it's possible to mix by visual cues only
- Crossfader and EQ controls are provided, but no builtin headphone cueing
- Each deck can be assigned to any audio output device

![Screenshot](https://github.com/hukkax/Decks/blob/main/docs/images/main.png)

## Status

- I already use this in my DJ sets
- No documentation yet
- Linux build needs work
- Lots of work to be done regarding code quality, stability, GUI, features

## Build info

Use Lazarus to build.

- Windows: Stable enough
- Linux: Builds and runs; unstable on Qt5, scaling issues on GTK2
- Mac: Untested

Required Lazarus packages:
- BGRAControls
- DecksGUI (included)

## Libraries used (included)

- BASS, BASS_FX, BASSmix
- OvoTag
- RtMidi (if compiling with MIDI support)

## Keyboard controls

- Left/Right: Browse decks
- Delete:     Close deck
- Space:      Play/Pause
- Control:    Cue
- Num+/Up:    Bend up
- Num-/Down:  Bend down
- Plus:       Add zone
- Minus:      Remove zone
