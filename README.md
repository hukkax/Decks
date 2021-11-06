# Decks

A deejaying software written in Free Pascal.

- Utilizes beat graphs for tempo determination and navigation
- Meant to be used with an external mixer, but it's possible to mix by visual cues only
- Crossfader and EQ controls are provided, but no headphone cueing
- Unlimited number of simultaneous decks; decks can be freely assigned to any audio output device

## Status

- I already use this in my DJ sets
- No documentation yet
- Linux build unstable and thus unusable
- Lots of work to be done regarding code quality, stability, GUI, features

## Build info

Use Lazarus to build.

- Windows: Stable
- Linux: Builds and runs; crashes randomly during playback
- Mac: Untested

Required Lazarus packages:
- BGRAControls
- DecksGUI (included)

## Libraries used (included)

- BASS, BASS_FX, BASSmix
- OvoTag
- RtMidi

![Screenshot](https://github.com/hukkax/Decks/blob/main/docs/images/main.png)
