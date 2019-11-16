# FRACTAL/VISIONS

A fractal based music video for the song Science/Visions by CHVRCHES, written in Haskell.

## Dependencies

- Haskell Stack

- JuicyPixels, random, array, parallel, colour

```
stack install JuicyPixels random array parallel
```

- FFmpeg

On Ubuntu and Debian-based systems:
```
sudo apt install ffmpeg
```

On macOS
```
brew install ffmpeg
```

## Building

1. Install the dependencies
2. Run `make`
3. After rendering, the video shoud be in the root folder under the name `out.mp4`

## Technical Explanation

Using Haskell, we generate a fractal on every frame and write the fractal as a png file into `out/`. The parameters of the fractal (coefficients, zoom, depth, color saturation, etc.) are controlled by the section of the song and the kick drum. FFmpeg is then used to combine the frames into a video and add the song audio.

The hardest part of the video was likely the circle which appears on vocal chops. Though this was expected to be easy, we ran into several issues in the implementation. Unfortunately, the circle was scrapped due to time constraints, however the code remains. Otherwise, scaffolding the project in general and dealing with Haskell's ~~oddities~~ innovative approach to program state was a fun challenge.

## Credits
### Circle-generating algorithm
Source: [Phil Schumann's GitHub repository](https://github.com/metaleap/rosetta-haskell-dump/blob/master/bitmap-midpoint-circle-algorithm-1.hs)

