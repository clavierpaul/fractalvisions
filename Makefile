out.mp4: out/%.png
	ffmpeg -framerate 60 -i out/frame%04d.png -i song.wav -c:v libx264 -c:a libvorbis -shortest out.mp4

out/%.png:
	mkdir -p out/
	stack run

clean:
	rm -rf out/
	rm -f *.mp4