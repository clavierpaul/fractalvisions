BUILD := $(shell stack path --local-install-root)

out.mp4: out/%.png
	ffmpeg -framerate 60 -i out/frame%05d.png -i song.wav -c:v libx264 -c:a libvorbis -shortest out.mp4

out/%.png:
	mkdir -p out/
	stack build
	cp $(BUILD)/bin/fractalvisions .
	./fractalvisions

clean:
	rm -rf out/
	rm -f *.mp4