main.js: $(shell find src -name '*.elm')
	elm-format src/*.elm --yes
	elm make --output main.js src/Main.elm
