build:
	elm make src/Matematik.elm --output=matematik.js

live:
	elm-live src/Matematik.elm --no-server -- --output=matematik.js --optimize
