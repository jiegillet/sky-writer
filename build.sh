#! /bin/bash
elm make src/Main.elm --output=elm.js
# npm install --global uglify-js
COMPRESSION_OPTS='pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe'
npx uglifyjs elm.js --compress "$COMPRESSION_OPTS" | npx uglifyjs --mangle --output elm.min.js
rm elm.js
