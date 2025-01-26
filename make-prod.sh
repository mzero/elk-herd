#!/bin/sh

set -e

mkdir build 2>/dev/null || true
rm -rf elm-stuff   # clean compile, each time


function run_null {
  cp "$1" "$2"
}

function run_uglifyjs {
  local opts
  opts='pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9]'
  opts=$opts,'pure_getters,keep_fargs=false,unsafe_comps,unsafe'

  uglifyjs "$1" --compress "$opts" | uglifyjs --mangle --source-map --output "$2"
}

function run_terser {
  local opts
  opts='pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9]'
  opts=$opts,pure_getters,keep_fargs=false,unsafe_comps,unsafe

  terser "$1" --compress "$opts" | terser --mangle --source-map --output "$2"
}

function run_closure_local {
  # ---> SET THESE FOR YOUR SYSTEM <---
  java_exe=/Applications/Minecraft.app/Contents/runtime/jre-x64/1.8.0_74/bin/java
  closure_jar=/Users/markl/Projects/A/closure-compiler/closure-compiler-v20190929.jar

  "$java_exe" -jar "$closure_jar" \
  --js "$1" \
  --js_output_file "$2" \
  --create_source_map "$2.map" \
  --jscomp_off uselessCode
}

function run_closure_api {
  # Warning: Google is about to sunset this service

  curl --output "$2" \
    --data output_info=errors \
    --data output_info=compiled_code \
    --data-urlencode "js_code@$1" \
    https://closure-compiler.appspot.com/compile
}



function compact_js {
  local mod="$1"
  local src="build/$mod.js"
  local out="build/$mod-min.js"

  # ---> PICK ONE <---
  # run_null "$src" "$out"
  # run_uglifyjs "$src" "$out"
  run_terser "$src" "$out"
  # run_closure_local "$src" "$out"
  # run_closure_api "$src" "$out"

  gzip --keep --force "$out"

  ls -lh "build/$mod"*
}

(cd src/SysEx && ln -sf Debug.elm.prod Debug.elm)

echo "==="
echo "=== Building Main.elm"
echo "==="

elm make --optimize src/Main.elm --output build/main-prod.js
cat assets/*.js build/main-prod.js > build/main.js
compact_js main

echo

echo "==="
echo "=== Building Help.elm"
echo "==="

elm make --optimize src/Help.elm --output build/help-prod.js
cp build/help-prod.js build/help.js
compact_js help

echo

echo "==="
echo "=== Packaging"
echo "==="

rm -rf distribution

appdir=distribution/elk-herd
mkdir -p "$appdir"
cp -rp assets "$appdir"
rm "$appdir/assets/"*.js    # already incorporated into the main.js file
mkdir -p "$appdir/build"
cp -p build/main-min.js    "$appdir/build/main.js"
cp -p build/main-min.js.gz "$appdir/build/main.js.gz"
cp -p build/help-min.js    "$appdir/build/help.js"
cp -p build/help-min.js.gz "$appdir/build/help.js.gz"
cp -p index.html "$appdir"
cp -p help.html "$appdir"

(cd distribution; \
 tar cvzf elk-herd-live.tgz elk-herd | sort \
)

echo
