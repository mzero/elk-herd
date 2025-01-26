#!/bin/sh

mkdir build 2>/dev/null || true

function build_dev {
	SRC="$1"
	MOD="$(basename "$SRC" .elm | tr A-Z a-z)"
	OUT="build/$MOD-dev.js"
	LOG="build/$MOD-errors.txt"

	echo "==="
	echo "=== Building $SRC --> $OUT"
	echo "==="

	elm make --optimize "$SRC" --output "$OUT" 2>"$LOG"
	if grep -q 'DEBUG REMNANTS' "$LOG"
	then
		sed -e '1s/.*-- /--/' -e '/But the/,$d' "$LOG"
		echo 'Rebuilding without --optimize:'
		elm make "$SRC" --output "$OUT"
	else
		cat "$LOG"
	fi
	rm "$LOG"

	echo
}

(cd src/SysEx && ln -sf Debug.elm.dev Debug.elm)

build_dev src/Main.elm
cat assets/*.js build/main-dev.js > build/main.js

build_dev src/Help.elm
cp build/help-dev.js build/help.js
