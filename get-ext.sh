#!/bin/sh

set -e

rm -rf build/ext
rm -rf assets/ext

mkdir -p build/ext 2>/dev/null || true
mkdir -p assets/ext 2>/dev/null || true



# JQUERY

JQ_VER='3.4.1'

echo "==="
echo "=== Fetching jQuery ${JQ_VER} ..."
echo "==="

curl -o build/ext/jquery-${JQ_VER}.min.js \
	"https://code.jquery.com/jquery-${JQ_VER}.min.js"

cp -p build/ext/jquery-${JQ_VER}.min.js  assets/ext/jquery-${JQ_VER}.min.js

echo
echo

# BOOTSTRAP

BS_VER='4.3.1'

echo "==="
echo "=== Fetching Bootstrap ${BS_VER} ..."
echo "==="

curl -o build/ext/bootstrap-${BS_VER}-dist.zip -L \
	"https://github.com/twbs/bootstrap/releases/download/v${BS_VER}/bootstrap-${BS_VER}-dist.zip"
unzip build/ext/bootstrap-${BS_VER}-dist.zip -d build/ext

cp -p build/ext/bootstrap-4.3.1-dist/js/bootstrap.bundle.min.js \
	assets/ext/bootstrap-${BS_VER}.bundle.min.js
cp -p build/ext/bootstrap-4.3.1-dist/css/bootstrap.min.css \
	assets/ext/bootstrap-${BS_VER}.min.css

echo
echo

# FONTS

echo "==="
echo "=== Fetching fonts ..."
echo "==="

curl -o build/ext/Source_Code_Pro.zip -L \
	"https://github.com/adobe-fonts/source-code-pro/releases/download/2.042R-u%2F1.062R-i%2F1.026R-vf/TTF-source-code-pro-2.042R-u_1.062R-i.zip"
unzip build/ext/Source_Code_Pro.zip -d build/ext/fonts

cp -p build/ext/fonts/TTF/SourceCodePro-Semibold.ttf assets/ext


echo
echo

echo "==="
echo "=== ... done!"
echo "==="
