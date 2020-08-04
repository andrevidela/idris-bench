#! /bin/zsh
rm -rf build/ttc/letTestOld.ttc build/ttc/letTestOld.ttm &&
idris2 -o letTestOld letTestold.idr &&
time build/exec/letTestOld

