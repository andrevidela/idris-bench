#! /bin/zsh
    rm -rf build/ttc/letTestMut.ttc build/ttc/letTestMut.ttm &&
    idris2dev -o letTestMut letTestMut.idr &&
    build/exec/letTestMut

