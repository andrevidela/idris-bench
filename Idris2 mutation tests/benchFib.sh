#!/bin/zsh

rm -rf build/ttc/fibTest.ttc build/ttc/fibTest.ttm
idris2dev -o fibTest fibTest.idr
# build/exec/idris2dev -o fibTest_node fibTest.idr --cg node

build/exec/fibTest
# time node --stack-size=20000 build/exec/fibTest_node
