#!/bin/zsh

rm -rf build/ttc/fibTest_old.ttc build/ttc/fibTest_old.ttm
idris2dev -o fibTest_old fibTestNoMutation.idr
# idris2 -o fibTest_node_old fibTestNoMutation.idr --cg node

build/exec/fibTest_old
# time node --stack-size=20000 build/exec/fibTest_node_old
