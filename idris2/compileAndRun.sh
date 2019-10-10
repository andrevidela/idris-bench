#!/bin/zsh

rm -rf time.txt
touch time.txt
find . -type f -perm +111 -delete
rm -rf *.ibc
for n in $(ls *.idr) ; do
  base=$(basename $n .idr);
  idris2 $n -o $base
  echo $base >> time.txt
  { time ./$base.so > /dev/null; } 2>> time.txt
done
