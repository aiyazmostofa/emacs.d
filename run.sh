#!/bin/bash
PREFIX=puthere
g++ -std=c++11 -O2 -Wall main.cpp &> compile.log
if [[ $? != 0 ]]; then
    echo "* COMPILE TIME ERROR"
    echo "#+BEGIN_SRC"
    cat compile.log 
    echo "#+END_SRC"
    rm compile.log
    exit
fi
rm compile.log
for i in *.in; do
    [ -f "$i" ] || break
    cp $i $PREFIX.in
    echo "* $i"
    echo "#+BEGIN_SRC"
    timeout 5 ./a.out < $PREFIX.in
    [ -f "$PREFIX.out" ] && cat "$PREFIX.out"
    echo "#+END_SRC"
    rm -f $PREFIX.in $PREFIX.out
done
rm a.out
