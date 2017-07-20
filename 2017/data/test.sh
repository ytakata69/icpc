#!/bin/sh

problems=$@

case x"$problems" in
x) problems='A B C D E G' ;;
esac

for p in $problems
do
  cd ../$p
  if [ -f main.c ]; then
    make -f ../data/test-makefile prob=$p test-aout
  fi
  if [ -f main.py ]; then
    make -f ../data/test-makefile prob=$p test-py
  fi
  if [ -f Main.java ]; then
    make -f ../data/test-makefile prob=$p test-j
  fi
  if [ -f Main2.java ]; then
    make -f ../data/test-makefile prob=$p test-j2
  fi
done
