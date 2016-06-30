#!/bin/sh

problems=$@
case "x$problems" in
x) problems='A B C D E F' ;;
esac

data=../data

for p in $problems
do
  (cd ../$p
   echo javac $p.java
   javac $p.java
   for n in 1 2 3 4
   do
     echo java $p '<' $data/$p$n '|' diff - $data/$p$n.ans
     java $p < $data/$p$n | diff - $data/$p$n.ans
   done
  )
done
