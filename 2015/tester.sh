#!/bin/sh

# ./tester.sh sut problem
# ex) ./tester.sh a.ml A

sut="$1"
problem="$2"

case x"$problem"x in
xx) echo 'Usage: ./tester.sh sut problem' 1>&2 ; exit 1;;
*)  ;;
esac

judgedir=judge

for i in 1 2 3 4
do
  testcase="${judgedir}/${problem}${i}"
  answer="${testcase}.ans"
  echo "$sut < $testcase | diff - $answer"
  $sut < "$testcase" | diff - "$answer"
done
