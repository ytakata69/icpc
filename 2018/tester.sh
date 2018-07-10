#!/bin/sh

judge=judge

sut="$1"
if [ ! "$sut" ]; then
  echo 'Usage: ./tester.sh system_under_test' 1>&2
  exit 1
fi
if [ ! -x "$sut" ]; then
  echo "$sut does not exist" 1>&2
  exit 1
fi


p=`echo "${sut}" | sed 's/^\(.\).*/\1/' | tr a-z A-Z`
for n in 1 2 3 4
do
  j=${judge}/${p}${n}
  if [ -f "${j}" -a -f "${j}.ans" ]; then
    printf '%s%d\n' ${p} ${n}
    ./${sut} < "${j}" | diff - "${j}.ans"
  fi
done
