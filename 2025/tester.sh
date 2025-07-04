#!/bin/sh

data=judgedata

sut="$1"
if [ ! "$sut" ]; then
  echo 'Usage: ./tester.sh system_under_test' 1>&2
  exit 1
fi
if [ ! -x "$sut" ]; then
  echo "$sut does not exist" 1>&2
  exit 1
fi

# problem code (A, B, ...)
p=`echo "${sut}" | sed 's/^\(.\).*/\1/' | tr a-z A-Z`

# run four test cases
for j in ${data}/${p}/0*.in
do
  ans=`echo "${j}" | sed 's/\.in$/.ans/'`
  if [ -f "${j}" -a -f "${ans}" ]; then
    printf '%s/%s\n' "${p}" `basename "${j}"`

    ./${sut} < "${j}" | diff - "${ans}"
  fi
done
