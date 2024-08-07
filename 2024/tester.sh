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

# for Problem E
output_checker="${data}/E/output_checker"
if [ "${p}" = E -a ! -x "$output_checker" ]; then
  (cd "${data}/${p}" && \
   g++ -o `basename "$output_checker"` --std=c++11 output_checker.cc)
fi
tmpout="tmp.$$"

# run four test cases
for n in 1 2 3 4
do
  j=${data}/${p}/${p}${n}
  if [ -f "${j}" -a -f "${j}.ans" ]; then
    printf '%s%d\n' ${p} ${n}

    if [ "${p}" = E ]; then
      ./${sut} < "${j}" > "${tmpout}"
      "${output_checker}" "${j}" "${tmpout}" "${j}.ans" || echo "Fail"
      rm "${tmpout}"
    else
      ./${sut} < "${j}" | diff - "${j}.ans"
    fi
  fi
done
