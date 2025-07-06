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

dir="${data}/${p}"


# for Problems E & F
if [ "${p}" = E -o "${p}" = F ]; then
  output_checker="${dir}/output_checker"
  if [ ! -x "$output_checker" ]; then
    (cd "${dir}" && \
     g++ -o `basename "$output_checker"` --std=c++11 output_checker.cc)
  fi
fi
tmpout="tmp.$$"


# run four test cases
for j in "${dir}"/0*.in
do
  ans="${dir}"/`basename "${j}" .in`.ans
  if [ -f "${j}" -a -f "${ans}" ]; then
    printf '%s/%s\n' "${p}" `basename "${j}"`

    if [ -n "$output_checker" -a -x "$output_checker" ]; then
      ./${sut} < "${j}" > "${tmpout}"
      "${output_checker}" "${j}" "${tmpout}" "${ans}" || echo "Fail"
      rm "${tmpout}"
    else
      ./${sut} < "${j}" | diff - "${ans}"
    fi
  fi
done
