#!/bin/sh

url=https://icpc.iisf.or.jp/past-icpc/domestic2015/qualify2015_data
judge=judge

if [ ! -d ${judge} ]; then
  mkdir ${judge}
fi
cd ${judge}

for p in A B C D E F G H
do
  for n in 1 2 3 4
  do
    curl -O ${url}/${p}${n}
    curl -O ${url}/${p}${n}.ans
  done
done
