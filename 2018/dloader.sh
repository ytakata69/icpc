#!/bin/sh

url=http://icpc.iisf.or.jp/past-icpc/domestic2018/judgedata
judge=judge

if [ ! -d ${judge} ]; then
  mkdir ${judge}
fi
cd ${judge}

for p in A B C D E F G H
do
  for n in 1 2 3 4
  do
    curl -O ${url}/${p}/${p}${n}
    curl -O ${url}/${p}/${p}${n}.ans
  done
done
