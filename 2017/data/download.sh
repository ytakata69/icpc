#!/bin/sh
url=http://icpc.iisf.or.jp/past-icpc/domestic2017/judgedata/

for p in A B C D E F G H
do
  curl -O "${url}/${p}/${p}"'[1-4]'
  curl -O "${url}/${p}/${p}"'[1-4].ans'
done
