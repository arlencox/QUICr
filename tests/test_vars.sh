#!/bin/bash

tf=/tmp/test.sdsl

for i in {0..40}
do
  unlink $tf
  # generate test
  for((j=0; $j < $i * 2; j = $j + 2))
  do
    echo "x$j = x$(($j + 1));" >> $tf
  done
  cat tests/for-in-update.sdsl >> $tf

  echo "$i pairs"
  time ./Main.native -bdd-full $tf
  time ./Main.native -bdd-full -pack $tf
done
