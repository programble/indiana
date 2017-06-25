#!/bin/sh
set -eu

input=`mktemp`
output=`mktemp`

echo ': TEST-MODE ;' > $input
cat indy.f $1 >> $input
echo 'TEST' >> $input

./indy < $input 2>&1 | sed 's/DSP=[0-9]*//g' > $output
diff -u $1.out $output || true

rm $input $output
