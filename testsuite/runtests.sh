#!/bin/sh
DIRNAME=`dirname $0`

"${DIRNAME}"/tests/NinetyNine.hs

[ "1" = `echo "hello" | "${DIRNAME}"/../Wc.out -l` ] || echo "FAIL"
[ "0" = `echo -n "hello" | "${DIRNAME}"/../Wc.out -l` ] || echo "FAIL"
[ "9999999" = `yes | head -9999999 | "${DIRNAME}"/../Wc.out -l` ] || echo "FAIL"
[ "9999999" = `yes | head -9999999 | "${DIRNAME}"/../Wc.out -w` ] || echo "FAIL"
