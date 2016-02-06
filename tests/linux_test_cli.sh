#!/bin/bash

run="racket Locknut_CLI.rkt"
pass1="123456789101112131415"
pass2="1234567890"

txtA="/tmp/sample1.txt"
txtB="/tmp/sample2.txt"
lnA="/tmp/sample1.txt.locknut"
lnB="/tmp/sample2.txt.locknut"

# signals whether to run extended tests
extended=0

function cleanup() {
  rm -f /tmp/sample*
  cp tests/backup_samples/sample* /tmp/
}

# test script for CLI version on Linux
echo Starting tests
cd ..
cleanup

echo ; echo "[PASS] running cli_tests.rkt"
racket tests/cli_tests.rkt


# normal tests
echo ; echo "[PASS] encrypt file"
$run -e -p $pass1 $txtA

echo ; echo "[FAIL] decrypt file with incorrect password"
$run -d -p $pass2 $lnA

echo ; echo "[PASS] decrypt file"
$run -d -p $pass1 $lnA
cleanup


# extended tests
if test $extended == "1" ; then

  # bad arguments
  echo ; echo "[FAIL] encrypt non existant file"
  $run -e -p $pass1 bogusfile.txt

  echo ; echo "[FAIL] decrypt non existant file"
  $run -d -p $pass1 bogusfile.txt

  echo ; echo "[FAIL] don't specify encrypt or decrypt"
  $run -p $pass1 bogusfile.txt
  $run bogusfile.txt
  $run $txtA
  cleanup


  # no password tests
  echo ; echo "[PASS] encrypt file without password"
  $run -e $txtA

  echo ; echo "[FAIL] decrypt passwordless file with password"
  $run -d -p $pass1 $lnA

  echo ; echo "[PASS] decrypt passwordless file"
  $run -d $lnA
  cleanup


  # extension tests
  echo ; echo "[FAIL] encrypt file with .locknut extension"
  $run -e -p $pass1 $lnB

  echo ; echo "[FAIL] decrypt file without .locknut extension"
  $run -d -p $pass1 $txtB
  cleanup


  # standard key tests
  echo ; echo "[PASS] encrypt with standard key"
  $run -e -s -p $pass1 $txtA

  echo ; echo "[FAIL] decrypt with correct password, but personal key"
  $run -d -p $pass1 $lnA

  echo ; echo "[FAIL] decrypt with standard key, but wrong password"
  $run -d -s -p $pass2 $lnA

  echo ; echo "[FAIL] decrypt with standard key, but no password"
  $run -d  $lnA

  echo ; echo "[PASS] decrypt with standard key, correct password"
  $run -d -s -p $pass1 $lnA
  cleanup


  # password is key tests
  echo ; echo "[PASS] encrypt with pass-as-key"
  $run -e -V -p $pass1 $txtA

  echo ; echo "[FAIL] decrypt with correct password, but not pass-as-key"
  $run -d -p $pass1 $lnA

  echo ; echo "[FAIL] decrypt with pass-as-key, but wrong password"
  $run -d -V -p $pass2 $lnA

  echo ; echo "[PASS] decrypt with pass-as-key, correct password"
  $run -d -V -p $pass1 $lnA

fi

# done
echo ; echo Test finished
rm -f tests/sample*
