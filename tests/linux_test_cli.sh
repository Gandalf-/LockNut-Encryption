#!/bin/bash

run="racket Locknut_CLI.rkt"
pass1="123456789101112131415"
pass2="1234567890"

# test script for CLI version on Linux
echo Starting tests
cp backup_samples/sample1.txt .
cp backup_samples/sample2.txt .
cd ..

# encrypt file, PASS
echo
echo encrypt file, should PASS
$run -e -p $pass1 tests/sample1.txt

# encrypt file with .locknut extension, should FAIL
echo
echo encrypt file with .locknut extension, should FAIL
$run -e -p $pass1 tests/sample2.txt

# decrypt file with incorrect password, should FAIL
echo
echo decrypt file with incorrect password, should FAIL
$run -d -p $pass2 tests/sample1.locknut

# decrypt file, should PASS
echo
echo decrypt file, should PASS
$run -d -p $pass1 tests/sample1.locknut

# decrypt file without .locknut extension, should FAIL
echo
echo decrypt file without .locknut extension, should FAIL
$run -d -p $pass1 tests/sample1.txt

echo
echo Test finished
