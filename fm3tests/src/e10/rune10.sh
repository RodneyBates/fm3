#/usr/bin/bash

TESTSDIR="."

function runtest # $1 is test directory simple name, e.g. e1001.
  {
    ../onetest.sh compile $1 
    ../onetest.sh check $1 
  }

runtest e1001 
runtest e1002 
runtest e1003 
runtest e1004 
runtest e1005 
runtest e1006 
runtest e1007 
runtest e1008 
runtest e1009 
runtest e1010 
runtest e1011
runtest e1012 


