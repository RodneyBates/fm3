#/usr/bin/bash

TESTSDIR="."

function runtest # $1 is test directory simple name, e.g. e1001.
  {
    ../onetest.sh compile $1 
    ../onetest.sh check $1 
  }

runtest c1001 
runtest c1002 
runtest c1003 
runtest c1004 
runtest c1005 
runtest c1006 
runtest c1007 
runtest c1008 
runtest c1009 
runtest c1010 
runtest c1011 
runtest c1012
runtest c1013
runtest c1014
runtest c1015
runtest c1016
runtest c1017
runtest c1018
runtest c1019
runtest c1020
runtest c1021
runtest c1022
runtest c1023
runtest c1024
runtest c1025


