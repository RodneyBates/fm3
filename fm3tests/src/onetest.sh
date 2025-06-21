#/usr/bin/bash

# bash script for handling one FM3 compile test case.
# No attempt to run the compiled code.

# $1 must be one of "compile", "check", "test", "add", or "update"
# compile runs a single compile that incldes the  entire test case.
# check checks output files for presence and expected results.
# test does compile followed by check.
# update makes each *.expected file equal to its corresponding output file.
# add does a git adds for everything to be tracked in the test case.

# $2 is the top directory for the test, named, for example e1011. If omitted, defaults to "."

ACTION=$1
ESALL=0

# Text attributes:
# Why are these different from codes in FM3TextColors.i3? 
RED="\\033[31m"
GREEN="\\033[32m"
ORANGE="\\033[33m"
PLAIN="\\033[0m"

# Variables herein that are semantically boolean follow the bash exit status
# convention that 0 is true and 1 is false. 

  function checkarg # Check for one of the valid actions. 
    { docompile=1
      docheck=1
      doadd=1
      doupdate=1
      if [ ${ACTION}% = compile% ]
      then docompile=0
      elif [ ${ACTION}% = check% ]
      then docheck=0
      elif [ ${ACTION}% = test% ]
      then docompile=0
           docheck=0
      elif [ ${ACTION}% = add% ]
      then doadd=0
      elif [ ${ACTION}% = update% ]
      then doupdate=0
      else
        echo -e $TAG Unknown action: ${ACTION}
        echo -e $TAG "    Must be one of \"compile\", \"check\", \"test\", \"add\", or \"update\""
        ESALL=1
        return 1  
      fi
    }

  function multilog # $1 is message
    { # put $1 in all action-specified logs.
      if [ $docompile = 0 ]; then echo -e "$1" | tee -a $TMPCOMPILELOG; fi
      if [ $docheck = 0 ];   then echo -e "$1" | tee -a $CHECKLOG; fi
      if [ $doupdate = 0 ];  then echo -e "$1" | tee -a $UPDATELOG; fi
      if [ $doadd = 0 ];     then echo -e "$1" | tee -a $ADDLOG; fi
    } 

  function compile
    { # $SOURCES is list of source file simple names for the compile command line.
      local FM3
      local CLARGS
      local ES
      if [ $docompile = 0 ] 
      then

        # Optional file named fm3 contains path to compiler executable. 
        if test -f fm3 ; then FM3=`cat fm3`; else FM3="../../FM3"; fi
        FM3=`abspath $FM3`
        # Optional file named clargs contains compile command line options.
        if test -f clargs
        then CLARGS=`/clargs`
        else CLARGS="--disasm --no-disasm-verbose --exprs --no-expr-addrs" 
        fi 
        echo -e `$FM3 --version 2>&1` | tee -a $TMPCOMPILELOG 
        CMD="$FM3 $CLARGS $SOURCES"
        echo -e "$TAG Compiling in $SRCDIR," | tee -a $TMPCOMPILELOG 
        echo -e "$TAG   using command \"$CMD\"" | tee -a $TMPCOMPILELOG 
        cd $SRCDIR
        $CMD 2>&1 | tee tmplog0 
        ES=$?
        cat tmplog0 >> $TMPCOMPILELOG
        rm tmplog0
        if [ $ES -ne 0 ]
        then #compiler failure 
          echo -e $TAG $RED "Compiler failed." $PLAIN  
          ESALL=1  
        else #Compiler run ended normally.
          echo -e "$TAG $GREEN Compile finished OK" $PLAIN | tee -a $TMPCOMPILELOG  
        fi
        mv $TMPCOMPILELOG $TESTDIR/compilelog #Overlaying the old one.
      fi
    }
    
  function addOneFile
    { # Do a git add for a tracked file.
      # $1 is directory containing the file.
      # $2 is file simple name.
      local ES
      local CMD 
      if [ $doadd = 0 ]  
      then       
        if test -f ${1}/${2}
        then # File exists. 
          CMD="git add ${1}/${2}"
          #multilog "$TAG In directory ${1}," 
          echo -e -n "$TAG Running  $CMD ..." | tee  -a $ADDLOG 
          $CMD 2>&1 | tee -a $ADDLOG
          ES=$?
          if [ $ES = 0 ]
          then 
            echo -e $GREEN "OK." $PLAIN | tee -a $ADDLOG
          else
            echo -e $RED "FAILED." $PLAIN | tee -a $ADDLOG
            ESALL=1
          fi
        fi
      fi
    } 

  function updateOneFile
    { # Update an expected file to equal its compiler-produced counterpart.
      # $1 is directory containing the file and its expected file ${2}.expected
      # $2 is file simple name.
      local ES 
      local CMD 
      if [ $doupdate = 0 ] 
      then
        if test -f ${1}/${2}
        then # File exists. 
          CMD="cp -p ${1}/${2} ${1}/${2}.expected"
          #multilog "$TAG In directory ${1}," 
          echo -e -n "$TAG Running  $CMD ..." | tee  -a $UPDATELOG 
          $CMD 2>&1 | tee -a $UPDATELOG # Produces no output? 
          ES=$?
          if [ $ES = 0 ]
          then 
            echo -e $GREEN "OK." $PLAIN | tee -a $UPDATELOG
          else
            echo -e $RED "FAILED." $PLAIN | tee -a $UPDATELOG
            ESALL=1
          fi
        fi
      fi
    } 

  function checkOneFile
    { # check that a produced files matches its expected file.
      # $1 is directory containing the file and its expected file ${2}.expected
      # $2 is file simple name.
        local DIFCMD
        local DIFFLOG
        local PREVDIR
        local ES
      # if [ $ESALL -ne 0 ]; then return; fi
      if [ $docheck = 0 ] 
      then
        if test -f ${1}/${2}.expected 
        then #Expected file is present.
          DIFFLOG=${2}.diff 
          DIFFCMD="diff -u ${2}.expected ${2}"
          PREVDIR=`pwd`
          cd ${1}
          #multilog "$TAG In directory ${1}," 
          echo -e -n "$TAG Running $DIFFCMD ..." | tee -a $CHECKLOG 
          $DIFFCMD 2>&1 > $DIFFLOG  
          ES=$?
          if [ ${ES} = 0 ] # No differences.
          then
            rm -f ${DIFFLOG}
            rm -f ${TESTDIR}/${DIFFLOG} # Possible old version.
            echo -e $GREEN "OK." $PLAIN | tee -a $CHECKLOG
          else
            mv $DIFFLOG $TESTDIR
            echo -e "${RED} Not as expected$PLAIN" | tee -a $CHECKLOG
            ESALL=1
          fi
          cd $PREVDIR
        #elif test -f ${1}/${2}.expected 
        #then #expected file exists (but is empty).  Ignore this case.
        #  echo -e $TAG Expected file ${1}/${2}.expected "is empty, ignored." 2>&1 | tee -a $CHECKLOG
        else #expected file is missing altogether. 
          echo -e "$TAG Expected file ${1}/${2}.expected is ${RED}missing${PLAIN}." | tee -a $CHECKLOG
          ESALL=1
        fi
      fi
    }

# Main body.    
STARTDIR=`pwd` # To return to at end.
if [ "${2}%" = "%" ] ; then DIR="."; else DIR="${2}"; fi
TESTDIR="`abspath $DIR`"
cd $TESTDIR

# Does $TESTDIR/run.sh contain a unique test script for this test. 
if test -f run.sh 
then # run it with same arg.
  ./run.sh $ACTION 
else # Do this script.

  TAG="${ORANGE}script:$PLAIN "
  TAGN="          " 
  checkarg

  if test -f donotrun;
  then
    echo -e "${TAG} ${GREEN}Not running in ${TESTDIR}${PLAIN}" 
    touch ${TESTDIR}/NOTRUN
    exit
  fi      

  TMPCOMPILELOG="${TESTDIR}/tmpcompilelog" # Will rename at end.
  # Leave the leftover one around during compile.
  CHECKLOG="${TESTDIR}/checklog"
  if [ $docheck = 0 ]; then rm -f $CHECKLOG; fi # Remove old leftover.
  UPDATELOG="${TESTDIR}/updatelog"
  if [ $doupdate = 0 ]; then rm -f $UPDATELOG; fi  # Remove old leftover.
  ADDLOG="${TESTDIR}/addlog"
  if [ $doadd = 0 ]; then rm -f $ADDLOG; fi # Remove old leftover.
  if [ $docheck = 0 ]
  then rm -f ${TESTDIR}/SUCCEEDED ${TESTDIR}/FAILED ${TESTDIR}/NOTRUN
  fi 
  multilog "$TAG Command entered in directory = $STARTDIR"
  multilog "$TAG Test directory is $TESTDIR" 
    
  if [ $ESALL -ne 0 ]; then exit 1; fi

  SRCDIR="$TESTDIR/src"
  BUILDDIR="$TESTDIR/build"

  # Optional file srcdirs lists directories to look for source files in.
  if test -f srcdirs ; then SRCDIRS=`cat srcdirs`; else SRCDIRS="$SRCDIR"; fi

  # Optional file sources lists source file simple names for the compile command.
  if test -f sources ; then SOURCES=`cat sources`; else SOURCES="Main.m3"; fi

  # Optional file imports list source file simple names that will be compiled
  # due to IMPORT and EXPORTS. The compiler will find them in a single run,
  # but check, update, or add results from them 
  if test -f imports ; then IMPORTS=`cat imports`; else IMPORTS=""; fi
  
  ALLSRCS="$SOURCES $IMPORTS"
  echo SOURCES = $SOURCES
  echo IMPORTS = $IMPORTS
  echo ALLSRCS = $ALLSRCS

  # Optional file streams contains name suffixes of intermediate stream files.
  if test -f streams; then STREAMS=`streams`; else STREAMS="FM3Pass1 FM3Pass2"; fi

  # Optional file exprs contains name suffixes of data structure dumps files.
  if test -f exprs ; then EXPRS=`cat exprs`; else EXPRS="FM3Pass2.Exprs"; fi

  # Maybe run the compiler.
  compile

  # FM3Log
  checkOneFile ${SRCDIR} FM3Log
  updateOneFile ${SRCDIR} FM3Log
  addOneFile ${SRCDIR} FM3Log
  addOneFile ${SRCDIR} FM3Log.expected

  # Test-environment files:
  addOneFile ${SRCDIR} srcdirs
  addOneFile ${SRCDIR} sources
  addOneFile ${SRCDIR} imports
  addOneFile ${SRCDIR} streamss
  addOneFile ${SRCDIR} exprs
  # addOneFile ${SRCDIR} donotrun
  # Weird paradox. donotrun will have to be added and removed manually.  
    
  # Command line source files:
  for SRC in $SOURCES
  do
    addOneFile $SRC # Source file itself
    # Per-source log files: 
    checkOneFile ${SRCDIR} ${SRC}.Log
    updateOneFile ${SRCDIR} ${SRC}.Log
    addOneFile ${SRCDIR} ${SRC}.Log
    addOneFile ${SRCDIR} ${SRC}.Log.expected
  done

  # EXPORT/IMPORT source files.
  for SRC in $IMPORTS
  do
    # Per-source log files: 
    checkOneFile ${SRCDIR} ${SRC}.Log
    updateOneFile ${SRCDIR} ${SRC}.Log
    addOneFile ${SRCDIR} ${SRC}
    addOneFile ${SRCDIR} ${SRC}.Log
    addOneFile ${SRCDIR} ${SRC}.Log.expected
  done

  # Handle compiler-built files.
  for SRC in $ALLSRCS
  do 
    # Do stream files:       
    for STREAM in $STREAMS 
    do checkOneFile $BUILDDIR ${SRC}.${STREAM}.DisAsm 
       updateOneFile $BUILDDIR ${SRC}.${STREAM}.DisAsm
       addOneFile $BUILDDIR ${SRC}.${STREAM}
       addOneFile $BUILDDIR ${SRC}.${STREAM}.DisAsm
       addOneFile $BUILDDIR ${SRC}.${STREAM}.DisAsm.expected
    done #streams

    # Do expression dumps: 
    for EXPR in $EXPRS 
    do checkOneFile $BUILDDIR  ${SRC}.${EXPR}
       updateOneFile $BUILDDIR  ${SRC}.${EXPR}
       addOneFile $BUILDDIR  ${SRC}.${EXPR}
       addOneFile $BUILDDIR  ${SRC}.${EXPR}.expected
    done #exprs
  done # Sources

  # Summarize:
  if [ $docheck = 0 ] 
  then # Doing check.  Report results.
    if [ $ESALL -ne 0 ]
    then # Check failure 
      touch ${TESTDIR}/FAILED 
      echo -e -n $TAG $RED "################ F A I L E D ################" $PLAIN | tee -a $CHECKLOG
      echo -e " check(s)  in `pwd`" | tee -a $CHECKLOG
    else # Check success.
      touch ${TESTDIR}/SUCCEEDED
      touch ${TESTDIR}/LASTSUCCEEDED
      echo -e "$TAG $GREEN ---------------- Succeeded in `pwd` $PLAIN" | tee -a $CHECKLOG 
    fi
  fi

  # Finish up.
  cd $STARTDIR
  if [ $ESALL -ne 0 ]; then exit 1; fi 

fi 
