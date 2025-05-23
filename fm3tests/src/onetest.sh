#/usr/bin/bash

# bash script for handling one FM3 compile test case.
# No attempt to run the compiled code.

# $1 must be one of "compile", "check", "test", "add", or "update"
# compile runs a single compile that incldes the  entire test case.
# check checks output files for presence and expected results.
# test does compile followed by check.
# add does a git adds for everything to be tracked in the test case.
# update makes each *.expected file equal to its corresponding output file.
# $2 is the top directory for the test, named, for example e1011. If omitted,
# defaults to "."

ACTION=$1
ESALL=0

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
        echo $TAG Unknown action: ${ACTION}
        echo $TAG "  Must be one of \"compile\", \"check\", \"test\", \"add\", or \"update\""
        ESALL=1
        return 1  
      fi
    }

    function compile
    { # $SOURCES is list of source file simple names for the compile command line.
      local FM3
      local CLARGS
      local ES
      if [ $ESALL -ne 0 ]; then return; fi
      if [ $docompile = 0 ] 
      then
        # Optional file named fm3 conatains path to compiler executable. 
        if test -f fm3 ; then FM3=`cat fm3`; else FM3="../../FM3"; fi
        FM3=`abspath $FM3`
        # Optional file named clargs contains compile command line options.
        if test -f clargs
        then CLARGS=`cat clargs`
        else CLARGS="--disasm --no-disasm-verbose --exprs" 
        fi 
        echo $TAG `$FM3 --version 2>&1` | tee -a $TESTLOG 
        CMD="$FM3 $CLARGS $SOURCES"
        echo $TAG "Running compile in $SRCDIR," | tee -a $TESTLOG 
        echo $TAG "  using command $CMD" | tee -a $TESTLOG 
        cd $SRCDIR
        $CMD 2>&1 > tmplog0 
        ES=$?
        cat tmplog0 | tee -a $TESTLOG
        rm tmplog0
        if [ $ES -ne 0 ]
        then #compiler crashed.
          echo $TAG $RED "Compile failed." $PLAIN  
          ESALL=1  
        else #Compiler run ended normally.
          echo $TAG $GREEN "Compile finished OK" $PLAIN 
        fi
      fi
    }
    
  function addOneFile
    { # Do a git add for a tracked file.
      # $1 is directory containing the file.
      # $2 is file simple name.
      local ES 
      if [ $ESALL -ne 0 ]; then return; fi
      if [ $doadd = 0 ]  
      then       
        if test -f ${1}/${2}
        then
          git add ${1}/${2} 
          ES=$?
          if [ ES -ne 0 ]; then ESALL=1; fi
        fi
      fi
    } 

  function updateOneFile
    { # Update an expected file to equal its compiler-produced counterpart.
      # $1 is directory containing the file and its expected file ${2}.expected
      # $2 is file simple name.
      local ES 
      if [ $ESALL -ne 0 ]; then return; fi
      if [ $doupdate = 0 ] 
      then
        if test -f ${1}/${2}
        then
          cp -p ${1}/${2} ${1}/${2}.expected
          ES=$?
          if [ ES > 0 ]; then ESALL=1; fi
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
        if test -s ${1}/${2}.expected 
        then #Expected file is present and nonempty.
          DIFFCMD="diff -u ${2}.expected ${2}"
          DIFFLOG=${1}/${2}.diff 
          echo $TAG "In directory ${1}," | tee -a $TESTLOG
          PREVDIR=`pwd`
          cd ${1}
          echo $TAG "  running" $DIFFCMD | tee -a $TESTLOG 
          $DIFFCMD 2>&1 > $DIFFLOG  
          ES=$?
          cd $PREVDIR
          mv $DIFFLOG $TESTDIR
          if [ ${ES} = 0 ]
          then
            echo $TAG $GREEN "  OK." $PLAIN 
          else
            echo $TAG $RED "  File" ${2} not as expected $PLAIN 2>&1 | tee -a $TESTLOG
            ESALL=1
          fi
        elif test -f ${1}/${2}.expected 
        then #expected file exists (but is empty).  Ignore this case.
          echo $TAG Expected file ${1}/${2}.expected "is empty, ignored." 2>&1 | tee -a $TESTLOG
        else #expected file is missing altogether. 
          echo $TAG Expected file ${1}/${2}.expected is missing. 2>&1 | tee -a $TESTLOG
          ESALL=1
        fi
      fi
    }

STARTDIR=`pwd` # To return to at end.
if [ "${2}%" = "%" ] ; then DIR="."; else DIR="${2}"; fi
TESTDIR="`abspath $DIR`"
TESTLOG="${TESTDIR}/tmptestlog" # Will rename at end.
rm -f $TESTLOG # Just in case. 
rm -f ${TESTDIR}/SUCCEEDED ${TESTDIR}/FAILED ${TESTDIR}/NOTRUN
echo $TAG "Test directory is $TESTDIR" | tee -a $TESTLOG

# Does $TESTDIR/run.sh contain a unique test script for this test. 
cd $TESTDIR
if test -f run.sh 
then # run it with same arg.
  ./run.sh $ACTION 
else # Do this script.
  TAG="script: "
  checkarg  
  if [ $ESALL -ne 0 ]; then exit 1; fi

  SRCDIR="$TESTDIR/src"
  BUILDDIR="$TESTDIR/build"

  # Text attributes:
  # These are not being interpreted: 
  #RED="\\033[91m"
  #GREEN="\\033[92m"
  #ORANGE="\\033[93m"
  #PLAIN="\\033[0m"

  # Optional file srcdirs lists directories to look for source files in.
  if test -f srcdirs ; then SRCDIRS=`cat srcdirs`; else SRCDIRS="$SRCDIR"; fi

  # Optional file sources lists source file simple names for the compile command.
  if test -f sources ; then SOURCES=`cat sources`; else SOURCES="Main.m3"; fi

  # Optional file imports list source file simple names that will be compiled
  # due to IMPORT and EXPORTS. Sheck tem but do not add or update.
  if test -f imports ; then IMPORTS=`cat imports`; else IMPORTS="Main.i3"; fi
  
  ALLSRCS="$SOURCES $IMPORTS"

  # Optional file streams contains name suffixes of intermediate stream files.
  if test -f streams
  then STREAMS=`cat streams`
  else STREAMS="FM3Pass1 FM3Pass2"
  fi

  # Optional file exprs contains name suffixes of data structure dumps files.
  if test -f exprs ; then EXPRS=`cat exprs`; else EXPRS="FM3Pass2.Exprs"; fi

  # Maybe run the compiler.
  compile

  # FM3Log
    checkOneFile ${SRCDIR} FM3Log
    updateOneFile ${SRCDIR} FM3Log
    addOneFile ${SRCDIR} FM3Log
    
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
    addOneFile ${SRCDIR} ${SRC}.Log
    addOneFile ${SRCDIR} ${SRC}.Log.expected
  done

  # Handle built files.
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
  echo $TAG $STARTDIR
  cd $STARTDIR
  echo $TAG "Last action was" $ACTION '.' | tee -a $TESTLOG
  if [ $docheck = 0 ] && [ $ESALL -ne 0 ] 
  then # Overall failure
    touch ${TESTDIR}/FAILED 
    echo $TAG $RED "################ F A I L E D ################" $PLAIN | tee -a $TESTLOG
    echo $TAG "---------------- Failed in `pwd`" | tee -a $TESTLOG
    mv $TESTLOG $TESTDIR/testlog #Overlaying the old one. 
    exit 1
  else # Overall success
    touch ${TESTDIR}/SUCCEEDED
    touch ${TESTDIR}/LASTSUCCEEDED
    echo $TAG $GREEN "---------------- Succeeded in `pwd`" $PLAIN | tee -a $TESTLOG
    mv $TESTLOG $TESTDIR/testlog #Overlaying the old one. 
  fi
fi 
