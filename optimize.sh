#!/bin/bash

if [ $# -eq 0 ]
  then
    echo "== Available Optimisations ==" 
    echo "-unreach  ;  Removes unreachable blocks."
    echo "-dead     ;  Removes dead code."
    echo "-mutate   ;  Redundant instruction modification."
    echo "-merge    ;  Merge and delete blocks."
    echo "-fix      ;  Perform all optimisations until stable."
  else
    if [ $# -gt 1 ]
      then
        arg1="-all"
        if [[ "$3" != "" ]] 
          then
            arg1="$3"
        fi
        cmd="dist/build/imp/imp $arg1 $1 > $2"
        eval $cmd
    fi
fi

