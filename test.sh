#!/bin/bash

function diffokifempty {
    DIFF=$(diff - $1)

    if [ "$DIFF" != "" ]
    then
        echo "FAIL"
        echo "$DIFF"
    else
        echo "OK"
    fi
}

function failifnotok {
    LAST=$(tail -n 1)
    
    if [[ "$LAST" != "OK"* ]]
    then
        echo "FAIL"
    else
        echo "OK"
    fi
}

#Failing tests: semantic checks, runtime errors and so forth. 
echo "==Failing tests=="

echo
echo "Static semantic checks."

dist/build/imp/imp -e -check        "test/fails/extra_args.imp" | failifnotok
dist/build/imp/imp -e -check        "test/fails/fewer_args.imp" | failifnotok
dist/build/imp/imp -e -check        "test/fails/func_args_num.imp" | failifnotok
dist/build/imp/imp -e -check        "test/fails/func_dupe_var.imp" | failifnotok
dist/build/imp/imp -e -check        "test/fails/func_redef.imp" | failifnotok
dist/build/imp/imp -e -check        "test/fails/func_undef.imp" | failifnotok
dist/build/imp/imp -e -check        "test/fails/nomain.imp" | failifnotok
dist/build/imp/imp -e -check        "test/fails/unreachable.imp" | failifnotok
dist/build/imp/imp -e -check        "test/fails/var_undef.imp" | failifnotok

echo
echo "Runtime checks"
echo "Source Code"

dist/build/imp/imp -e -interpret    "test/fails/extra_args.imp" | diffokifempty "test/fails/extra_args.imp.int"
dist/build/imp/imp -e -interpret    "test/fails/fewer_args.imp" | diffokifempty "test/fails/fewer_args.imp.int"
dist/build/imp/imp -e -interpret    "test/fails/func_args_num.imp" | diffokifempty "test/fails/func_args_num.imp.int"
dist/build/imp/imp -e -interpret    "test/fails/func_redef.imp" | diffokifempty "test/fails/func_redef.imp.int"
dist/build/imp/imp -e -interpret    "test/fails/func_undef.imp" | diffokifempty "test/fails/func_undef.imp.int"
dist/build/imp/imp -e -interpret    "test/fails/nomain.imp" | diffokifempty "test/fails/nomain.imp.int"
dist/build/imp/imp -e -interpret    "test/fails/unreachable.imp" | diffokifempty "test/fails/unreachable.imp.int"
dist/build/imp/imp -e -interpret    "test/fails/var_undef.imp" | diffokifempty "test/fails/var_undef.imp.int"

echo
echo "IR Code"
# More runtime checks; of intermediate code this time.
dist/build/imp/imp -e -interpret    "test/fails/ir/missblock.ir" | diffokifempty "test/fails/ir/missblock.ir.int"
dist/build/imp/imp -e -interpret    "test/fails/ir/missfunc.ir" | diffokifempty "test/fails/ir/missfunc.ir.int"
dist/build/imp/imp -e -interpret    "test/fails/ir/missreg.ir" | diffokifempty "test/fails/ir/missreg.ir.int"
dist/build/imp/imp -e -interpret    "test/fails/ir/missvar.ir" | diffokifempty "test/fails/ir/missvar.ir.int"
dist/build/imp/imp -e -interpret    "test/fails/ir/nomain.ir" | diffokifempty "test/fails/ir/nomain.ir.int"
dist/build/imp/imp -e -interpret    "test/fails/ir/wrongargs.ir" | diffokifempty "test/fails/ir/wrongargs.ir.int"


echo
echo
echo "==Passing Tests=="
#Passing tests


echo "Assignment"
# Testing different forms of assignment
dist/build/imp/imp -e -check        "test/works/assignment/assignments.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/assignment/polyassign.imp" | failifnotok

dist/build/imp/imp -e -interpret    "test/works/assignment/assignments.imp" | diffokifempty "test/works/assignment/assignments.imp.int"
dist/build/imp/imp -e -interpret    "test/works/assignment/polyassign.imp" | diffokifempty "test/works/assignment/polyassign.imp.int"


echo
echo "Library"
# Library importation.
dist/build/imp/imp -e -convert      "test/works/include/include_chain.imp" | failifnotok
dist/build/imp/imp -e -parse        "test/works/include/include_chain.imp" | failifnotok

dist/build/imp/imp -e -check        "test/works/include/include_chain.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/include/include_lib.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/include/include_local.imp" | failifnotok

dist/build/imp/imp -e -interpret    "test/works/include/include_chain.imp" | diffokifempty "test/works/include/include_chain.imp.int"
dist/build/imp/imp -e -interpret    "test/works/include/include_lib.imp" | diffokifempty "test/works/include/include_lib.imp.int"
dist/build/imp/imp -e -interpret    "test/works/include/include_local.imp" | diffokifempty "test/works/include/include_local.imp.int"


echo
echo "IR Language"
# Intermediate language conversion/interpretation tests.
dist/build/imp/imp -e -lex          "test/works/ir/fact_orig.ir" | failifnotok

dist/build/imp/imp -e -interpret    "test/works/ir/fact_orig.ir" 10 | diffokifempty "test/works/ir/fact_orig.ir.int"
dist/build/imp/imp -e -interpret    "test/works/ir/num_demo.ir" | diffokifempty "test/works/ir/num_demo.ir.int"


echo
echo "Operators"
# Testing some of the operators/precedence rules.
dist/build/imp/imp -e -check        "test/works/operators/arithmetic.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/operators/funcop.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/operators/logic_test.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/operators/precedence.imp" | failifnotok

dist/build/imp/imp -e -parse        "test/works/operators/arithmetic.imp" | failifnotok
dist/build/imp/imp -e -parse        "test/works/operators/funcop.imp" | failifnotok
dist/build/imp/imp -e -parse        "test/works/operators/logic_test.imp" | failifnotok
dist/build/imp/imp -e -parse        "test/works/operators/precedence.imp" | failifnotok

dist/build/imp/imp -e -interpret    "test/works/operators/arithmetic.imp" | diffokifempty "test/works/operators/arithmetic.imp.int"
dist/build/imp/imp -e -interpret    "test/works/operators/funcop.imp" | diffokifempty "test/works/operators/funcop.imp.int"
dist/build/imp/imp -e -interpret    "test/works/operators/logic_test.imp" 2 0 | diffokifempty "test/works/operators/logic_test.imp.int"
dist/build/imp/imp -e -interpret    "test/works/operators/precedence.imp" | diffokifempty "test/works/operators/precedence.imp.int"


echo
echo "Semantics"
# Programs to test language features. 
dist/build/imp/imp -e -check        "test/works/semantics/factorial.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/semantics/iter_factorial.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/semantics/iter_fibonacci.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/semantics/thenless.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/semantics/zero.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/sorts/bubble_sort6.imp" | failifnotok

dist/build/imp/imp -e -interpret    "test/works/semantics/factorial.imp" 10 | diffokifempty "test/works/semantics/factorial.imp.int"
dist/build/imp/imp -e -interpret    "test/works/semantics/iter_factorial.imp" 10 | diffokifempty "test/works/semantics/iter_factorial.imp.int"
dist/build/imp/imp -e -interpret    "test/works/semantics/iter_fibonacci.imp" 10 | diffokifempty "test/works/semantics/iter_fibonacci.imp.int"
dist/build/imp/imp -e -interpret    "test/works/semantics/thenless.imp" | diffokifempty "test/works/semantics/thenless.imp.int"
dist/build/imp/imp -e -interpret    "test/works/semantics/zero.imp" | diffokifempty "test/works/semantics/zero.imp.int"
dist/build/imp/imp -e -interpret    "test/works/sorts/bubble_sort6.imp" 5 4 3 2 1 | diffokifempty "test/works/sorts/bubble_sort6.imp.int"

echo
echo "Unextended"
# Tests of the language without extended features
dist/build/imp/imp -lex             "test/works/unextended/fact_orig.imp" | failifnotok
dist/build/imp/imp -parse           "test/works/unextended/fact_orig.imp" | failifnotok
dist/build/imp/imp -parse           "test/works/unextended/zero.imp" | failifnotok
dist/build/imp/imp -check           "test/works/unextended/fact_orig.imp" | failifnotok
dist/build/imp/imp -convert         "test/works/unextended/fact_orig.imp" | failifnotok

dist/build/imp/imp -interpret       "test/works/unextended/fact_orig.imp" 10 | failifnotok
dist/build/imp/imp -interpret       "test/works/factorial_original.ir" 10 | failifnotok
dist/build/imp/imp -interpret       "test/works/unextended/uninitialised.imp" | failifnotok
dist/build/imp/imp -interpret       "test/works/unextended/zero.imp" | failifnotok

echo
echo "Miscellaneous"
# General stress tests.
dist/build/imp/imp -e -interpret    "test/works/min_iter_factorial.imp" 10 | failifnotok
dist/build/imp/imp -e -interpret    "test/works/modulo_demo.imp" | diffokifempty "test/works/modulo_demo.imp.int"
dist/build/imp/imp -e -interpret    "test/works/num_demo.imp" | diffokifempty "test/works/num_demo.imp.int"
dist/build/imp/imp -e -interpret    "test/works/primelist.imp" 10 | diffokifempty "test/works/primelist.imp.int"

dist/build/imp/imp -e -interpret    "test/works/not.imp" | failifnotok
dist/build/imp/imp -e -check        "test/works/not.imp" | failifnotok
dist/build/imp/imp -e -convert      "test/works/not.imp" | failifnotok
dist/build/imp/imp -e -lex          "test/works/not.imp" | failifnotok
dist/build/imp/imp -e -parse        "test/works/not.imp" | failifnotok

