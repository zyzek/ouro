#!/bin/bash

dist/build/imp/imp -e -check 		"test/works/assignment/assignments.imp"
dist/build/imp/imp -e -interpret 	"test/works/assignment/assignments.imp"

dist/build/imp/imp -e -check 		"test/works/assignment/polyassign.imp"
dist/build/imp/imp -e -interpret 	"test/works/assignment/polyassign.imp"

dist/build/imp/imp -e -check 		"test/works/include/include_chain.imp"
dist/build/imp/imp -e -interpret 	"test/works/include/include_chain.imp"

dist/build/imp/imp -e -check 		"test/works/include/include_lib.imp"
dist/build/imp/imp -e -interpret 	"test/works/include/include_lib.imp"

dist/build/imp/imp -e -check 		"test/works/include/include_local.imp"
dist/build/imp/imp -e -interpret 	"test/works/include/include_local.imp"

dist/build/imp/imp -e -check 		"test/works/include/multincluded.imp"
dist/build/imp/imp -e -interpret 	"test/works/include/multincluded.imp"

dist/build/imp/imp -e -interpret 	"test/works/ir/fact_orig.ir" 10

dist/build/imp/imp -e -interpret 	"test/works/ir/num_demo.ir"

dist/build/imp/imp -e -interpret 	"test/works/ir/numbers.ir"

dist/build/imp/imp -e -check 		"test/fails/extra_args.imp"
dist/build/imp/imp -e -interpret 	"test/fails/extra_args.imp"

dist/build/imp/imp -e -check 		"test/fails/fewer_args.imp"
dist/build/imp/imp -e -interpret 	"test/fails/fewer_args.imp"

dist/build/imp/imp -e -check 		"test/fails/func_args_num.imp"
dist/build/imp/imp -e -interpret 	"test/fails/func_args_num.imp"

dist/build/imp/imp -e -check 		"test/fails/func_dupe_var.imp"

dist/build/imp/imp -e -check 		"test/fails/func_redef.imp"
dist/build/imp/imp -e -interpret 	"test/fails/func_redef.imp"

dist/build/imp/imp -e -check 		"test/fails/func_undef.imp"
dist/build/imp/imp -e -interpret 	"test/fails/func_undef.imp"

dist/build/imp/imp -e -check 		"test/fails/nomain.imp"
dist/build/imp/imp -e -interpret 	"test/fails/nomain.imp"

dist/build/imp/imp -e -check 		"test/fails/unreachable.imp"
dist/build/imp/imp -e -interpret 	"test/fails/unreachable.imp"

dist/build/imp/imp -e -check 		"test/fails/var_undef.imp"
dist/build/imp/imp -e -interpret 	"test/fails/var_undef.imp"

dist/build/imp/imp -e -interpret 	"test/fails/ir/missblock.imp"
dist/build/imp/imp -e -interpret 	"test/fails/ir/missfunc.imp"
dist/build/imp/imp -e -interpret 	"test/fails/ir/missreg.imp"
dist/build/imp/imp -e -interpret 	"test/fails/ir/missvar.imp"
dist/build/imp/imp -e -interpret 	"test/fails/ir/nomain.imp"
dist/build/imp/imp -e -interpret 	"test/fails/ir/wrongargs.imp"

dist/build/imp/imp -e -check 		"test/works/operators/arithmetic.imp"
dist/build/imp/imp -e -interpret 	"test/works/operators/arithmetic.imp"

dist/build/imp/imp -e -check 		"test/works/operators/funcop.imp"
dist/build/imp/imp -e -interpret 	"test/works/operators/funcop.imp"

dist/build/imp/imp -e -check 		"test/works/operators/logic_test.imp"
dist/build/imp/imp -e -interpret 	"test/works/operators/logic_test.imp" 2 0

dist/build/imp/imp -e -check 		"test/works/operators/precedence.imp"
dist/build/imp/imp -e -interpret 	"test/works/operators/precedence.imp"

dist/build/imp/imp -e -check 		"test/works/semantics/factorial.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/factorial.imp" 10

dist/build/imp/imp -e -check 		"test/works/semantics/iter_factorial.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/iter_factorial.imp" 10

dist/build/imp/imp -e -check 		"test/works/semantics/iter_fibonacci.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/iter_fibonacci.imp" 10

dist/build/imp/imp -e -check 		"test/works/semantics/thenless.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/thenless.imp"

dist/build/imp/imp -e -check 		"test/works/semantics/zero.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/zero.imp"

dist/build/imp/imp -e -check 		"test/works/sorts/bubble_sort6.imp"
dist/build/imp/imp -e -interpret 	"test/works/sorts/bubble_sort6.imp" 5 4 3 2 1

dist/build/imp/imp -e -check 		"test/works/sorts/merge_sort4.imp"
dist/build/imp/imp -e -interpret 	"test/works/sorts/merge_sort4.imp" 1 7 9 5

dist/build/imp/imp -e -lex 			"test/works/unextended/fact_orig.imp"
dist/build/imp/imp -e -parse 		"test/works/unextended/fact_orig.imp"

dist/build/imp/imp -e -interpret	"test/works/unextended/uninitialised.imp"
dist/build/imp/imp -e -interpret	"test/works/factorial_original.ir" 10

dist/build/imp/imp -e -lex 			"test/works/fact_orig.imp"
dist/build/imp/imp -e -parse 		"test/works/fac_orig.imp"







dist\build\imp\imp -e -interpret 	"test/works/ir/fact_orig.ir" 10	 > "test/works/ir/fact_orig.ir.interpret"
dist\build\imp\imp -e -interpret 	"test/works/operators/logic_test.imp" 2 0 > "test/works/operators/logic_test.imp.interpret"
dist\build\imp\imp -e -interpret 	"test/works/semantics/factorial.imp" 10 > "test/works/semantics/factorial.imp.interpret"   	
dist\build\imp\imp -e -interpret 	"test/works/semantics/iter_factorial.imp" 10 > "test/works/semantics/iter_factorial.imp.interpret"
dist\build\imp\imp -e -interpret 	"test/works/semantics/iter_fibonacci.imp" 10 > "test/works/semantics/iter_fibonacci.imp.interpret"

dist\build\imp\imp -e -interpret 	"test/works/sorts/bubble_sort6.imp" 5 4 3 2 1 > "test/works/sorts/bubble_sort6.imp.interpret"
dist\build\imp\imp -e -interpret 	"test/works/sorts/merge_sort4.imp" 1 7 9 5 > "test/works/sorts/bubble_sort6.imp.interpret"
dist\build\imp\imp -e -interpret	"test/works/factorial_original.ir" 10 > "test/works/factorial_original.ir.interpret"


dist\build\imp\imp -e -interpret 	"test/works/min_iter_factorial.imp" > "test/works/min_iter_factorial.imp.interpret"
dist\build\imp\imp -e -check 		"test/works/min_iter_factorial.imp" > "test/works/min_iter_factorial.imp.check"

dist\build\imp\imp -e -convert 		"test/works/multincluded.imp" 		> "test/works/multincluded.imp.convert"
dist\build\imp\imp -e -parse 		"test/works/multincluded.imp" 		> "test/works/multincluded.imp.parse"
dist\build\imp\imp -e -lex 			"test/works/numbers.ir" 			> "test/works/numbers.ir.lex"
