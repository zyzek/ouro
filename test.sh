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

dist/build/imp/imp -e -interpret 	"test/works/ir/fact_orig.ir"

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
dist/build/imp/imp -e -interpret 	"test/works/operators/logic_test.imp"

dist/build/imp/imp -e -check 		"test/works/operators/precedence.imp"
dist/build/imp/imp -e -interpret 	"test/works/operators/precedence.imp"

dist/build/imp/imp -e -check 		"test/works/semantics/factorial.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/factorial.imp"

dist/build/imp/imp -e -check 		"test/works/semantics/iter_factorial.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/iter_factorial.imp"

dist/build/imp/imp -e -check 		"test/works/semantics/iter_fibonacci.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/iter_fibonacci.imp"

dist/build/imp/imp -e -check 		"test/works/semantics/thenless.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/thenless.imp"

dist/build/imp/imp -e -check 		"test/works/semantics/zero.imp"
dist/build/imp/imp -e -interpret 	"test/works/semantics/zero.imp"



# dist\build\imp\imp -e -interpret 	"test/works/semantics/zero.imp" > "test/works/semantics/zero.imp.interpret"
# dist\build\imp\imp -e -check 		"test/works/semantics/zero.imp" > "test/works/semantics/zero.imp.check"

# dist\build\imp\imp -e -convert 		"test/works/multincluded.imp" 		> "test/works/multincluded.imp.convert"
# dist\build\imp\imp -e -parse 		"test/works/multincluded.imp" 		> "test/works/multincluded.imp.parse"
# dist\build\imp\imp -e -lex 			"test/works/numbers.ir" 			> "test/works/numbers.ir.lex"
