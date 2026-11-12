# Surviving mutants (80 of 1002)

Each mutant below was changed in `colon-1.0.tm` and the tests did not notice. I read the code
around every one and judged it equivalent (same behavior), dead code, or only reachable through
input where the module drops text (the last group, where a test would fix a bug in place). These
are judgments from reading, not proofs. Numbers are mutant ids in `results.csv`; `L` is the line
in `colon-1.0.tm`.

## Debug-only depth counter (nothing reads it)

- #162  L148  delete: variable depth   `variable depth`
- #171  L155  0 -> 1   `set depth 0`
- #172  L155  delete: set depth 0   `set depth 0`
- #263  L226  delete: variable depth   `variable depth`
- #270  L232  incr depth -> incr depth -1   `set dep [incr depth]`
- #274  L238  delete: set depth $dep   `set depth $dep`
- #339  L297  1 -> 2   `set depth [expr {$dep - 1}]`
- #340  L297  1 -> 0   `set depth [expr {$dep - 1}]`
- #341  L297  delete: set depth [expr {$dep - 1}]   `set depth [expr {$dep - 1}]`
- #348  L313  delete: variable depth   `variable depth`
- #354  L320  delete: set dep [incr depth]   `set dep [incr depth]`
- #355  L320  incr depth -> incr depth -1   `set dep [incr depth]`
- #676  L569  delete: variable depth   `variable depth`
- #679  L573  delete: set dep [incr depth]   `set dep [incr depth]`
- #680  L573  incr depth -> incr depth -1   `set dep [incr depth]`
- #720  L635  delete: variable depth   `variable depth`
- #724  L639  delete: set dep [incr depth]   `set dep [incr depth]`
- #725  L639  incr depth -> incr depth -1   `set dep [incr depth]`

## Variable set to empty before use, but append/lappend creates it anyway

- #40  L69  delete: set bytecode ""   `set bytecode ""`
- #106  L107  delete: set output {}   `set output {}`
- #133  L128  delete: set merged {}   `set merged {}`
- #571  L505  delete: set kids {}   `set kids {}`
- #596  L524  delete: set parts {}   `set parts {}`
- #615  L538  delete: set opcodes ""   `set opcodes ""`
- #741  L674  delete: set result {}   `set result {}`
- #834  L765  delete: set newline {}   `set newline {}`
- #871  L859  delete: set out {}   `set out {}`
- #941  L952  delete: set touched {}   `set touched {}`

## Flag or loop value changed from 1 to 2 (still true), or a default no caller relies on

- #122  L116  1 -> 2   `set needsMerge 1`
- #307  L270  1 -> 2   `set strictFlag 1`
- #572  L506  1 -> 2   `while 1 {`
- #685  L577  1 -> 2   `while 1 {`
- #739  L673  1 -> 2   `proc transform= {arglist body {inproc 0} {preserve 1}} {`
- #906  L893  1 -> 2   `set useRegex 1`
- #912  L897  1 -> 2   `set listOnly 1`
- #947  L965  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`
- #979  L1005  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`
- #993  L1020  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`

## Tokenizer pre-check; the merge pass checks the same condition again

- #107  L108  0 -> 1   `set needsMerge 0`
- #116  L115  0 -> 1   `if {($last eq "e" || $last eq "E") && [string is double -strict "${prev}0"]} {`
- #117  L115  && -> ||   `if {($last eq "e" || $last eq "E") && [string is double -strict "${prev}0"]} {`
- #139  L132  0 -> 1   `if {[regexp {^[0-9]*\.?[0-9]+[eE]$} $tok] &&`

## Precedence numbers whose change does not alter any parse (the grid in calc-45.1 checks every pair)

- #177  L166  0 -> 1   `=  0 -`
- #181  L170  1 -> 0   `:  1 -`
- #240  L189  12 -> 11   `** 12 expon`
- #260  L219  13 -> 14   `set preprec 13`

## Boundary or loop bound that cannot be reached with the current tables

- #405  L360  < -> <=   `for {set j $tokpos} {$j < [llength $tokens]} {incr j} {`
- #420  L364  < -> <=   `if {$j < [llength $tokens] && [lindex $tokens $j+1] eq "="} {`
- #698  L586  > -> >=   `if {$expectedArgs > 0} {`
- #707  L599  0 -> 1   `if {$expectedArgs < 0} {`
- #708  L599  < -> <=   `if {$expectedArgs < 0} {`

## Operand parsed at a precedence where a leading name handles = itself

- #427  L368  0 -> 1   `return "[parse 0][destructure $pat]"`
- #429  L370  0 -> 1   `set opcodes [parse 0]`
- #440  L375  0 -> 1   `append opcodes [parse 0]`
- #495  L434  0 -> 1   `append opcodes [parse 0]`
- #510  L445  0 -> 1   `append opcodes [parse 0]`
- #536  L468  0 -> 1   `append opcodes [parse 0]`
- #714  L617  0 -> 1   `append opcodes [parse 0]`

## Redundant or optimisation-only code with the same result

- #38  L64  delete: return $stmtcode   `return $stmtcode`
- #471  L403  delete: set token [lindex $tokens $tokpos]   `set token [lindex $tokens $tokpos]`
- #519  L456  1 -> 0   `if {$indexCount > 1} {`
- #520  L456  > -> >=   `if {$indexCount > 1} {`
- #661  L555  0 -> 1   `} elseif {[lindex $kid 0] eq "n"} {`
- #737  L654  delete: return $opcodes   `return $opcodes`
- #881  L866  eq -> ne   `if {$ns eq "::"} {`
- #882  L866  negate if condition   `if {$ns eq "::"} {`
- #883  L867  delete: lappend out {*}[info procs ::*]   `lappend out {*}[info procs ::*]`
- #929  L924  delete: set p "::$p"   `set p "::$p"`

## xstrcat is a disabled experiment (it fails with a TAL error whatever the arity)

- #250  L211  2 -> 3   `xstrcat      {strcat 2}`
- #251  L211  2 -> 1   `xstrcat      {strcat 2}`

## strictFlag is only read after it has been set

- #12  L30  delete: variable strictFlag 0   `variable strictFlag 0`

## Closing-bracket fallback: only reached when text sits between } and ], which the code drops (see findings.md)

- #817  L748  delete: set suffix ""   `set suffix ""`
- #818  L749  eq -> neq   `while {$suffix eq "" && $i < [llength $lines]} {`
- #819  L749  < -> <=   `while {$suffix eq "" && $i < [llength $lines]} {`
- #820  L749  && -> ||   `while {$suffix eq "" && $i < [llength $lines]} {`
- #821  L749  eq -> ne   `while {$suffix eq "" && $i < [llength $lines]} {`
- #822  L749  negate while condition   `while {$suffix eq "" && $i < [llength $lines]} {`
- #823  L750  delete: incr i   `incr i`
- #824  L750  incr i -> incr i -1   `incr i`
- #825  L751  delete: set suffix [string trimleft [lindex $lines $i]]   `set suffix [string trimleft [lindex $lines $i]]`
