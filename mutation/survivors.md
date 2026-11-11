# Surviving mutants (80 of 1003)

Each mutant below was changed in `colon-1.0.tm` and the tests did not notice. I read the code
around every one and judged it equivalent (same behavior), dead code, or only reachable through
input where the module drops text (the last group, where a test would fix a bug in place). These
are judgments from reading, not proofs. Numbers are mutant ids in `results.csv`; `L` is the line
in `colon-1.0.tm`.

## Debug-only depth counter (nothing reads it)

- #162  L148  delete: variable depth   `variable depth`
- #171  L155  0 -> 1   `set depth 0`
- #172  L155  delete: set depth 0   `set depth 0`
- #279  L226  delete: variable depth   `variable depth`
- #286  L232  incr depth -> incr depth -1   `set dep [incr depth]`
- #290  L238  delete: set depth $dep   `set depth $dep`
- #340  L287  1 -> 2   `set depth [expr {$dep - 1}]`
- #341  L287  1 -> 0   `set depth [expr {$dep - 1}]`
- #342  L287  delete: set depth [expr {$dep - 1}]   `set depth [expr {$dep - 1}]`
- #349  L303  delete: variable depth   `variable depth`
- #355  L310  delete: set dep [incr depth]   `set dep [incr depth]`
- #356  L310  incr depth -> incr depth -1   `set dep [incr depth]`
- #677  L559  delete: variable depth   `variable depth`
- #680  L563  delete: set dep [incr depth]   `set dep [incr depth]`
- #681  L563  incr depth -> incr depth -1   `set dep [incr depth]`
- #721  L625  delete: variable depth   `variable depth`
- #725  L629  delete: set dep [incr depth]   `set dep [incr depth]`
- #726  L629  incr depth -> incr depth -1   `set dep [incr depth]`

## Variable set to empty before use, but append/lappend creates it anyway

- #40  L69  delete: set bytecode ""   `set bytecode ""`
- #106  L107  delete: set output {}   `set output {}`
- #133  L128  delete: set merged {}   `set merged {}`
- #572  L495  delete: set kids {}   `set kids {}`
- #597  L514  delete: set parts {}   `set parts {}`
- #616  L528  delete: set opcodes ""   `set opcodes ""`
- #742  L664  delete: set result {}   `set result {}`
- #835  L755  delete: set newline {}   `set newline {}`
- #872  L849  delete: set out {}   `set out {}`
- #942  L942  delete: set touched {}   `set touched {}`

## Flag or loop value changed from 1 to 2 (still true), or a default no caller relies on

- #321  L270  1 -> 2   `set strictFlag 1`
- #907  L883  1 -> 2   `set useRegex 1`
- #913  L887  1 -> 2   `set listOnly 1`
- #740  L663  1 -> 2   `proc transform= {arglist body {inproc 0} {preserve 1}} {`
- #948  L955  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`
- #980  L995  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`
- #994  L1010  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`
- #122  L116  1 -> 2   `set needsMerge 1`
- #573  L496  1 -> 2   `while 1 {`
- #686  L567  1 -> 2   `while 1 {`

## Tokenizer pre-check; the merge pass checks the same condition again

- #107  L108  0 -> 1   `set needsMerge 0`
- #116  L115  0 -> 1   `if {($last eq "e" || $last eq "E") && [string is double -strict "${prev}0"]} {`
- #117  L115  && -> ||   `if {($last eq "e" || $last eq "E") && [string is double -strict "${prev}0"]} {`
- #139  L132  0 -> 1   `if {[regexp {^[0-9]*\.?[0-9]+[eE]$} $tok] &&`

## Precedence numbers whose change does not alter any parse (the grid in calc-45.1 checks every pair)

- #177  L166  0 -> 1   `=  0 -`
- #178  L169  1 -> 2   `?  1 -`
- #181  L170  1 -> 0   `:  1 -`
- #256  L189  12 -> 11   `** 12 expon`
- #276  L219  13 -> 14   `set preprec 13`

## Boundary or loop bound that cannot be reached with the current tables

- #406  L350  < -> <=   `for {set j $tokpos} {$j < [llength $tokens]} {incr j} {`
- #421  L354  < -> <=   `if {$j < [llength $tokens] && [lindex $tokens $j+1] eq "="} {`
- #699  L576  > -> >=   `if {$expectedArgs > 0} {`
- #708  L589  0 -> 1   `if {$expectedArgs < 0} {`
- #709  L589  < -> <=   `if {$expectedArgs < 0} {`

## Operand parsed at a precedence where a leading name handles = itself

- #428  L358  0 -> 1   `return "[parse 0][destructure $pat]"`
- #441  L365  0 -> 1   `append opcodes [parse 0]`
- #496  L424  0 -> 1   `append opcodes [parse 0]`
- #511  L435  0 -> 1   `append opcodes [parse 0]`
- #715  L607  0 -> 1   `append opcodes [parse 0]`

## Redundant or optimisation-only code with the same result

- #38  L64  delete: return $stmtcode   `return $stmtcode`
- #472  L393  delete: set token [lindex $tokens $tokpos]   `set token [lindex $tokens $tokpos]`
- #738  L644  delete: return $opcodes   `return $opcodes`
- #882  L856  eq -> ne   `if {$ns eq "::"} {`
- #883  L856  negate if condition   `if {$ns eq "::"} {`
- #884  L857  delete: lappend out {*}[info procs ::*]   `lappend out {*}[info procs ::*]`
- #930  L914  delete: set p "::$p"   `set p "::$p"`
- #520  L446  1 -> 0   `if {$indexCount > 1} {`
- #521  L446  > -> >=   `if {$indexCount > 1} {`
- #662  L545  0 -> 1   `} elseif {[lindex $kid 0] eq "n"} {`

## Label names are arbitrary (true_res is renamed to a unique false_res label)

- #337  L281  true_res -> false_res   `set code [string map [list pop_A pop_A$n true_res true_res$n \`

## xstrcat is a disabled experiment (it fails with a TAL error whatever the arity)

- #266  L211  2 -> 3   `xstrcat      {strcat 2}`
- #267  L211  2 -> 1   `xstrcat      {strcat 2}`

## strictFlag is only read after it has been set

- #12  L30  delete: variable strictFlag 0   `variable strictFlag 0`

## Closing-bracket fallback: only reached when text sits between } and ], which the code drops (see findings.md)

- #818  L738  delete: set suffix ""   `set suffix ""`
- #819  L739  eq -> neq   `while {$suffix eq "" && $i < [llength $lines]} {`
- #820  L739  < -> <=   `while {$suffix eq "" && $i < [llength $lines]} {`
- #821  L739  && -> ||   `while {$suffix eq "" && $i < [llength $lines]} {`
- #822  L739  eq -> ne   `while {$suffix eq "" && $i < [llength $lines]} {`
- #823  L739  negate while condition   `while {$suffix eq "" && $i < [llength $lines]} {`
- #824  L740  delete: incr i   `incr i`
- #825  L740  incr i -> incr i -1   `incr i`
- #826  L741  delete: set suffix [string trimleft [lindex $lines $i]]   `set suffix [string trimleft [lindex $lines $i]]`
