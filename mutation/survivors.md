# Surviving mutants (110 of 1168)

Each mutant below was changed in `colon-1.0.tm` and the tests did not notice, on Tcl 8.6.14 or on
9.1.1 (a mutant that survives on only one of them is marked). I read the code around every one and
judged it equivalent (same behavior), dead code, or only reachable through input where the module
drops text (the last group, where a test would fix a bug in place). These are judgments from reading,
not proofs. Numbers are mutant ids in `mutants.json`, `results.csv` and `results-9.1.csv`; `L` is the
line in `colon-1.0.tm`.

## Debug-only depth counter (nothing reads it)

- #148  L161  delete: variable depth   `variable depth`
- #157  L168  0 -> 1   `set depth 0`
- #158  L168  delete: set depth 0   `set depth 0`
- #358  L334  delete: variable depth   `variable depth`
- #365  L340  incr depth -> incr depth -1   `set dep [incr depth]`
- #369  L346  delete: set depth $dep   `set depth $dep`
- #451  L420  1 -> 2   `set depth [expr {$dep - 1}]`
- #452  L420  1 -> 0   `set depth [expr {$dep - 1}]`
- #453  L420  delete: set depth [expr {$dep - 1}]   `set depth [expr {$dep - 1}]`
- #460  L436  delete: variable depth   `variable depth`
- #467  L444  delete: set dep [incr depth]   `set dep [incr depth]`
- #468  L444  incr depth -> incr depth -1   `set dep [incr depth]`
- #789  L693  delete: variable depth   `variable depth`
- #792  L697  delete: set dep [incr depth]   `set dep [incr depth]`
- #793  L697  incr depth -> incr depth -1   `set dep [incr depth]`
- #835  L759  delete: variable depth   `variable depth`
- #839  L763  delete: set dep [incr depth]   `set dep [incr depth]`
- #840  L763  incr depth -> incr depth -1   `set dep [incr depth]`

## Variable set to empty before use, but append/lappend creates it anyway

- #47  L72  delete: set bytecode ""   `set bytecode ""`
- #90  L118  delete: set output {}   `set output {}`
- #119  L141  delete: set merged {}   `set merged {}`
- #296  L270  delete: set alts {}   `set alts {}`
- #684  L629  delete: set kids {}   `set kids {}`
- #709  L648  delete: set parts {}   `set parts {}`
- #728  L662  delete: set opcodes ""   `set opcodes ""`
- #856  L798  delete: set result {}   `set result {}`
- #891  L827  delete: set rest {}   `set rest {}`
- #897  L832  delete: set rest {}   `set rest {}`
- #987  L918  delete: set newline {}   `set newline {}`
- #1024  L1012  delete: set out {}   `set out {}`
- #1097  L1111  delete: set touched {}   `set touched {}`

## Flag or loop value changed from 1 to 2 (still true), or a default no caller relies on

- #106  L127  1 -> 2   `set needsMerge 1`
- #402  L378  1 -> 2   `set strictFlag 1`
- #685  L630  1 -> 2   `while 1 {`
- #798  L701  1 -> 2   `while 1 {`
- #854  L797  1 -> 2   `proc transform= {arglist body {inproc 0} {preserve 1}} {`
- #879  L821  1 -> 2   `while {1} {`
- #1062  L1052  1 -> 2   `set useRegex 1`
- #1068  L1056  1 -> 2   `set listOnly 1`
- #1104  L1125  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`
- #1142  L1167  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`
- #1159  L1183  1 -> 2   `set newbody [::Calc::transform= $arglist $body 1 $preserve]`

## Tokenizer pre-check; the merge pass checks the same condition again

- #91  L119  0 -> 1   `set needsMerge 0`
- #100  L126  0 -> 1   `if {($last eq "e" || $last eq "E") && [string is double -strict "${prev}0"]} {`
- #101  L126  && -> ||   `if {($last eq "e" || $last eq "E") && [string is double -strict "${prev}0"]} {`
- #125  L145  0 -> 1   `if {[regexp {^[0-9]*\.?[0-9]+[eE]$} $tok] &&`

## Precedence numbers whose change does not alter any parse (the grid in calc-45.1 checks every pair)

- #163  L179  0 -> 1   `=  0 -`
- #167  L183  1 -> 0   `:  1 -`
- #226  L202  12 -> 11   `** 12 expon`
- #246  L234  13 -> 14   `set preprec 13`

## Boundary or loop bound that cannot be reached with the current tables

- #518  L484  < -> <=   `for {set j $tokpos} {$j < [llength $tokens]} {incr j} {`
- #533  L488  < -> <=   `if {$j < [llength $tokens] && [lindex $tokens $j+1] eq "="} {`
- #822  L723  0 -> 1   `if {$expectedArgs < 0} {`
- #823  L723  < -> <=   `if {$expectedArgs < 0} {`
- #870  L810  >= -> >   `if {$i >= [llength $lines]} {`
- #949  L878  >= -> >   `if {$i >= [llength $lines]} {`

## No native function takes 0 arguments, so > 0 / >= 0 / < 0 / <= 0 on the expected count make no difference

- #810  L710  0 -> 1   `if {$expectedArgs >= 0} {`
- #811  L710  >= -> >   `if {$expectedArgs >= 0} {`

## Operand parsed at a precedence where a leading name handles = itself

- #540  L492  0 -> 1   `return "[parse 0][destructure $pat]"`
- #542  L494  0 -> 1   `set opcodes [parse 0]`
- #553  L499  0 -> 1   `append opcodes [parse 0]`
- #608  L558  0 -> 1   `append opcodes [parse 0]`
- #623  L569  0 -> 1   `append opcodes [parse 0]`
- #649  L592  0 -> 1   `append opcodes [parse 0]`
- #829  L741  0 -> 1   `append opcodes [parse 0]`

## Redundant or optimisation-only code with the same result

- #45  L67  delete: return $stmtcode   `return $stmtcode`
- #584  L527  delete: set token [lindex $tokens $tokpos]   `set token [lindex $tokens $tokpos]`
- #632  L580  1 -> 0   `if {$indexCount > 1} {`
- #633  L580  > -> >=   `if {$indexCount > 1} {`
- #774  L679  0 -> 1   `} elseif {[lindex $kid 0] eq "n"} {`
- #852  L778  delete: return $opcodes   `return $opcodes`
- #1034  L1019  eq -> ne   `if {$ns eq "::"} {`
- #1035  L1019  negate if condition   `if {$ns eq "::"} {`
- #1036  L1020  delete: lappend out {*}[info procs ::*]   `lappend out {*}[info procs ::*]`
- #1085  L1083  delete: set p "::$p"   `set p "::$p"`

## xstrcat is a disabled experiment (it fails with a TAL error whatever the arity)

- #236  L226  2 -> 3   `xstrcat      {strcat 2}`
- #237  L226  2 -> 1   `xstrcat      {strcat 2}`

## strictFlag is only read after it has been set

- #18  L32  delete: variable strictFlag 0   `variable strictFlag 0`

## Declarations and initial values in the operator tables (created by the first assignment; a flag value of 2 is as true as 1)

- #19  L33  delete: variable curns ::   `variable curns ::   ;# namespace the Calc code being compiled lives in, used to find the procs it calls`
- #284  L258  1 -> 2   `variable inlineOps 1`
- #287  L259  delete: variable opcmd   `variable opcmd`
- #288  L260  delete: variable opinline   `variable opinline`
- #289  L261  delete: variable opspell   `variable opspell`
- #290  L262  delete: array set opcmd {}   `array set opcmd {}`
- #291  L263  delete: array set opinline {}   `array set opinline {}`
- #292  L264  delete: array set opspell {}   `array set opspell {}`

## method= records the namespace ::oo::define, where nothing is ever looked up

- #1154  L1179  1 -> 2   `set ::Calc::curns [uplevel 1 {namespace current}]`
- #1155  L1179  1 -> 0   `set ::Calc::curns [uplevel 1 {namespace current}]`

## Only matters on Tcl 8.6: Tcl 9 reads 1_000 as a number itself

- #487  L460  delete: regsub -all {_} $token {} token   `regsub -all {_} $token {} token`  (9.1 only)

## Closing-bracket fallback: only reached when text sits between } and ], which the code drops (see findings.md)

- #876  L819  0 -> 1   `set open [lindex $bi 0]`
- #882  L822  1 -> 2   `set p [string first \} $accum [expr {$p + 1}]]`
- #883  L822  1 -> 0   `set p [string first \} $accum [expr {$p + 1}]]`
- #884  L823  0 -> 1   `if {$p < 0 || [info complete [string range $accum $open $p]]} break`
- #885  L823  < -> <=   `if {$p < 0 || [info complete [string range $accum $open $p]]} break`
- #888  L825  0 -> 1   `if {$p < 0} {`
- #889  L825  < -> <=   `if {$p < 0} {`
- #893  L830  1 -> 2   `set rest [string range $accum [expr {$p + 1}] end]`
- #898  L834  delete: set rest [string trimleft $rest]   `set rest [string trimleft $rest]`
- #899  L835  0 -> 1   `if {[string index $rest 0] ne ";"} {set rest "; $rest"}`
- #900  L835  ne -> eq   `if {[string index $rest 0] ne ";"} {set rest "; $rest"}`
- #901  L835  negate if condition   `if {[string index $rest 0] ne ";"} {set rest "; $rest"}`
- #970  L901  delete: set suffix ""   `set suffix ""`
- #971  L902  eq -> neq   `while {$suffix eq "" && $i < [llength $lines]} {`
- #972  L902  < -> <=   `while {$suffix eq "" && $i < [llength $lines]} {`
- #973  L902  && -> ||   `while {$suffix eq "" && $i < [llength $lines]} {`
- #974  L902  eq -> ne   `while {$suffix eq "" && $i < [llength $lines]} {`
- #975  L902  negate while condition   `while {$suffix eq "" && $i < [llength $lines]} {`
- #976  L903  delete: incr i   `incr i`
- #977  L903  incr i -> incr i -1   `incr i`
- #978  L904  delete: set suffix [string trimleft [lindex $lines $i]]   `set suffix [string trimleft [lindex $lines $i]]`
