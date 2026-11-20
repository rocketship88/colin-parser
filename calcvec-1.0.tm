# calcvec: vector operators for Calc.   package require calcvec
#   \u2299 / :dot:      dot product of lists of any length (compiled in place as a loop)
#   :dot3:            dot product of 3 element lists, compiled in place with no loop
#   \u00d7 / :cross:  cross product of two 3 element lists, compiled in place with no loop
#   :vadd: :vsub:    sum and difference of two lists of the same length, at the level of + and -
#   \u22a0 / :mmul:  matrix product      \u2297 / :outer:  outer product
# The symbols are written as backslash-u escapes below, so this file is pure ASCII.
package require colon
package provide calcvec 1.0

proc ::tcl::mathop::dot {u v} {
    if {[llength $u] != [llength $v]} {
        error "dot: the lists have different lengths, [llength $u] and [llength $v]"
    }
    set sum 0
    foreach a $u b $v { set sum [expr {$sum + $a * $b}] }
    return $sum
}

proc ::tcl::mathop::cross {u v} {
    lassign $u a b c
    lassign $v x y z
    list [expr {$b*$z - $c*$y}] [expr {$c*$x - $a*$z}] [expr {$a*$y - $b*$x}]
}

# Stack, top first: sum i n v u.  i counts up from 0 and the loop stops when i reaches n, the length,
# so i takes the values 0 to n-1, and the terms are added in list order.
Calc::defineOperator \u2299 -name dot -prec 11 -spelled :dot: -inline {
    over 1; listLength; over 1; listLength; neq; jumpTrue dotbad@;
    over 1; listLength; push 0; push 0;
    label dotloop@; over 1; over 3; lt; jumpFalse dotdone@;
    over 4; over 2; listIndex; over 4; over 3; listIndex; mult; add;
    reverse 2; push 1; add; reverse 2; jump dotloop@;
    label dotdone@; reverse 5; pop; pop; pop; pop; jump dotend@;
    label dotbad@; push error; push {dot: the lists have different lengths, };
    over 3; listLength; push { and }; over 4; listLength; strcat 4; invokeStk 2; pop; pop;
    label dotend@;
}

# Fixed size, 3 elements, no loop.  The stack holds u v, plus d values built so far above them.
namespace eval calcvec {}
proc calcvec::el {which i d} {
    # a copy of element i of u (which 0) or of v (which 1): u is d+1 down from the top, v is d down
    return "over [expr {$d + 1 - $which}]; push $i; listIndex; "
}
proc calcvec::dot3code {} {
    set c ""
    set d 0
    foreach i {0 1 2} {
        append c [el 0 $i $d] [el 1 $i [expr {$d + 1}]] "mult; "
        incr d
        if {$i > 0} { append c "add; "; incr d -1 }
    }
    append c "reverse 3; pop; pop; "
    return $c
}
proc calcvec::cross3code {} {
    set c ""
    set d 0
    foreach {a b} {1 2  2 0  0 1} {
        # u[a]*v[b] - u[b]*v[a]
        append c [el 0 $a $d] [el 1 $b [expr {$d + 1}]] "mult; "
        append c [el 0 $b [expr {$d + 1}]] [el 1 $a [expr {$d + 2}]] "mult; sub; "
        incr d
    }
    append c "list 3; reverse 3; pop; pop; "
    return $c
}
Calc::defineOperator :dot3: -name dot -prec 11 -inline [calcvec::dot3code]
Calc::defineOperator \u00d7 -name cross -prec 11 -spelled :cross: -inline [calcvec::cross3code]

# Matrix product.  A is m rows of n numbers, B is n rows of p numbers; the result is m rows of p.
proc ::tcl::mathop::mmul {A B} {
    set n [llength $B]
    set cols {}
    for {set j 0} {$j < [llength [lindex $B 0]]} {incr j} {
        lappend cols [lmap row $B {lindex $row $j}]
    }
    lmap r $A {
        if {[llength $r] != $n} {
            error "mmul: a row of the first matrix has [llength $r] numbers but the second has $n rows"
        }
        lmap c $cols {
            set sum 0
            foreach a $r b $c { set sum [expr {$sum + $a * $b}] }
            set sum
        }
    }
}
proc ::tcl::mathop::outer {u v} {
    lmap a $u { lmap b $v { expr {$a * $b} } }
}
Calc::defineOperator \u22a0 -name mmul -prec 11 -spelled :mmul:
Calc::defineOperator \u2297 -name outer -prec 11 -spelled :outer:

# Vector sum and difference, element by element.  They sit at the level of + and - (10), so
# a :vadd: b :vsub: c is (a + b) - c, and a cross or dot product is done first.
proc ::tcl::mathop::vadd {u v} {
    if {[llength $u] != [llength $v]} {
        error "vadd: the lists have different lengths, [llength $u] and [llength $v]"
    }
    lmap a $u b $v { expr {$a + $b} }
}
proc ::tcl::mathop::vsub {u v} {
    if {[llength $u] != [llength $v]} {
        error "vsub: the lists have different lengths, [llength $u] and [llength $v]"
    }
    lmap a $u b $v { expr {$a - $b} }
}
Calc::defineOperator :vadd: -name vadd -prec 10
Calc::defineOperator :vsub: -name vsub -prec 10
