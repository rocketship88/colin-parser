proc transform= {arglist body {inproc 0} {preserve 1}} {
    set result {}
    set lines [split $body \n]
    set i 0
    while {$i < [llength $lines]} {
        set line [lindex $lines $i]
        if {[regexp {^\s*:\s*\{} $line]} {
            # path 1: : {...} possibly multiline
            set accum $line
            while {![info complete $accum]} {
                incr i
                append accum \n [lindex $lines $i]
            }
            # extract expression - strip leading : and outer { }
            regexp {:\s*(\{.*\})} $accum -> braced
            set expr [string range $braced 1 end-1]
            regsub {\s*;#[^\n]*$} $expr {} expr
            set tal [::Calc::compile0 $expr]
            if {$inproc} {
                regsub -all {push ([[:alpha:]:][\w:]*); loadStk} $tal {load \1} tal
            }
            if {$preserve} {
                append result "tcl::unsupported::assemble \{$tal\} ; if \{0\} \{ $accum \}\n"
            } else {
                append result "tcl::unsupported::assemble \{$tal\}\n"
            }

        } elseif {1 && [regexp {^\s*:\s*(\S[^\n]*)} $line -> expr]} {
            # path 2: : expr  single line unbraced
            regsub {\s*;#[^\n]*$} $expr {} expr
            set tal [::Calc::compile0 $expr]
            if {$inproc} {
                regsub -all {push ([[:alpha:]:][\w:]*); loadStk} $tal {load \1} tal
            }
            if {$preserve} {
                append result "tcl::unsupported::assemble \{$tal\} ; if \{0\} \{ $line \}\n"
            } else {
                append result "tcl::unsupported::assemble \{$tal\}\n"
            }

        } elseif {1 && [regexp {\[:\s*[^\]]+\]} $line]} {
            # path 3: inline [: expr] replacement - one for one substitution
            set newline {}
            set pos 0
            while {[regexp -indices -start $pos {\[:\s*([^\]]+)\]} $line match submatch]} {
                # append everything before this match unchanged
                append newline [string range $line $pos [expr {[lindex $match 0]-1}]]
                # extract and compile the expression
                set expr [string range $line {*}$submatch]
                set tal [::Calc::compile0 $expr]
                if {$inproc} {
                    regsub -all {push ([[:alpha:]:][\w:]*); loadStk} $tal {load \1} tal
                }
                append newline "\[tcl::unsupported::assemble \{$tal\}\]"
                set pos [expr {[lindex $match 1]+1}]
            }
            # append remainder of line after last match
            append newline [string range $line $pos end]
            append result $newline \n

        } else {
            # passthrough
            append result $line \n
        }
        incr i
    }
    return $result
}

proc proc= {name arglist body {preserve 1}} {
    proc $name $arglist [transform= $arglist $body 1 $preserve]
}

