# Module colon 1.0
#
# make : the command and alias =, or other way around, alias is actually a performance hit
# this version is optimized for either bracing or a single argument, just to avoid the costly join
# which is needed since args could include the actual braces witchh would trigger a syntax error
# and otherwise it's likely more costly to remove the pair with string ops
proc : {arg args} {
    if { [llength $args] != 0 } {
        set arg [join "$arg $args"] 
    }
    if {[info exist ::Calc::cache($arg)]  } {
#        incr ::Calc::cachehits($arg) ;# keep here, who knows, might be faster than being above, needed to actually count hits
        tailcall  ::tcl::unsupported::assemble $::Calc::cache($arg)
    }
    tailcall ::tcl::unsupported::assemble  [set ::Calc::cache($arg) [::Calc::compile0 $arg]]
}
proc = {arg args} {
    if { [llength $args] != 0 } {
        set arg [join "$arg $args"] 
    }
    if { [info exist ::Calc::cache($arg)]  } {
#        incr ::Calc::cachehits($arg) ;# keep here, who knows, might be faster than being above, needed to actually count hits
        tailcall  ::tcl::unsupported::assemble $::Calc::cache($arg)
    }
    tailcall ::tcl::unsupported::assemble  [set ::Calc::cache($arg) [::Calc::compile0 $arg]]
}
#interp alias {} = {} : ;# shorthands 

namespace eval Calc {

# breakup compile into 2 parts to handle ; up front, 
# also allow for '  alias, ' needs no bracing
# but newline does, or needs backslash\n
# multiple ;;; are ignored and treated as one
# comments can be #... or ;# it doesn't matter
# empty lines are also allowed
proc compile0 {exp {inproc 0}} {
    if { [array size ::Calc::cache] > 1000 } {
        error "Calc: Cache exhausted with expression: $exp" ;# this is a programmer error, is using $sub in a Calc expression
    }
    # remove all comments that begin with the # character to the end of the line, but don't consume the newline
    # we can also support ;# since many will be used to that. In addition, the below code works with multiple ;
    regsub -all  {#[^\n]*}  $exp {} exp
#    set exp [string map {"'" ";" "\n" ";"} $exp] ;# this will make \n a statement separator, not using this now
    set exp [string map {"'" ";" "\n" " "} $exp]
    # Fast path: no semicolons
#            return [compile [tokenise $exp]]


    if {[string first ";" $exp] == -1} {
        set stmtcode [compile [tokenise $exp]]
        if {$inproc} {
            if {[regexp {^push ([[:alpha:]:][\w:]*); (.*); storeStk; $} $stmtcode -> name expr]} {
                set stmtcode "$expr; store $name; "
            }
        }
        return $stmtcode
    }
   
    # Has semicolons - split and compile each
    set statements [split $exp ";"]
    set bytecode ""
    set count 0
    foreach stmt $statements {
        set stmt [string trim $stmt]
        if {$stmt eq ""} continue
        
        if {$count > 0} {
            append bytecode "pop; "
        }
        if { $inproc } {
        	set stmtcode [compile [tokenise $stmt]]
        	# peephole: push name; <expr>; storeStk; -> <expr>; store name;
        	if {[regexp {^push ([[:alpha:]:][\w:]*); (.*); storeStk; $} $stmtcode -> name expr]} {
        		set stmtcode "$expr; store $name; "
        	}
        	append bytecode $stmtcode
        } else {
        	append bytecode [compile [tokenise $stmt]]
        }
        
        incr count
    }
    
    return $bytecode
}
#instrument+  Calc::compile0  
proc tokenise input {
    set op_re  {\*\*|%|/|\*|-|\+|>>|<<|>=|<=|>|<|!=|==|=|&&|&|\|\||\||\^|::|:|\?|,|!|~|\(|\)}
    #                                                  ^ added = here (after ==)
    # ... rest stayed the same

    set pos 0
    set output {}
    foreach ind [regexp -indices -all -inline -- $op_re $input] {
        lassign $ind start end
        set prev [string trim [string range $input $pos $start-1]]
        if {$prev ne {}} {lappend output $prev}
        lappend output [string range $input $start $end]
        set pos [expr {$end + 1}]
    }
    set rest [string trim [string range $input $pos end]]
    if {$rest ne {}} {lappend output $rest}
    return $output
}

proc compile toks {
    variable depth
    variable tokens
    variable tokpos

    if {[llength $toks] == 0} {error "Calc: nothing to calculate"}
    set tokens $toks
    set tokpos 0
    set depth 0
    return [parse 0] 
}

# Pratt Parser loosely based on https://www.rosettacode.org/wiki/Arithmetic_evaluation#Nim

# Define infix operators, their precedences and bytecodes
# with chatgpt providing the bytecode to replace the lor/land codes
foreach {op prec code} {
    )  0 -
    ,  0 -
    =  0 -
    ?  1 -
    :  1 -
    || 2 {jumpTrue true_res; jumpFalse pop_A; push 1; jump end_res; label pop_A; push 0; jump end_res; label true_res; pop; push 1; label end_res}
    && 3 {jumpFalse pop_A; jumpFalse false_res; push 1; jump end_res; label pop_A; pop; label false_res; push 0; label end_res}
    |  4 bitor
    ^  5 bitxor
    &  6 bitand
    == 7 eq
    != 7 neq
    <  8 lt
    >  8 gt
    <= 8 le
    >= 8 ge
    << 9 lshift
    >> 9 rshift
    +  10 add
    -  10 sub
    *  11 mult
    /  11 div
    %  11 mod
    ** 12 expon
} {
    set inprec($op) $prec
    set incode($op) $code
}

# Define prefix operators and their bytecodes
foreach {op code} {
    + uplus
    - uminus
    ! not
    ~ bitnot
} {
    set precode($op) $code
}

# our name, vs. inline bytecode name and arg count, negative arg count means variable with minimum abs(count)
array set nativeFunc {
    strlen      {strlen 1}
    llength     {listLength 1}
    lindex		 {lindexMulti -2}
    list        {list -1}
    xstrcat      {strcat 2}
    xtoupper     {strcaseUpper 1}
    xtolower     {strcaseLower 1}
    xstrindex    {strindex 2}
    xstrrange    {strrange 3}
}
    
# Prefix ops all have the same precedence
set preprec 13

# Parse expression until we hit an operator with precedence lower than min_prec.
# The expression is supplied as a list of tokens in the global var tokens.
# The current position in the input is in global var tokpos.
# Returns the TAL bytecode to evaluate the expression.
proc parse min_prec {
    variable depth
    variable incode
    variable inprec
    variable tokens
    variable tokpos
    set token [lindex $tokens $tokpos]
    set dep [incr depth]
    #deb2 "[string repeat {  } $dep]PARSE min_prec=$min_prec tokpos=$tokpos token='$token'"
    incr tokpos
    
    # EXISTING CODE: Normal expression parsing
    set opcodes [parsePrefix $token]
    set depth $dep
    while {$tokpos < [llength $tokens]} {
        
        set token [lindex $tokens $tokpos]
        if {[info exists inprec($token)]} {
            set tok_prec $inprec($token)
        } else {
            error "Calc: expected operator but found '$token'"
        }
        #deb2 "[string repeat {  } $dep]PARSE token=$token tok_prec=$tok_prec"
        if {$tok_prec < $min_prec} {
            break
        }
    # Binary ops are left-associative except for **
        if {$tok_prec == $min_prec && $token ne "**"} {
            break
        }
        # if-then-else needs special handling
        incr tokpos
        if {$token eq "?"} {
            append opcodes [parseTernary]
            continue
    }
        # Infix operator
        append opcodes [parse $tok_prec] "$incode($token); "
    }
    #deb2 "[string repeat {  } $dep]PARSE opcodes='$opcodes'"
    set depth [expr {$dep - 1}]
    return $opcodes
}

#instrument+  parse  
# Parse expression up to the first operator at the same level of parentheses.
# Returns the bytecode to evaluate the subexpression.
proc parsePrefix token {
    variable depth
    variable precode
    variable preprec
    variable tokens
    variable tokpos
    variable nativeFunc
 
    set dep [incr depth]
    #deb2 "[string repeat {  } $dep]PARSEPREFIX token`$token` tokpos=$tokpos"
    
    # Is it a number? In C might use Tcl_GetNumberFromObj() here
    # remove underscores from numbers so works in 8.6, normalize 0dxxx numbers also
    if {[string is digit -strict [string index $token 0]]} {
        regsub -all {_} $token {} token
        regsub {^0[dD]} $token {} token
    }
    if {[string is entier $token] || [string is double $token]} {
	    set nexttok [lindex $tokens $tokpos]
	    if {$nexttok eq "="} {
	        error "Calc: cannot assign to number literal '$token'"
	    }
        return "push $token; "
    }
    
    # Is it boolean? In C might use Tcl_GetBoolean() here
    if {$token eq "false" || $token eq "true"} {
	    set nexttok [lindex $tokens $tokpos]
	    if {$nexttok eq "="} {
	        error "Calc: cannot assign to boolean literal '$token'"
	    }
        return "push $token; "
    }
    
    # Parenthesised subexpression?
    if {$token eq "("} {
        set opcodes [parse 0]
        set token [lindex $tokens $tokpos]
        if {$token eq ")"} {
            incr tokpos
            return $opcodes
        }
        error "Calc: expected ')' but found '$token'"
    }
    
    # Unary operator?
    if {$token in {+ - ! ~}} {
        return "[parse $preprec]$precode($token); "
    }
    
    # Try to build a name (may include ::)
    set name ""
    while {$token eq {::} || [regexp {^[[:alpha:]_][[:alnum:]_]*$} $token]} {
        append name $token
        set token [lindex $tokens $tokpos]
        incr tokpos
    }
    
    if {$name ne {}} {
        # We have a name, check what follows
        incr tokpos -1
        set token [lindex $tokens $tokpos]
        
        # Is it followed by (? Then it's a function or array
        if {$token eq "("} {
            incr tokpos  ;# Skip the (
            
            # Check if it's a native function FIRST
            if {[info exists nativeFunc($name)]} {
                set opcodes [parseFuncArgs $name $nativeFunc($name)]
                return $opcodes
            }
            
            # Check if it's a mathfunc SECOND
            set fun [namespace which tcl::mathfunc::$name]
            if {$fun ne {}} {
                set opcodes "push $fun; "
                append opcodes [parseFuncArgs $name ""]
                return $opcodes
            }
            
            # It's an array reference
            set opcodes "push $name; "
            
            # Parse first index
            append opcodes [parse 0]
            
            # Count indices for strcat
            set indexCount 1
            
            # Check for additional indices
            set token [lindex $tokens $tokpos]
            while {$token eq ","} {
                incr tokpos
                incr indexCount
                append opcodes "push {,}; "
                append opcodes [parse 0]
                set token [lindex $tokens $tokpos]
            }
            
            # Expect closing paren
            if {$token ne ")"} {
                error "Calc: expected ')' but found '$token'"
            }
            incr tokpos
            
            # Concatenate indices if multiple
            if {$indexCount > 1} {
                set strcatCount [expr {$indexCount * 2 - 1}]
                append opcodes "strcat $strcatCount; "
            }
            
            append opcodes "loadArrayStk; "
            return $opcodes
        } else {
		    # Check if this is an assignment
		    if {$token eq "="} {
		        incr tokpos  ;# skip =
		        set opcodes "push $name; "
		        append opcodes [parse 0]
		        append opcodes "storeStk; "
		        return $opcodes
		    }
            # It's a variable reference
            set opcodes "push $name; "
            append opcodes "loadStk; "
            return $opcodes
        }
    }
    
    error "Calc: expected start of expression but found '$token'"
}





# Parse zero or more arguments to a math function. The arguments are
# expressions separated by commas and terminated by a closing parenthesis.
# Returns the bytecode to evaluate the arguments and call the function.
proc parseFuncArgs {funcname nativeInfo} {
    variable depth
    variable tokens
    variable tokpos
    
    set dep [incr depth]
    set token [lindex $tokens $tokpos]
    set arg_num 1
    
    while 1 {
        if {$token eq ")"} {
            incr tokpos
            
            # Check if we were passed native instruction info
            if {$nativeInfo ne ""} {
                lassign $nativeInfo instruction expectedArgs
                
			    set actualArgs [expr {$arg_num - 1}]
			    if {$expectedArgs > 0} {
			        # Positive: exact match required
			        if {$actualArgs != $expectedArgs} {
			            error "Calc: $funcname expects exactly $expectedArgs args, got $actualArgs"
			        }
			    } else {
			        # Negative: at least abs(expectedArgs) required
			        set minArgs [expr {abs($expectedArgs)}]
			        if {$actualArgs < $minArgs} {
			            error "Calc: $funcname expects at least $minArgs args, got $actualArgs"
			        }
			    }
			    # Generate instruction
			    if {$expectedArgs < 0} {
			        # Variable args - instruction needs arg count, at least for lindexMulti, we don't have any other's to test right now
			        append opcodes "$instruction $actualArgs; "
			    } else {
			        # Fixed args - static instruction
			        append opcodes "$instruction; "
			    }
                
            } else {
                # Mathfunc call
                append opcodes "invokeStk $arg_num; "
            }
            return $opcodes
        }
        
        
        
        
        append opcodes [parse 0]
        incr arg_num
        set token [lindex $tokens $tokpos]
        switch $token {
            , { incr tokpos }
            ) {}
            default {
                error "Calc: expected ')' or ',' but found '$token'"
            }
        }
    }
}


# We have just seen the '?' of an if-then-else, so parse the rest of that.
# Returns the bytecode to check the previous condition, then evaluate the
# appropriate branch.
proc parseTernary {} {
    variable depth
    variable inprec
    variable tokens
    variable tokpos
    set dep [incr depth]
    #deb2 "[string repeat {  } $dep]PARSETERNARY tokpos=$tokpos"

    set else else[incr ::labelcount]
    set end end$::labelcount
    append opcodes "jumpFalse $else; [parse $inprec(:)]"

    set token [lindex $tokens $tokpos]
    if {$token ne ":"} {
        error "Calc: expected ':' but found '$token'"
    }
    incr tokpos

    append opcodes "jump $end; label $else; [parse $inprec(:)]"
    append opcodes "label $end; nop; "
    return $opcodes
}
proc transform= {arglist body {inproc 0} {preserve 1}} {
    set result {}
    set lines [split $body \n]
    set i 0
    while {$i < [llength $lines]} {
        set line [lindex $lines $i]
        if {[regexp {^\s*[=:]\s+\{} $line]} {
            # path 1: = or : followed by {...} possibly multiline
            set accum $line
            while {![info complete $accum]} {
                incr i
                append accum \n [lindex $lines $i]
            }
            # extract expression - strip leading = or : and outer { }
            regexp {[=:]\s+(\{.*\})} $accum -> braced
            set expr [string range $braced 1 end-1]
            regsub {\s*;#[^\n]*$} $expr {} expr
            set tal [::Calc::compile0 $expr $inproc]
            if {$inproc} {
                regsub -all {push ([[:alpha:]][^:;\s]*); loadStk} $tal {load \1} tal
            }
            if {$preserve} {
                append result "if \{0\} \{ $accum \} \{ tcl::unsupported::assemble \{$tal\} \}\n"
            } else {
                append result "tcl::unsupported::assemble \{$tal\}\n"
            }
        } elseif {1 && [regexp {^\s*[=:]\s+(\S[^\n]*)} $line -> expr]} {
            # path 2: = or : expr  single line unbraced
            regsub {\s*;#[^\n]*$} $expr {} expr
            set tal [::Calc::compile0 $expr $inproc]
            if {$inproc} {
                regsub -all {push ([[:alpha:]][^:;\s]*); loadStk} $tal {load \1} tal
            }
            if {$preserve} {
                append result "if \{0\} \{ $line \} \{ tcl::unsupported::assemble \{$tal\} \}\n"
            } else {
                append result "tcl::unsupported::assemble \{$tal\}\n"
            }
        } elseif {1 && [regexp {\[[=:]\s+[^\]]+\]} $line]} {
            # path 3: inline [= expr] or [: expr] replacement
            set newline {}
            set pos 0
            while {[regexp -indices -start $pos {\[[=:]\s+([^\]]+)\]} $line match submatch]} {
                # append everything before this match unchanged
                append newline [string range $line $pos [expr {[lindex $match 0]-1}]]
                # extract and compile the expression
                set expr [string range $line {*}$submatch]
                set tal [::Calc::compile0 $expr $inproc]
                if {$inproc} {
                    regsub -all {push ([[:alpha:]][^:;\s]*); loadStk} $tal {load \1} tal
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




} ;############### end namespace Calc
proc proc= {name arglist body {preserve 1}} {
    if {[catch {
        set newbody [::Calc::transform= $arglist $body 1 $preserve]
    } err_code]} {
    	error "Proc= error compiling '$name' = $err_code" 
    }
    proc $name $arglist $newbody
}
proc oo::define::method= {name arglist body {preserve 1}} {
    if {[catch {
        set newbody [::Calc::transform= $arglist $body 1 $preserve]
    } err_code]} {
        error "Method= error compiling '$name' = $err_code"
    }
    uplevel 1 [list method $name $arglist $newbody]
}
