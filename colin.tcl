# Tcl prototype of a next-generation concise expression evaluator '=' from Colin Macleod 
# milestone 7 before adding =
# Bare words are treated as variable references.
# Only numeric and boolean values and operations are supported.
catch {console show}
# uncomment these for debug output
proc deb1 s {}
proc deb2 s {}
#proc deb1 s {puts $s}
#proc deb2 s {puts $s}

# original code here
if 0 {
proc = args {
    set exp [join $args]
    if { [info exist ::cache($exp)]  } {
#       incr ::cachehits($exp)
        tailcall  ::tcl::unsupported::assemble $::cache($exp)
    }
    tailcall ::tcl::unsupported::assemble  [set ::cache($exp) [compile0 $exp]]
#
# experiment with using ? operator for possible speedup
#
#    expr { ![info exist ::cache($exp)] ?  \
#    [tailcall ::tcl::unsupported::assemble  [set ::cache($exp) [compile [tokenise $exp]]] ] : \
#    [tailcall  ::tcl::unsupported::assemble $::cache($exp)] }

}
}
# make : the command and alias =, or other way around, alias is actually a performance hit
# this version is optimized for either bracing or a single argument, just to avoid the costly join
# which is needed since args could include the actual braces witchh would trigger a syntax error
# and otherwise it's likely more costly to remove the pair with string ops
proc : {arg args} {
    if { [llength $args] != 0 } {
        set arg [join "$arg $args"] 
    }
    if { [info exist ::cache($arg)]  } {
        tailcall  ::tcl::unsupported::assemble $::cache($arg)
#       incr ::cachehits($arg) ;# keep here, who knows, might be faster than being above, needed to actually count hits
    }
    tailcall ::tcl::unsupported::assemble  [set ::cache($arg) [compile0 $arg]]
}
interp alias {} = {} : ;# shorthands 


# breakup compile into 2 parts to handle ; up front, 
# also allow for ' and newline aliases, ' needs no bracing
# but newline does, and then requires no semicolon (probably can have the semi then)
proc compile0 exp {
    set exp [string map {"'" ";" "\n" ";"} $exp]
    # Fast path: no semicolons
    if {[string first ";" $exp] == -1} {
        return [compile [tokenise $exp]]
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
        append bytecode [compile [tokenise $stmt]]
        incr count
    }
    return $bytecode
}
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

    if {[llength $toks] == 0} {error "Calc: nothing to calculate"}
    set ::tokens $toks
    set ::tokpos 0
    set ::depth 0
    return [parse 0] 
}

# Pratt Parser loosely based on https://www.rosettacode.org/wiki/Arithmetic_evaluation#Nim

# Define infix operators, their precedences and bytecodes
foreach {op prec code} {
    )  0 -
    ,  0 -
    =  0 -
    ?  1 -
    :  1 -
    || 2 lor
    && 3 land
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
# Prefix ops all have the same precedence
set preprec 13

# Parse expression until we hit an operator with precedence lower than min_prec.
# The expression is supplied as a list of tokens in the global var tokens.
# The current position in the input is in global var tokpos.
# Returns the TAL bytecode to evaluate the expression.
proc parse min_prec {
    set token [lindex $::tokens $::tokpos]
    set dep [incr ::depth]
    deb2 "[string repeat {  } $dep]PARSE min_prec=$min_prec tokpos=$::tokpos token='$token'"
    incr ::tokpos
    
    # NEW: Check for assignment
    set nextop [lindex $::tokens $::tokpos]
    if {$nextop eq "=" && [regexp {^[a-zA-Z_:][a-zA-Z0-9_:]*$} $token]} {
        incr ::tokpos  ;# skip the '='
        # Left side: just the variable name
        set opcodes "push $token; "
        # Right side: parse expression (recursive, handles a = b = c)
        append opcodes [parse 0]
        # Store the result
        append opcodes "storeStk; "
        set ::depth [expr {$dep - 1}]
        return $opcodes
    }
    
    # EXISTING CODE: Normal expression parsing
    set opcodes [parsePrefix $token]
    set ::depth $dep
    while {$::tokpos < [llength $::tokens]} {
        
        set token [lindex $::tokens $::tokpos]
        if {[info exists ::inprec($token)]} {
            set tok_prec $::inprec($token)
        } else {
            error "Calc: expected operator but found '$token'"
        }
        deb2 "[string repeat {  } $dep]PARSE token=$token tok_prec=$tok_prec"
        if {$tok_prec < $min_prec} {
            break
        }
    # Binary ops are left-associative except for **
        if {$tok_prec == $min_prec && $token ne "**"} {
            break
        }
        # if-then-else needs special handling
        incr ::tokpos
        if {$token eq "?"} {
            append opcodes [parseTernary]
            continue
    }
        # Infix operator
        append opcodes [parse $tok_prec] "$::incode($token); "
    }
    deb2 "[string repeat {  } $dep]PARSE opcodes='$opcodes'"
    set ::depth [expr {$dep - 1}]
    return $opcodes
}
#instrument+  parse  
# Parse expression up to the first operator at the same level of parentheses.
# Returns the bytecode to evaluate the subexpression.
proc parsePrefix token {
    set dep [incr ::depth]
    deb2 "[string repeat {  } $dep]PARSEPREFIX token=`$token` tokpos=$::tokpos"

    # Is it a number? In C might use Tcl_GetNumberFromObj() here
    if {[string is entier $token] || [string is double $token]} {
        return "push $token; "
    }
    # Is it boolean? In C might use Tcl_GetBoolean() here
    if {$token eq "false" || $token eq "true"} {
        return "push $token; "
    }
    # Parenthesised subexpression?
    if {$token eq "("} {
        set opcodes [parse 0]
        set token [lindex $::tokens $::tokpos]
        if {$token eq ")"} {
            incr ::tokpos
            return $opcodes
    }
        error "Calc: expected ')' but found '$token'"
    }
    # Unary operator?
    if {$token in {+ - ! ~}} {
        return "[parse $::preprec]$::precode($token); "
    }
    # Function call or array reference?
    set nexttok [lindex $::tokens $::tokpos]
    if {$nexttok eq "(" && [regexp {^[[:alpha:]]} $token]} {
        set fun [namespace which tcl::mathfunc::$token]
        if {$fun ne {}} {
            # It's a function
            incr ::tokpos
            set opcodes "push $fun; "
            append opcodes [parseFuncArgs]
            return $opcodes
        } else {
            # Not a function, assume array reference
            incr ::tokpos
            set opcodes "push $token; "
            # Parse the index expression - leaves VALUE on stack
            append opcodes [parse 0]
            # Expect closing paren
            set closing [lindex $::tokens $::tokpos]
            if {$closing ne ")"} {
                error "Calc: expected ')' but found '$closing'"
            }
            # Stack now has: [arrayname, indexvalue]
            incr ::tokpos     
            append opcodes "loadArrayStk; "
            return $opcodes
        }
    }
    # Variable reference?
    set name {}
    while {$token eq {::} || [regexp {^[[:alpha:]][[:alnum:]_]*$} $token]} {
        append name $token
        set token [lindex $::tokens $::tokpos]
        incr ::tokpos
    }
    if {$name ne {}} {
        incr ::tokpos -1
        set opcodes "push $name; "
        append opcodes "loadStk; "
        return $opcodes
    }
    error "Calc: expected start of expression but found '$token'"
}
# Parse zero or more arguments to a math function. The arguments are
# expressions separated by commas and terminated by a closing parenthesis.
# Returns the bytecode to evaluate the arguments and call the function.
proc parseFuncArgs {} {
    set dep [incr ::depth]
    deb2 "[string repeat {  } $dep]PARSEFUNCARGS tokpos=$::tokpos"

    set token [lindex $::tokens $::tokpos]
    set arg_num 1
    while 1 {
        if {$token eq ")"} {
            incr ::tokpos
            append opcodes "invokeStk $arg_num; "
            return $opcodes
        }
        append opcodes [parse 0]
        incr arg_num

        set token [lindex $::tokens $::tokpos]
        switch $token {
            , { incr ::tokpos }
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
    set dep [incr ::depth]
    deb2 "[string repeat {  } $dep]PARSETERNARY tokpos=$::tokpos"

    set else else[incr ::labelcount]
    set end end$::labelcount
    append opcodes "jumpFalse $else; [parse $::inprec(:)]"

    set token [lindex $::tokens $::tokpos]
    if {$token ne ":"} {
        error "Calc: expected ':' but found '$token'"
    }
    incr ::tokpos

    append opcodes "jump $end; label $else; [parse $::inprec(:)]"
    append opcodes "label $end; nop; "
    return $opcodes
}





# globals to change if we go to a namespace, such as ::Calc::
#::depth
#::incode
#::inprec
#::precode
#::preprec
#::tokens
#::tokpos
#


if [catch {


# ============================================================================
# TEST SUITE
# ============================================================================



# ============================================================================
# VERIFICATION TEST SUITE - = command vs expr
# ============================================================================
set failures 0
set tests 0
proc verify {description expr_result calc_result} {
    global failures
    puts "[incr ::tests]) description= |$description| expr_result= |$expr_result| calc_result= |$calc_result| " ;update
    if {$expr_result ne $calc_result} {
        puts stderr "FAILED: $description"
        puts stderr "  expr returned: $expr_result"
        puts stderr "  = returned: $calc_result"
        incr failures
    }
}

puts "\n1 ========== Basic Arithmetic =========="
verify "1 + 2" [expr {1 + 2}] [= 1 + 2]

verify "10 - 3" [expr {10 - 3}] [= 10 - 3]
verify "5 * 6" [expr {5 * 6}] [= 5 * 6]
verify "20 / 4" [expr {20 / 4}] [= 20 / 4]
verify "17 % 5" [expr {17 % 5}] [= 17 % 5]
verify "2 ** 10" [expr {2 ** 10}] [= 2 ** 10]


puts " 7 ========== Operator Precedence =========="
verify "2 + 3 * 4" [expr {2 + 3 * 4}] [= 2 + 3 * 4]
verify "(2 + 3) * 4" [expr {(2 + 3) * 4}] [= (2 + 3) * 4]
verify "2 ** 3 ** 2" [expr {2 ** 3 ** 2}] [= 2 ** 3 ** 2]

puts " 10 ========== Variables =========="
set x 10
set y 20
set z 5
verify "x + y" [expr {$x + $y}] [= x + y]
verify "x * y + z" [expr {$x * $y + $z}] [= x * y + z]
verify "(x + y) * z" [expr {($x + $y) * $z}] [= (x + y) * z]

puts "13 ========== Bitwise Operations =========="
verify "15 & 7" [expr {15 & 7}] [= 15 & 7]
verify "8 | 4" [expr {8 | 4}] [= 8 | 4]
verify "12 ^ 5" [expr {12 ^ 5}] [= 12 ^ 5]
verify "1 << 8" [expr {1 << 8}] [= 1 << 8]
verify "256 >> 4" [expr {256 >> 4}] [= 256 >> 4]

puts "18 ========== Boolean Operations =========="

if {[string match 8.6* [info pa]]} {
verify "true && true" [expr {true && true}] [= true && true]
verify "true && false" [expr {true && false}] [= true && false]
verify "false || true" [expr {false || true}] [= false || true]
verify "!true" [expr {!true}] [= !true]
} else {
incr tests 4
}

puts "22 ========== Comparison =========="
set a 10
set b 20
verify "a < b" [expr {$a < $b}] [= a < b]
verify "a > b" [expr {$a > $b}] [= a > b]
verify "a == 10" [expr {$a == 10}] [= a == 10]
verify "a != b" [expr {$a != $b}] [= a != b]

puts "26 ========== Math Functions =========="
verify "sqrt(25)" [expr {sqrt(25)}] [= sqrt(25)]
verify "sqrt(x*x)" [expr {sqrt($x*$x)}] [= sqrt(x*x)]
set angle 90
verify "sin(angle)" [expr {sin($angle)}] [= sin(angle)]
verify "abs(-42)" [expr {abs(-42)}] [= abs(-42)]

puts "30 ========== Arrays =========="
array set data {10 "ten" 20 "twenty" 30 "thirty"}
set idx 10
verify "data(idx)" [expr {$data($idx)}] [= data(idx)]
set idx 20
verify "data(idx) after change" [expr {$data($idx)}] [= data(idx)]

array set nums {5 100 10 200 15 300}
set i 5
set j 10
verify "nums(i) + nums(j)" [expr {$nums($i) + $nums($j)}] [= nums(i) + nums(j)]

puts "33 ========== Complex Array Indices =========="
array set matrix {0 10 1 20 2 30 3 40}
set n 1
verify "matrix(n*2)" [expr {$matrix([expr {$n*2}])}] [= matrix(n*2)]
verify "matrix(n+1)" [expr {$matrix([expr {$n+1}])}] [= matrix(n+1)]

puts "35 ========== Ternary Operator =========="
set val 15
verify "val > 10 ? 100 : 200" [expr {$val > 10 ? 100 : 200}] [= val > 10 ? 100 : 200]
set val 5
verify "val > 10 ? 100 : 200 (val changed)" [expr {$val > 10 ? 100 : 200}] [= val > 10 ? 100 : 200]

puts "37 ========== Mixed Expressions =========="
set price 100
set quantity 5
set discount 10
verify "(price * quantity) - discount" [expr {($price * $quantity) - $discount}] [= (price * quantity) - discount]
verify "price * (quantity - 1)" [expr {$price * ($quantity - 1)}] [= price * (quantity - 1)]

puts "39 ========== Unary Operators =========="
set num 42
verify "+num" [expr {+$num}] [= +num]
verify "-num" [expr {-$num}] [= -num]
verify "~num" [expr {~$num}] [= ~num]

puts "42 ========== Boolean with Variables =========="
if { [string match 8.6* [info pa]] } {
set flag true
verify "flag && true" [expr {$flag && true}] [= flag && true]
set flag false
verify "flag && true (flag changed)" [expr {$flag && true}] [= flag && true]
} else {
incr tests 2
}

puts "44 ========== Nested Functions =========="
verify "sqrt(abs(-16))" [expr {sqrt(abs(-16))}] [= sqrt(abs(-16))]
verify "abs(sin(0))" [expr {abs(sin(0))}] [= abs(sin(0))]

puts "46 ========== Bignum Arithmetic =========="
verify "999999999999999999999 + 1" [expr {999999999999999999999 + 1}] [= 999999999999999999999 + 1]
verify "2 ** 100" [expr {2 ** 100}] [= 2 ** 100]
verify "10 ** 50" [expr {10 ** 50}] [= 10 ** 50]

puts "49 ========== Bignum Operations =========="
set big1 [expr {2 ** 100}]
set big2 [expr {3 ** 100}]
verify "big1 + big2" [expr {$big1 + $big2}] [= big1 + big2]
verify "big1 * big2" [expr {$big1 * $big2}] [= big1 * big2]
verify "big2 - big1" [expr {$big2 - $big1}] [= big2 - big1]

puts "52 ========== Factorial-like =========="
set n 1
set m 1
for {set i 1} {$i <= 50} {incr i} {
    set n [expr {$n * $i}]
    set m [= m * i]
}
verify "50! + 1" [expr {$n + 1}] [= m + 1]

puts "54 ========== Mixed Bignum and Regular =========="
set small 42
set huge [expr {10 ** 100}]
verify "huge + small" [expr {$huge + $small}] [= huge + small]
verify "huge - huge + small" [expr {$huge - $huge + $small}] [= huge - huge + small]

puts "55 ========== Bignum in Arrays =========="
array set bigdata {}
set bigdata(1) [expr {2 ** 200}]
set bigdata(2) [expr {3 ** 200}]
set idx1 1
set idx2 2
verify "bigdata(idx1) + bigdata(idx2)" [expr {$bigdata($idx1) + $bigdata($idx2)}] [= bigdata(idx1) + bigdata(idx2)]

puts "56 ========== Bignum Comparisons =========="
set a [expr {10 ** 100}]
set b [expr {10 ** 100 + 1}]
verify "a < b" [expr {$a < $b}] [= a < b]
verify "a == a" [expr {$a == $a}] [= a == a]
verify "b > a" [expr {$b > $a}] [= b > a]

puts "59 ========== Bignum with Math Functions =========="
set bigneg [expr {-(2 ** 100)}]
verify "abs(bigneg)" [expr {abs($bigneg)}] [= abs(bigneg)]

puts "60 ========== Bignum Ternary =========="
set x [expr {10 ** 100}]
set y [expr {10 ** 50}]
verify "x > y ? x : y" [expr {$x > $y ? $x : $y}] [= x > y ? x : y]

puts "61 ========== Bignum Bitwise =========="
set b1 [expr {2 ** 65}]
set b2 [expr {2 ** 66}]
verify "b1 | b2" [expr {$b1 | $b2}] [= b1 | b2]
verify "b1 & b2" [expr {$b1 & $b2}] [= b1 & b2]

puts "\n63 ========== Single Argument Math Functions =========="
verify "sqrt(144)" [expr {sqrt(144)}] [= sqrt(144)]
verify "abs(-99)" [expr {abs(-99)}] [= abs(-99)]
verify "floor(3.7)" [expr {floor(3.7)}] [= floor(3.7)]
verify "ceil(3.2)" [expr {ceil(3.2)}] [= ceil(3.2)]
verify "round(3.5)" [expr {round(3.5)}] [= round(3.5)]
verify "int(7.9)" [expr {int(7.9)}] [= int(7.9)]

set val 16
verify "sqrt(val)" [expr {sqrt($val)}] [= sqrt(val)]
set neg -50
verify "abs(neg)" [expr {abs($neg)}] [= abs(neg)]

puts "71 ========== Trigonometric Functions =========="
verify "sin(0)" [expr {sin(0)}] [= sin(0)]
verify "cos(0)" [expr {cos(0)}] [= cos(0)]
verify "tan(0)" [expr {tan(0)}] [= tan(0)]
set pi 3.14159265359
verify "sin(pi)" [expr {sin($pi)}] [= sin(pi)]
verify "cos(pi)" [expr {cos($pi)}] [= cos(pi)]

puts "76 ========== Logarithmic and Exponential =========="
verify "exp(0)" [expr {exp(0)}] [= exp(0)]
verify "exp(1)" [expr {exp(1)}] [= exp(1)]
verify "log(1)" [expr {log(1)}] [= log(1)]
verify "log10(100)" [expr {log10(100)}] [= log10(100)]
set e 2.71828
verify "log(e)" [expr {log($e)}] [= log(e)]

puts "81 ========== Two Argument Functions =========="
verify "min(5, 10)" [expr {min(5, 10)}] [= min(5, 10)]
verify "min(10, 5)" [expr {min(10, 5)}] [= min(10, 5)]
verify "max(5, 10)" [expr {max(5, 10)}] [= max(5, 10)]
verify "max(10, 5)" [expr {max(10, 5)}] [= max(10, 5)]
verify "pow(2, 8)" [expr {pow(2, 8)}] [= pow(2, 8)]
verify "pow(10, 3)" [expr {pow(10, 3)}] [= pow(10, 3)]
verify "hypot(3, 4)" [expr {hypot(3, 4)}] [= hypot(3, 4)]
verify "atan2(1, 1)" [expr {atan2(1, 1)}] [= atan2(1, 1)]

puts "89 ========== Two Argument Functions with Variables =========="
set a 7
set b 12
verify "min(a, b)" [expr {min($a, $b)}] [= min(a, b)]
verify "max(a, b)" [expr {max($a, $b)}] [= max(a, b)]
verify "pow(a, 2)" [expr {pow($a, 2)}] [= pow(a, 2)]
verify "hypot(a, b)" [expr {hypot($a, $b)}] [= hypot(a, b)]

puts "93 ========== Multiple Argument Functions =========="
verify "min(5, 10, 3)" [expr {min(5, 10, 3)}] [= min(5, 10, 3)]
verify "min(10, 5, 15, 2)" [expr {min(10, 5, 15, 2)}] [= min(10, 5, 15, 2)]
verify "max(5, 10, 3)" [expr {max(5, 10, 3)}] [= max(5, 10, 3)]
verify "max(10, 5, 15, 2)" [expr {max(10, 5, 15, 2)}] [= max(10, 5, 15, 2)]

set x 8
set y 3
set z 15
verify "min(x, y, z)" [expr {min($x, $y, $z)}] [= min(x, y, z)]
verify "max(x, y, z)" [expr {max($x, $y, $z)}] [= max(x, y, z)]

puts "99 ========== Mixed Expressions with Functions =========="
verify "min(a, b) + max(a, b)" [expr {min($a, $b) + max($a, $b)}] [= min(a, b) + max(a, b)]
verify "sqrt(pow(3, 2) + pow(4, 2))" [expr {sqrt(pow(3, 2) + pow(4, 2))}] [= sqrt(pow(3, 2) + pow(4, 2))]
verify "max(abs(-5), abs(-3))" [expr {max(abs(-5), abs(-3))}] [= max(abs(-5), abs(-3))]

puts "102 ========== Functions with Expressions as Arguments =========="
verify "min(a+1, b-1)" [expr {min($a+1, $b-1)}] [= min(a+1, b-1)]
verify "max(a*2, b/2)" [expr {max($a*2, $b/2)}] [= max(a*2, b/2)]
verify "pow(a+b, 2)" [expr {pow($a+$b, 2)}] [= pow(a+b, 2)]
verify "sqrt(a*a + b*b)" [expr {sqrt($a*$a + $b*$b)}] [= sqrt(a*a + b*b)]

puts "106 ========== Nested Multi-Argument Functions =========="
verify "max(min(a, b), min(y, z))" [expr {max(min($a, $b), min($y, $z))}] [= max(min(a, b), min(y, z))]
verify "min(max(5, 10), max(3, 8))" [expr {min(max(5, 10), max(3, 8))}] [= min(max(5, 10), max(3, 8))]

puts "108 ========== Functions with Bignums =========="
set huge1 [expr {10 ** 50}]
set huge2 [expr {10 ** 51}]
verify "min(huge1, huge2)" [expr {min($huge1, $huge2)}] [= min(huge1, huge2)]
verify "max(huge1, huge2)" [expr {max($huge1, $huge2)}] [= max(huge1, huge2)]
verify "pow(2, 100)" [expr {pow(2, 100)}] [= pow(2, 100)]

puts "111 ========== Functions in Arrays =========="
array set results {}
set results(1) 10
set results(2) 20
set results(3) 5
set k1 1
set k2 2
set k3 3
verify "min(results(k1), results(k2))" [expr {min($results($k1), $results($k2))}] [= min(results(k1), results(k2))]
verify "max(results(k1), results(k2), results(k3))" [expr {max($results($k1), $results($k2), $results($k3))}] [= max(results(k1), results(k2), results(k3))]

puts "113 ========== Functions with Ternary =========="
set test 100
verify "min(test, 50) > 40 ? 1 : 0" [expr {min($test, 50) > 40 ? 1 : 0}] [= min(test, 50) > 40 ? 1 : 0]
verify "max(a, b) < 20 ? max(a,b) : 20" [expr {max($a, $b) < 20 ? max($a,$b) : 20}] [= max(a, b) < 20 ? max(a,b) : 20]

puts "115 ========== Combining Everything =========="
set base 3
set exp 4
verify "sqrt(pow(base, exp))" [expr {sqrt(pow($base, $exp))}] [= sqrt(pow(base, exp))]
verify "min(abs(-a), abs(-b), abs(-z))" [expr {min(abs(-$a), abs(-$b), abs(-$z))}] [= min(abs(-a), abs(-b), abs(-z))]
verify "max(sqrt(a), sqrt(b), sqrt(z))" [expr {max(sqrt($a), sqrt($b), sqrt($z))}] [= max(sqrt(a), sqrt(b), sqrt(z))]







puts "\n========== Variable Scoping Tests =========="

# Test 118: Local variables with functions
proc test_locals {} {
    set x 5
    set y 12
    verify "sqrt(x*x + y*y) in locals" [expr {sqrt($x*$x + $y*$y)}] [= sqrt(x*x + y*y)]
}
test_locals

# Test 119: Global with :: prefix
set ::gx 100
set ::gy 200
proc test_global_colon {} {
    verify "max(::gx, ::gy)" [expr {max($::gx, $::gy)}] [= max(::gx, ::gy)]
}
test_global_colon

# Test 120: Global with global declaration
set ga 50
set gb 75
proc test_global_declared {} {
    global ga gb
    verify "min(ga, gb) with global" [expr {min($ga, $gb)}] [= min(ga, gb)]
}
test_global_declared

# Test 121: Mix of local and global
set ::radius 10
proc test_mixed {} {
    set height 20
    global radius
    verify "pow(radius, 2) + height" [expr {pow($radius, 2) + $height}] [= pow(radius, 2) + height]
}
test_mixed

# Test 122: Namespace variables
namespace eval ::myns {
    variable data 42
    proc test_namespace {} {
        variable data
        verify "sqrt(::myns::data) in namespace" [expr {sqrt($::myns::data)}] [= sqrt(::myns::data)]
    }
}
::myns::test_namespace




puts "\n========== Custom Math Functions (using math library) =========="

# Load math library
package require math

# Wrap math::fibonacci so it's available as a math function
proc tcl::mathfunc::fibonacci {n} {
    return [math::fibonacci $n]
}

# Test with literals
verify "fibonacci(10)" [expr {fibonacci(10)}] [= fibonacci(10)]
verify "fibonacci(20)" [expr {fibonacci(20)}] [= fibonacci(20)]
verify "fibonacci(30)" [expr {fibonacci(30)}] [= fibonacci(30)]

# Test with variables
set n 15
verify "fibonacci(n)" [expr {fibonacci($n)}] [= fibonacci(n)]

# Test generating large bignums
verify "fibonacci(100)" [expr {fibonacci(100)}] [= fibonacci(100)]
verify "fibonacci(200)" [expr {fibonacci(200)}] [= fibonacci(200)]

puts "fibonacci(100) = [= fibonacci(100)]"
puts "fibonacci(200) = [= fibonacci(200)]"
puts "fibonacci(200) length: [string length [= fibonacci(200)]] digits"

# Test custom function in expressions
set x 10
set y 12
verify "fibonacci(x) + fibonacci(y)" [expr {fibonacci($x) + fibonacci($y)}] [= fibonacci(x) + fibonacci(y)]
verify "max(fibonacci(8), fibonacci(9))" [expr {max(fibonacci(8), fibonacci(9))}] [= max(fibonacci(8), fibonacci(9))]

# Test nested with other functions
verify "sqrt(fibonacci(10))" [expr {sqrt(fibonacci(10))}] [= sqrt(fibonacci(10))]
verify "abs(fibonacci(5) - 100)" [expr {abs(fibonacci(5) - 100)}] [= abs(fibonacci(5) - 100)]

# Test with large fibonacci in comparisons
set fib100 [= fibonacci(100)]
set fib101 [= fibonacci(101)]
verify "fib100 < fib101" [expr {$fib100 < $fib101}] [= fib100 < fib101]










puts "\n========== CACHE CONTENTS =========="
parray cache
puts "\n========== CACHE hit count =========="
catch {parray cachehits}

puts "\n=========================================="
if {$failures == 0} {
    puts "ALL $::tests TESTS PASSED!"
} else {
    puts "TOTAL FAILURES: $failures"
}
puts "=========================================="



} err_code details] {
	puts "$err_code \n\n\n$details"
}
