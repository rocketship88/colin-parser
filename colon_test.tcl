#
# test suite for colon module 
#
::tcl::tm::path add [file dirname [info script]]
package require colon
catch {console show}
# --------------------------- knobs -------------------------------------------
set doAllTests        t   ;# all verify tests
set putsAllTests      [expr {"-v" in $argv}]  ;# puts on each verify test; run with -v to see them
# -----------------------------------------------------------------------------

if [catch {
if {$doAllTests} { 



# ============================================================================
# VERIFICATION TEST SUITE - = command vs expr
# ============================================================================
set failures 0
set tests 0
proc verify {description expr_result calc_result} {
    global failures
    if { $::putsAllTests } {
        puts "[incr ::tests]) description= |$description| expr_result= |$expr_result| calc_result= |$calc_result| " ;update
    } else {
        incr ::tests
    }
    if {$expr_result ne $calc_result} {
        puts stderr "FAILED: $description"
        puts stderr "  expr returned: $expr_result"
        puts stderr "  = returned: $calc_result"
        incr failures
    }
}
# For scientific notation, normalize both through expr before comparing
proc verifyFloat {desc a b} {
    global tests failures
    incr tests
    set na [expr {double($a)}]
    set nb [expr {double($b)}]
    if {$na == $nb} {
        if {$::putsAllTests} {
            puts "$tests) description= |$desc| expr_result= |$a| calc_result= |$b| "
        }
    } else {
        puts "$tests) description= |$desc| expr_result= |$a| calc_result= |$b| "
        puts "FAILED: $desc"
        puts "  expr returned: $a"
        puts "  = returned: $b"
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


puts "7 ========== Operator Precedence =========="
verify "2 + 3 * 4" [expr {2 + 3 * 4}] [= 2 + 3 * 4]
verify "(2 + 3) * 4" [expr {(2 + 3) * 4}] [= (2 + 3) * 4]
verify "2 ** 3 ** 2" [expr {2 ** 3 ** 2}] [= 2 ** 3 ** 2]

puts "10 ========== Variables =========="
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

if {[string match \n* [info pa]] || 1} {
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
if { [string match 8.6* [info pa]] || 1} {
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

puts "53 ========== Mixed Bignum and Regular =========="
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

puts "63 ========== Single Argument Math Functions =========="
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


puts "118 ========== Variable Scoping Tests =========="

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




puts "123 ========== Custom Math Functions (using math library) =========="

# Load math library
if {[catch {
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
} err_code]} {
	puts "Math test failures: $err_code"
	incr failures 
}



puts "134 ========== Assignment Operator =========="
set x 0
set y 0
: x = 10
verify "x = 10" [expr {$x}] 10

: y = x + 5
verify "y = x + 5" [expr {$y}] 15

: z = x * 2
verify "z = x * 2" [set z] 20

puts "137 ========== Self Assignment =========="
set counter 5
: counter = counter + 1
verify "counter = counter + 1" [expr {$counter}] 6

set val 100
: val = val * 2
verify "val = val * 2" [expr {$val}] 200

puts "139 ========== Chained Assignment =========="
: {a = b = c = 50}
verify "a = b = c (a)" [expr {$a}] 50
verify "a = b = c (b)" [expr {$b}] 50
verify "a = b = c (c)" [expr {$c}] 50

puts "142 ========== Multiple Statements =========="
: {p = 10 ; q = 20 ; p + q}
verify "p + q after assignment" [expr {$p + $q}] 30

puts "143 ========== Gather Function =========="
proc tcl::mathfunc::gather {args} { list {*}$args }

set result [: gather(10, 20, 30)]
verify "gather(10, 20, 30)" [list 10 20 30] $result

set x 5
set y 10
set coords [: gather(x*2, y*2)]
verify "gather(x*2, y*2)" [list [expr {$x*2}] [expr {$y*2}]] $coords

puts "145 ========== Complex: Assignment + Gather =========="
: {px = 100 ; py = 200}
set points [: gather(px, py, px+10, py+10)]
verify "gather with assigned vars" [list 100 200 110 210] $points




puts "146 ========== Logical AND (&&) Operator =========="
verify "true && true" [expr {true && true}] [: true && true]
verify "true && false" [expr {true && false}] [: true && false]
verify "false && true" [expr {false && true}] [: false && true]
verify "false && false" [expr {false && false}] [: false && false]

set a true
set b false
verify "a && b with variables" [expr {$a && $b}] [: a && b]

set x 10
set y 5
verify "x > 5 && y < 10" [expr {$x > 5 && $y < 10}] [: x > 5 && y < 10]

puts "152 ========== Logical OR (||) Operator =========="
verify "true || true" [expr {true || true}] [: true || true]
verify "true || false" [expr {true || false}] [: true || false]
verify "false || true" [expr {false || true}] [: false || true]
verify "false || false" [expr {false || false}] [: false || false]

set c false
set d true
verify "c || d with variables" [expr {$c || $d}] [: c || d]

set p 3
set q 20
verify "p < 5 || q > 15" [expr {$p < 5 || $q > 15}] [: p < 5 || q > 15]


puts "158 ========== Multi-Dimensional Arrays =========="
array set arr2 {
    0,0 100  0,1 101  0,2 102
    1,0 200  1,1 201  1,2 202
    2,0 300  2,1 301  2,2 302
}
set i 0
set j 1
verify "arr2(i,j)" [expr {$arr2($i,$j)}] [= arr2(i,j)]
set i 1
set j 2
verify "arr2(i,j) after change" [expr {$arr2($i,$j)}] [= arr2(i,j)]
verify "arr2(0,0) + arr2(1,1)" [expr {$arr2(0,0) + $arr2(1,1)}] [= arr2(0,0) + arr2(1,1)]
verify "arr2(i,j) * 2" [expr {$arr2($i,$j) * 2}] [= arr2(i,j) * 2]
puts "162 ========== 3D Arrays =========="
array set cube {
    0,0,0 1000  0,0,1 1001  0,1,0 1010  0,1,1 1011
    1,0,0 2000  1,0,1 2001  1,1,0 2010  1,1,1 2011
}
set i 1
set j 0
set k 1
verify "cube(i,j,k)" [expr {$cube($i,$j,$k)}] [= cube(i,j,k)]
verify "cube(0,0,0) + cube(1,1,1)" [expr {$cube(0,0,0) + $cube(1,1,1)}] [= cube(0,0,0) + cube(1,1,1)]

puts "164 ========== Multi-Dim with Expressions =========="
set row 1
set col 0
verify "arr2(row*1, col+1)" [expr {$arr2([expr {$row*1}],[expr {$col+1}])}] [= arr2(row*1, col+1)]
verify "arr2(i+1, j-1 + 2)" [expr {$arr2([expr {$i+1}],[expr {$j-1+2}])}] [= arr2(i+1, j-1 + 2)]
verify "cube(i-1, j, k)" [expr {$cube([expr {$i-1}],$j,$k)}] [= cube(i-1, j, k)]

# test
proc test0 {x} {
    set a2 10
    set a1 $a2
    set a3 $x
    
    set b2 20
    set b1 $b2
    set b3 5
    
    set V [list [expr {$a2 * $b3 - $a3 * $b2}] \
                [expr {$a3 * $b1 - $a1 * $b3}] \
                [expr {$a1 * $b2 - $a2 * $b1}] \
    ]
    lassign {0 1 2} zero one two
    return "$V / [lindex $V $zero] [lindex $V $one] [lindex $V $two]"
}
proc test1 {x} {

 = a1 = a2 = 10 ' a3 = x ;# comment
 = b1 = b2 = 20 ' b3 = 5  ;# uses transform 
 =  V =  list( a2 * b3 - a3 * b2, \
                a3 * b1 - a1 * b3, \
                a1 * b2 - a2 * b1  \
              ) ;# a comment here too
   lassign {0 1 2} zero one two
   return "$V / [= lindex(V , zero)] [= lindex(V, one)] [= lindex(V ,two)]"
} 	 
proc= test2 {x} {

 : a1 = a2 = 10 ' a3 = x ;# comment
 : b1 = b2 = 20 ' b3 = 5  ;# uses transform 
 : { V =  list( a2 * b3 - a3 * b2, 
                a3 * b1 - a1 * b3, 
                a1 * b2 - a2 * b1  
              ) ;# a comment here too
   }
   lassign {0 1 2} zero one two
   return "$V / [: lindex(V, zero)] [: lindex(V ,one)] [: lindex(V ,    two)]"
} 	 
puts "167 ========== proc= test =========="

verify "test 0/1" [test0 20] [test1 20]
verify "test 1/2" [test1 20] [test2 20]

puts "169 ========== method= test =========="
oo::class create Calculator {
    variable result
    
    constructor {} {
        set result 0
    }
    
    method= compute {x y} {
        : result = sqrt(x*x + y*y)
        return $result
    }
    
    method getResult {} {
        return $result
    }
}

set calc [Calculator new]
verify "compute 3 4" [expr {sqrt(3*3 + 4*4 )}] [$calc compute 3 4] 
verify "getResult" [expr {sqrt(3*3 + 4*4)}] [$calc getResult]  



puts "171 ========== Variable Scope in proc= =========="

# Setup namespace variables
namespace eval testns {
    variable _nsvar 100
    variable nsvar2 200
}
set ::_global 42
set ::plainGlobal 99

# Local variables optimised to LVT slots
proc= testLocal {x y} {
    : result = x + y
    return $result
}
verify "local vars x+y" [expr {5 + 10}] [testLocal 5 10]

# Underscore local variables
proc= testUnderscore {_x _y} {
    : _result = _x + _y
    return $_result
}
verify "underscore locals _x+_y" [expr {5 + 10}] [testUnderscore 5 10]

# Namespace qualified variables - runtime path
proc= testNamespace {x} {
    : testns::_nsvar + x
}
verify "namespace ::testns::_nsvar + x" [expr {$testns::_nsvar + 5}] [testNamespace 5]

# Global namespace variables
proc= testGlobal {x} {
    : ::plainGlobal + x
}
verify "::plainGlobal + x" [expr {$::plainGlobal + 5}] [testGlobal 5]

# Mixed local and namespace
proc= testMixed {x} {
    : x + ::_global
}
verify "local + ::_global" [expr {5 + $::_global}] [testMixed 5]

# Assignment to local vs namespace
proc= testAssign {x} {
    : local = x * 2
    : testns::nsvar2 = x * 3
    return $local
}
verify "assign local and namespace" [expr {5 * 2}] [testAssign 5]
verify "namespace var assigned" [expr {5 * 3}] $testns::nsvar2

# Chained assignment local
proc= testChained {x} {
    : a = b = x + 1
    return $a
}
verify "chained assignment locals" [expr {5 + 1}] [testChained 5]

# Underscore namespace variable
proc= testNsUnderscore {x} {
    : testns::_nsvar + x
}
verify "namespace _nsvar + x" [expr {$testns::_nsvar + 5}] [testNsUnderscore 5]



puts "180 ========== Namespace Lookup Version Differences =========="

namespace eval outer {
    variable x 1000
    namespace eval inner {
        variable y 2000
    }
}

# These work the same on both versions - fully qualified always safe
proc= testFullyQualified {n} {
    : ::outer::x + n
}
verify "fully qualified ::outer::x" [expr {$::outer::x + 5}] [testFullyQualified 5]

proc= testFullyQualified2 {n} {
    : ::outer::inner::y + n
}
verify "fully qualified ::outer::inner::y" [expr {$::outer::inner::y + 5}] [testFullyQualified2 5]

if {$::tcl_version >= 9.0} {

    puts "182 ========== 9.x Relative Namespace Lookup =========="
    # In 9.x namespace variables must be fully qualified in proc=
    namespace eval outer {
        proc= ::outer::testRelative {n} {
            : ::outer::x + n
        }
    }
    verify "9.x fully qualified ::outer::x" [expr {$::outer::x + 5}] [::outer::testRelative 5]

    namespace eval outer::inner {
        proc= ::outer::inner::testRelativeInner {n} {
            : ::outer::inner::y + n
        }
    }
    verify "9.x fully qualified ::outer::inner::y" [expr {$::outer::inner::y + 5}] [::outer::inner::testRelativeInner 5]

} else {
    puts "182 ========== 8.6 Global Namespace Lookup =========="
    # In 8.6, unqualified namespace refs default to global namespace
    proc= testUnqualified {n} {
        : outer::x + n
    }
    verify "8.6 unqualified outer::x" [expr {$::outer::x + 5}] [testUnqualified 5]

    proc= testUnqualified2 {n} {
        : outer::inner::y + n
    }
    verify "8.6 unqualified outer::inner::y" [expr {$::outer::inner::y + 5}] [testUnqualified2 5]
}



puts "184 ========== Scientific Notation =========="

# Basic scientific notation without sign - works without merge
verifyFloat "10e5"          [expr {10e5}]          [= 10e5]
verifyFloat "1.5e3"         [expr {1.5e3}]         [= 1.5e3]
verifyFloat "2E4"           [expr {2E4}]           [= 2E4]

# Scientific notation with negative exponent - requires merge
verifyFloat "3.0e-33"       [expr {3.0e-33}]       [= 3.0e-33]
verifyFloat "1.5e-10"       [expr {1.5e-10}]       [= 1.5e-10]
verifyFloat "2.0E-5"        [expr {2.0E-5}]        [= 2.0E-5]

# Scientific notation with positive exponent - requires merge
verifyFloat "3.0e+33"       [expr {3.0e+33}]       [= 3.0e+33]
verifyFloat "1.5e+10"       [expr {1.5e+10}]       [= 1.5e+10]

# Scientific notation in expressions
verifyFloat "1.0e-3 + 2.0e-3" [expr {1.0e-3 + 2.0e-3}] [= 1.0e-3 + 2.0e-3]
verifyFloat "1.5e2 * 2.0e-1"  [expr {1.5e2 * 2.0e-1}]  [= 1.5e2 * 2.0e-1]

# Variables ending in e should not trigger false merge
set angle 45
set force 100
verifyFloat "angle + force" [expr {$angle + $force}] [= angle + force]

puts "195 ========== Scientific Notation Errors =========="

# These should produce errors
foreach {desc expr} {
    "invalid: bare e"           "e33"
    "invalid: e without digits" "3.0e"
    "invalid: double e"         "3.0e-3e2"
} {
    if {[catch {= $expr} err]} {
        puts "  expected error for $desc: ok"
    } else {
        puts "  UNEXPECTED SUCCESS for $desc"
        incr failures
    }
}




puts "195 ========== Tuples (compared with lassign) =========="
# clr unsets the named globals; vals lists their values (UNSET if missing)
proc clr {args} {foreach n $args {unset -nocomplain ::$n}}
proc vals {args} {
    set out {}
    foreach n $args {lappend out [expr {[info exists ::$n] ? [set ::$n] : "UNSET"}]}
    return $out
}

set pt {3 4 5}
set D {{1 2} {3 4} {5 6}}
set edge {{1 2} {7 9}}

clr x y z; lassign $pt x y z;               set want [vals x y z]
clr x y z; = (x, y, z) = pt;                verify "tuple (x,y,z) = pt"          $want [vals x y z]
clr x y z; = pt -> (x, y, z);               verify "tuple pt -> (x,y,z)"         $want [vals x y z]
clr x y z; = pt => (x, y, z);               verify "tuple pt => (x,y,z)"         $want [vals x y z]
verify "tuple returns the list"  $pt [= (x, y, z) = pt]
verify "tuple -> returns the list" $pt [= pt -> (x, y, z)]

clr x1 y1 x2 y2; lassign [lindex $edge 0] x1 y1; lassign [lindex $edge 1] x2 y2; set want [vals x1 y1 x2 y2]
clr x1 y1 x2 y2; = ((x1,y1),(x2,y2)) = edge; verify "nested ((x1,y1),(x2,y2)) = edge" $want [vals x1 y1 x2 y2]
clr x1 y1 x2 y2; = edge -> ((x1,y1),(x2,y2)); verify "nested edge -> ((x1,y1),(x2,y2))" $want [vals x1 y1 x2 y2]
clr x1 y1 x2 y2; = edge => ((x1,y1),(x2,y2)); verify "nested edge => ((x1,y1),(x2,y2))" $want [vals x1 y1 x2 y2]

clr ax ay bx by cx cy
lassign $D p1 p2 p3; lassign $p1 ax ay; lassign $p2 bx by; lassign $p3 cx cy; set want [vals ax ay bx by cx cy]
clr ax ay bx by cx cy; = D -> ((ax,ay),(bx,by),(cx,cy)); verify "three pairs, nested pattern" $want [vals ax ay bx by cx cy]

clr ax ay bx by cx cy; lassign $D ax ay bx by cx cy; set want [vals ax ay bx by cx cy]
clr ax ay bx by cx cy; = D -> (ax,ay,bx,by,cx,cy);  verify "three pairs, flat pattern (whole elements, rest empty)" $want [vals ax ay bx by cx cy]
clr ax ay bx by cx cy; = (ax,ay,bx,by,cx,cy) = D;    verify "three pairs, flat pattern with =" $want [vals ax ay bx by cx cy]

clr first mid last; lassign $pt first mid last; set want [vals first last]
clr first mid last; = (first, _, last) = pt; verify "skip with _" $want [vals first last]
verify "_ is not assigned" UNSET [vals _]

set s12 {1 2}; set s5 {1 2 3 4 5}
clr a b c; lassign $s12 a b c; set want [vals a b c]
clr a b c; = (a, b, c) = s12;   verify "missing elements are empty" $want [vals a b c]
clr a b; lassign $s5 a b; set want [vals a b]
clr a b; = (a, b) = s5;  verify "extra elements ignored" $want [vals a b]

clr x; = pt -> x; verify "single name takes the whole value" [list $pt] [vals x]

verify "list builder (1,2,3)"       [list 1 2 3]            [= (1,2,3)]
verify "list builder nested"        [list {1 2} {3 4}]      [= ((1,2),(3,4))]
set u 3; set v 4
verify "list builder with names"    [list 3 4 7]            [= (u, v, u+v)]
verify "grouping is not a list"     9                       [= (1+2)*3]
verify "round trip"                 {4 3 2 1}               [= {((a,b),(c,d)) = ((1,2),(3,4)); (d,c,b,a)}]

puts "216 ========== Strict tuples: => =========="
proc tryerr {script} {
    if {[catch {uplevel #0 $script} msg]} {return $msg} else {return OK}
}
verify "=> pattern longer than data"  "Calc: tuple mismatch: pattern (ax,ay,bx,by,cx,cy) expects 6, data has 3 (use -> to allow a mismatch)" [tryerr {= D => (ax,ay,bx,by,cx,cy)}]
verify "=> data longer than pattern"  "Calc: tuple mismatch: pattern (p,q) expects 2, data has 4 (use -> to allow a mismatch)" [tryerr {= (1,2,3,4) => (p,q)}]
verify "=> group against one value"   "Calc: tuple mismatch: pattern (p,q) expects 2, data has 1 (use -> to allow a mismatch)" [tryerr {= (1,2) => ((p,q),(r,s))}]
verify "=> inner group named"         "Calc: tuple mismatch: pattern (bx) expects 1, data has 2 (use -> to allow a mismatch)" [tryerr {= D => ((ax,ay),(bx),(cx,cy))}]
clr p q r; catch {= (1,2) => (p,q,r)}
verify "=> outer mismatch assigns nothing" {UNSET UNSET UNSET} [vals p q r]
clr p q r; = (1,2) -> (p,q,r)
verify "-> pads instead"              {1 2 {}} [vals p q r]
verify "=> _ counts as a slot"        OK [tryerr {= (1,2,3) => (p,_,r)}]
verify "=> _ slot, wrong size"        "Calc: tuple mismatch: pattern (p,_) expects 2, data has 3 (use -> to allow a mismatch)" [tryerr {= (1,2,3) => (p,_)}]

puts "224 ========== Tuples in procs =========="
proc plain_edge {edge} {
    lassign $edge p1 p2; lassign $p1 x1 y1; lassign $p2 x2 y2
    expr {hypot($x2-$x1, $y2-$y1)}
}
proc= tuple_edge {edge} {
    = ((x1,y1),(x2,y2)) = edge
    = hypot(x2-x1, y2-y1)
}
proc= tuple_edge_strict {edge} {
    = edge => ((x1,y1),(x2,y2))
    = hypot(x2-x1, y2-y1)
}
verify "proc= tuple edge"         [plain_edge $edge] [tuple_edge $edge]
verify "proc= strict tuple edge"  [plain_edge $edge] [tuple_edge_strict $edge]
verify "proc= strict mismatch"    "Calc: tuple mismatch: pattern ((x1,y1),(x2,y2)) expects 2, data has 3 (use -> to allow a mismatch)" [tryerr {tuple_edge_strict {{1 2} {3 4} {5 6}}}]
proc plain_ok {} {= (1,2,3) => (a,b,c); list $a $b $c}
verify "plain proc strict ok"     {1 2 3} [plain_ok]

puts "228 ========== Tuple targets that must be rejected =========="
foreach {desc script} {
    "number target"            {= 1 = 1 + 1}
    "number in pattern ="      {= (a, 1) = pt}
    "number in pattern ->"     {= pt -> (a, 1)}
    "number in pattern =>"     {= pt => (a, 1)}
    "float in pattern"         {= pt -> (a, 3.5)}
    "expression in pattern"    {= pt -> (a, b+1)}
    "function call in pattern" {= pt -> (a, sin(1))}
    "string in pattern"        {= pt -> (a, "b")}
} {
    verify "rejected: $desc" 1 [catch {uplevel #0 $script}]
}

puts "236 ========== ++ and -- (increment / decrement) =========="
# ++i / --i give the NEW value, i++ / i-- give the OLD value; all four change the variable
set i 5
verify "++i value"            6 [= ++i]
verify "++i variable"         6 $i
verify "i++ value"            6 [= i++]
verify "i++ variable"         7 $i
verify "--i value"            6 [= --i]
verify "--i variable"         6 $i
verify "i-- value"            6 [= i--]
verify "i-- variable"         5 $i
# same results as Tcl's incr
set i 5; set want [incr i];  set i 5; verify "++i same as incr"        $want [= ++i]
set i 5; set want [incr i -1]; set i 5; verify "--i same as incr -1"   $want [= --i]

puts "246 ========== ++ and -- create unset variables, as incr does =========="
clr n1 n2 n3 n4
verify "unset ++n value"      1  [= ++n1]
verify "unset ++n variable"   1  $n1
verify "unset n++ value"      0  [= n2++]
verify "unset n++ variable"   1  $n2
verify "unset --n value"      -1 [= --n3]
verify "unset --n variable"   -1 $n3
verify "unset n-- value"      0  [= n4--]
verify "unset n-- variable"   -1 $n4

puts "254 ========== ++ and -- inside larger expressions =========="
set i 5
verify "i++ + ++i"            12 [= i++ + ++i]       ;# 5 + 7
verify "i++ + ++i variable"   7  $i
verify "++i * 2"              16 [= ++i * 2]
set i 8
verify "j = i++ + 1"          9  [= j = i++ + 1]
verify "j = i++ + 1, j"       9  $j
verify "j = i++ + 1, i"       9  $i
verify "multi statement"      3  [= {a = 1; a++; a++; a}]
verify "a+++b is a++ + b"     3  [= {a = 1; b = 2; a+++b}]
verify "a+++b changed a"      2  $a
verify "ternary with i++"     1  [= {k = 0; k++ > -1 ? 1 : 0}]
verify "function args"        2  [= {m = 1; max(m++, m++)}]
verify "list of postfix"      {3 4 5} [= {m = 3; (m++, m++, m)}]
verify "negated prefix"       -4 [= {m = 3; -(++m)}]
verify "prefix of negative"   -4 [= {m = -5; ++m}]
verify "double value"         7.0 [= {m = 1; m++ * 2.0 + 5.0}]

puts "269 ========== ++ and -- only take whole integer variables =========="
set z 2.5
verify "float variable ++"    {expected integer but got "2.5"} [tryerr {= z++}]
verify "float variable --"    {expected integer but got "2.5"} [tryerr {= --z}]
set z abc
verify "string variable ++"   {expected integer but got "abc"} [tryerr {= ++z}]
verify "number after ++"      "Calc: expected a variable name after '++' but found '5'" [tryerr {= ++5}]
verify "paren after --"       "Calc: expected a variable name after '--' but found '('" [tryerr {= --(a)}]
verify "missing name"         1 [catch {= ++}]
foreach {desc script} {
    "number postfix"       {= 5++}
    "expression postfix"   {= (a+1)++}
    "array element prefix" {= ++data(1)}
    "array element postfix" {= data(1)++}
    "double ++"            {= ++ ++a}
    "i++ ++"               {= i++ ++}
    "5--3 is not 5 - -3"   {= 5--3}
} {
    verify "rejected: $desc" 1 [catch {uplevel #0 $script}]
}

puts "282 ========== tokenizer: spaced and scientific forms are unchanged =========="
verify "5 - -3"               8   [= 5 - -3]
verify "5 - (-3)"             8   [= 5 - (-3)]
verify "5 + +3"               8   [= 5 + +3]
verify "- -3"                 3   [= - -3]
verifyFloat "3.0e-3 * 1000"   3.0 [= 3.0e-3 * 1000]
verify "-> still works"       2   [= {l = list(1,2); l -> (p, q); q}]
verify "i - 1 unchanged"      4   [= {i = 5; i - 1}]
verify "i-1 unchanged"        4   [= {i = 5; i-1}]
verify "i+1 unchanged"        6   [= {i = 5; i+1}]

puts "291 ========== ++ and -- in procs =========="
proc plain_count {n} {
    set i 0; set s 0
    while {$i < $n} { set s [expr {$s + $i}]; incr i }
    list $s $i
}
proc= incr_count {n} {
    = {i = 0; s = 0}
    while {$i < $n} {
        = s = s + i
        = i++
    }
    list $s $i
}
proc= incr_count_pre {n} {
    = {i = 0; s = 0}
    while {$i < $n} {
        = s = s + i
        = ++i
    }
    list $s $i
}
proc= decr_count {n} {
    = k = n
    set out {}
    while {$k > 0} {
        = k--
        lappend out $k
    }
    set out
}
verify "proc= i++ loop"       [plain_count 10] [incr_count 10]
verify "proc= ++i loop"       [plain_count 10] [incr_count_pre 10]
verify "proc= k-- loop"       {2 1 0} [decr_count 3]
proc= incr_values {n} { = {x = n++; y = ++n; (x, y, n)} }
verify "proc= values"         {10 12 12} [incr_values 10]
proc= incr_unset {} { = {a = nvp++; b = ++nvq; (a, b, nvp, nvq)} }
verify "proc= unset vars"     {0 1 1 1} [incr_unset]
proc= incr_tuple {pt} {
    = pt -> (x, y)
    = {x++; y--; (x, y)}
}
verify "proc= with tuple"     {2 9} [incr_tuple {1 10}]
proc= incr_float {} { = {f = 2.5; f++} }
verify "proc= float error"    {expected integer but got "2.5"} [tryerr incr_float]
proc= incr_global {} { global gcount; = gcount++ }
set gcount 7
verify "proc= global value"   7 [incr_global]
verify "proc= global changed" 8 $gcount
verify "assembled to incrImm" 1 [string match {*incrImm*} [info body incr_count]]
verify "no incr in plain +"   0 [string match {*incrImm*} [info body plain_count]]

puts "302 ========== ++ and -- with Calc::transformProcs =========="
proc tp_incr {n} {
    = {n++; n++; n}
}
Calc::transformProcs tp_incr
verify "transformProcs ++"    3 [tp_incr 1]
verify "transformProcs body"  1 [string match {*incrImm*} [info body tp_incr]]

puts "320 ========== Found by mutation testing (see the mutation folder) =========="
# --- precedence and associativity of every pair of binary operators, against expr
set mutOps {|| && | ^ & == != < > <= >= << >> + - * / % **}
array unset ::Calc::cache
set bad {}
foreach a $mutOps { foreach b $mutOps {
    set text "x $a y $b z"
    set vals [expr {$a in {/ %} || $b in {/ %} ? {1 2 3} : {0 1 2 3}}]
    foreach x $vals { foreach y $vals { foreach z $vals {
        set want [expr {[catch {expr "$x $a $y $b $z"} w] ? "ERR" : $w}]
        set got  [expr {[catch {= $text} g] ? "ERR" : $g}]
        if {$want ne $got} {lappend bad "$text with $x $y $z: got $got want $want"}
    }}}
}}
verify "all pairs of binary operators agree with expr" {} [lrange $bad 0 2]
set bad {}
foreach u {- + ! ~} { foreach op $mutOps {
    set vals [expr {$op in {/ %} ? {1 2 3} : {0 1 2 3}}]
    foreach {ctext etext} [list "$u x $op y" "\$u \$x $op \$y" "x $op $u y" "\$x $op \$u \$y"] {
        foreach x $vals { foreach y $vals {
            set want [expr {[catch {expr [subst -nocommands $etext]} w] ? "ERR" : $w}]
            set got  [expr {[catch {= $ctext} g] ? "ERR" : $g}]
            if {$want ne $got} {lappend bad "$ctext with $x $y: got $got want $want"}
        }}
    }
}}
verify "unary operators bind tighter than binary ones" {} [lrange $bad 0 2]
array unset ::Calc::cache
set out {}
foreach {x y} {1 2 2 2 3 2} { lappend out [= x <= y] [= x >= y] [= x < y] [= x > y] }
verify "<= and >= around equality" {1 0 1 0 1 1 0 0 0 1 0 1} $out
set bad {}
foreach text {
    {a ? b : c} {a ? b + 1 : c + 2} {a || b ? c : d} {a ? b : c || d} {a ? b : (c ? d : e)}
    {a ? (b ? c : d) : e} {(a ? b : c) ? d : e} {a == b ? c : d} {a + b ? c : d} {a ? b : c + d * e}
    {a && b ? c : d} {a < b ? c : d} {a ? b : c == d} {a ? b : c << d} {a | b ? c : d}
} {
    set etext [regsub -all {\m([a-e])\M} $text {$\1}]
    foreach a {0 1 2} { foreach b {0 1 2} { foreach c {0 1 2} { foreach d {0 1 2} { foreach e {0 1 2} {
        if {[expr $etext] ne [= $text]} {lappend bad "$text with $a $b $c $d $e"}
    }}}}}
}
verify "ternary mixed with other operators" {} [lrange $bad 0 2]
set a 1; set b 2
verify "assignment is lower than ternary" {5 10 1 5 10 1 1} [list [= r = a + b * 2] [= s = a < b ? 10 : 20] [= t = u = a || 0] $r $s $t $u]

# --- one-element parentheses only group
set x {a b}
verify "(x) is just x" {{a b} {a b} {{a b} 1}} [list [= (x)] [= ((x))] [= (x, 1)]]

# --- native functions
set s abc; set l {x y z}
verify "native functions" {3 3 y {1 2 3} ABC abc b ab} \
    [list [= strlen(s)] [= llength(l)] [= lindex(l, 1)] [= list(1, 2, 3)] [= xtoupper(s)] [= xtolower(xtoupper(s))] [= xstrindex(s, 1)] [= xstrrange(s, 0, 1)]]
foreach {e msg} {
    {strlen(s, s)}      "Calc: strlen expects exactly 1 args, got 2"
    {llength()}         "Calc: llength expects exactly 1 args, got 0"
    {lindex(l)}         "Calc: lindex expects at least 2 args, got 1"
    {list()}            "Calc: list expects at least 1 args, got 0"
    {xtoupper(s, s)}    "Calc: xtoupper expects exactly 1 args, got 2"
    {xstrindex(s)}      "Calc: xstrindex expects exactly 2 args, got 1"
    {xstrrange(s, 0)}   "Calc: xstrrange expects exactly 3 args, got 2"
} {
    verify "arity: $e" $msg [tryerr [list = $e]]
}
proc= nf_shape {l s} { = {a = llength(l); b = strlen(s); c = lindex(l, 1); d = list(a, b)} }
set body [info body nf_shape]
verify "native function results in proc=" {3 5} [nf_shape {p q r} hello]
verify "native function instructions" {1 1 1 1} [list [string match *listLength* $body] [string match *strlen* $body] [string match *lindexMulti* $body] [string match {*list 2*} $body]]
proc nf_add {a b} {expr {$a + $b}}
proc= nf_call {x} { = {r = nf_add(x, 10); s = max(r, 3); sqrt(s)} }
verify "proc and mathfunc calls" {4.0 1} [list [nf_call 6] [string match *invokeStk* [info body nf_call]]]

# --- messages for malformed input
foreach {e msg} {
    {1 )}            "Calc: unexpected token ')' in expression '1 )'"
    {(1, 2}          "Calc: expected ')' but found ''"
    {(1}             "Calc: expected ')' but found ''"
    {max(1, 2}       "Calc: expected ')' or ',' but found ''"
    {nosuch(1, 2}    "Calc: expected ')' but found ''"
    {x ? 1}          "Calc: expected ':' but found ''"
    {5 = 3}          "Calc: cannot assign to number literal '5'"
    {true = 1}       "Calc: cannot assign to boolean literal 'true'"
    {false = 1}      "Calc: cannot assign to boolean literal 'false'"
    {a = 1 ) ; 3}    "Calc: unexpected token ')' in statement 'a = 1 )'"
    {a = 1; 2 3}     "Calc: expected start of expression but found '2 3'"
} {
    verify "error: $e" $msg [tryerr [list = $e]]
}
# --- the compile cache is limited to 1000 expressions
array unset ::Calc::cache
for {set i 0} {$i < 1000} {incr i} {set ::Calc::cache(fake$i) {}}
verify "cache: 1000 entries still compile" {0 8} [list [catch {= 7 + 1} m1] $m1]
verify "cache: the 1001st is refused"      {1 {Calc: Cache exhausted with expression: 8 + 1}} [list [catch {= 8 + 1} m2] $m2]
array unset ::Calc::cache
# --- numbers
set e 5
verify "number forms" {1001 12 6 1.0 1.0 1.0 20.0 20.0 2 2} \
    [list [= 1_000 + 1] [= 0d12] [= 0D5 + 1] [= 1e-2 * 100] [= 5e-1 * 2] [= 1E-1 * 10] [= 2e+1 * 1] [= 2E+1 * 1] [= e-3] [= e - 3]]
set a_b 4; set _c 5; set d0 6
verify "names with underscores" {5 6 7} [list [= a_b + 1] [= _c + 1] [= d0 + 1]]
verify "trailing comments" 6 [= {p = 3 ;# set p
                                 p * 2 # double it
                                }]

puts "338 ========== several && and || in one body (labels must be unique) =========="
proc= lab1 {a b c} {= (a < 0) || (b < 0) || (c < 0)}
verify "three || in one expression" {0 1 1 1} [list [lab1 1 1 1] [lab1 1 -1 1] [lab1 1 1 -1] [lab1 -1 1 1]]
proc= lab2 {a b c} { = { x = (a < 0) || (b < 0); y = (b < 0) || (c < 0); x*2 + y } }
verify "|| in two statements" {0 2 3 1} [list [lab2 1 1 1] [lab2 -1 1 1] [lab2 1 -1 1] [lab2 1 1 -1]]
proc= lab3c {a b c} {= !(((a<0)||(b<0)||(c<0)) && ((a>0)||(b>0)||(c>0)))}
proc lab3e {a b c} {expr {!((($a<0)||($b<0)||($c<0)) && (($a>0)||($b>0)||($c>0)))}}
set bad {}
foreach a {-2 0 3} {foreach b {-1 0 5} {foreach c {-4 0 7} {
    if {[lab3c $a $b $c] != [lab3e $a $b $c]} {lappend bad [list $a $b $c]}
}}}
verify "|| mixed with &&, against expr" {} $bad
proc= lab4 {a b c} {= a > 0 ? (b > 0 && c > 0 ? 1 : 2) : (b < 0 || c < 0 ? 3 : 4)}
verify "&& and || in ternary branches" {1 2 3 4} [list [lab4 1 1 1] [lab4 1 1 -1] [lab4 -1 -1 1] [lab4 -1 1 1]]
proc lab5 {a b c} {= (a > 0) && (b > 0) && (c > 0)}
Calc::transformProcs lab5
verify "two && through transformProcs" {1 0 0} [list [lab5 1 1 1] [lab5 1 1 0] [lab5 0 1 1]]

puts "345 ========== proc= / calc= / method= output and errors =========="
proc= pp1 {x} {
    = y = x + 1
    = {z = y * 2}
    set r [= z + 1]
    set s [: {
        w = z + 2
    }]
    return [list $r $s]
}
set b [info body pp1]
verify "proc= results" {11 12} [pp1 4]
verify "proc= uses locals" {0 0 1 1 1 1} [list [string match *loadStk* $b] [string match *storeStk* $b] [string match {*store y*} $b] [string match {*store z*} $b] [string match {*store w*} $b] [string match {*load x*} $b]]
proc= pp2 {x} {
    = y = x + 1
    return $y
}
proc= pp3 {x} {
    = y = x + 1
    return $y
} 0
verify "proc= keeps source unless debug is 0" {2 2 1 0 1} [list [pp2 1] [pp3 1] [string match {*if {0}*= y = x + 1*} [info body pp2]] [string match *if* [info body pp3]] [string match *assemble* [info body pp3]]]
proc= pp4 {x} {
    set v [: {
        t = x +
            10;
        t * 2
    }] ; set w 5
    set u [: x + 1 ]
    set k "<[= {x * 3}]>"
    set m [list [= x + 2] [: {x + 3}]]
    return [list $v $w $u $k $m]
}
verify "inline forms with text around them" {22 5 2 <3> {3 4}} [pp4 1]
proc= pp5 {x} {
    set v [: {
        x + 1
    }
    ]
    return $v
}
verify "closing bracket on a later line" {2 1} [list [pp5 1] [string match *assemble* [info body pp5]]]
foreach {name body msg} {
    e1 {
    set q 1
    = 1 + )
}            "Proc= error compiling 'e1'  line 3: Calc: expected start of expression but found ')'"
    e2 {
    set q 1
    set z [= 1 + )]
}            "Proc= error compiling 'e2'  line 3: Calc: expected start of expression but found ')'"
    e3 {
    set q 1
    = {
        a = 1;
        b = 2 )
    }
}            "Proc= error compiling 'e3'  line 6: Calc: unexpected token ')' in statement 'b = 2 )'"
    e4 {
    set q 1
    set z [= {
       1 + )
    }]
}            "Proc= error compiling 'e4'  line 5: Calc: expected start of expression but found ')'"
} {
    verify "proc= error $name" $msg [tryerr [list proc= $name {} $body]]
}
verify "calc= error" "Calc= error in section 'sect' line 3: Calc: expected start of expression but found ')'" [tryerr {calc= sect {
    set q 1
    = 1 + )
}}]
oo::class create T48
verify "method= error" "Method= error compiling 'm'  line 2: Calc: expected start of expression but found ')'" [tryerr {oo::define T48 {method= m {} {
    = 1 + )
}}}]
array unset ::Calc::calc
set q 3
calc= first {
    = q2 = q * 2
}
calc= second {
    = q3 = q * 3
} 0
verify "calc= runs in the caller, records unless debug is 0" {6 9 {{"calc=first"}}} [list $q2 $q3 [lsort [array names ::Calc::calc]]]
namespace eval ns48 {
    proc= f {x} { = x + 1 }
}
oo::class create T48b { method= m {x} { = x * 2 } }
verify "proc= and method= define in the caller's namespace" {2 {} 8} [list [ns48::f 1] [info procs ::f] [[T48b new] m 4]]

puts "362 ========== Calc::transformProcs options =========="
namespace eval ::tp49 {
    proc a {x {y 5}} {
        = x + y
    }
    proc b {x} {
        = {x * 2}
    }
    namespace eval inner {
        proc c {x} {
            = x - 1
        }
    }
}
proc tp49top {x} {
    = x + 100
}
verify "-listonly" {{::tp49::a ::tp49::b} ::tp49::inner::c {::tp49::a ::tp49::b} ::tp49top 0 0} \
    [list [Calc::transformProcs -listonly ::tp49::*] [Calc::transformProcs -listonly ::tp49::inner::*] \
          [Calc::transformProcs -listonly ::tp49::a ::tp49::b] [Calc::transformProcs -listonly tp49top] \
          [string match *assemble* [info body ::tp49::a]] [string match *assemble* [info body tp49top]]]
verify "-regex and globs" {{::tp49::a ::tp49::b} ::tp49top ::tp49top {} {}} \
    [list [Calc::transformProcs -listonly -regex {^::tp49::[ab]$}] [Calc::transformProcs -listonly -regex {^tp49t}] \
          [Calc::transformProcs -listonly {tp49t?p}] [Calc::transformProcs -listonly {tp49t?}] [Calc::transformProcs -listonly nosuchproc]]
set done [Calc::transformProcs ::tp49::a]
info default ::tp49::a y d
verify "transformProcs result, args and defaults" {::tp49::a {x y} 5 6 11 1} [list $done [info args ::tp49::a] $d [::tp49::a 1] [::tp49::a 1 10] [string match *assemble* [info body ::tp49::a]]]
Calc::transformProcs -preserve 0 ::tp49::b
set b1 [info body ::tp49::b]
Calc::transformProcs -preserve 1 tp49top
set b2 [info body tp49top]
verify "-preserve" {0 1 8 101} [list [string match *if* $b1] [string match {*if {0}*} $b2] [::tp49::b 4] [tp49top 1]]
set before [string match *assemble* [info body ::tp49::inner::c]]
Calc::transformProcs ::tp49::*
set mid [string match *assemble* [info body ::tp49::inner::c]]
Calc::transformProcs ::tp49::inner::*
verify "nested namespaces need their own pattern" {0 0 1 3} [list $before $mid [string match *assemble* [info body ::tp49::inner::c]] [::tp49::inner::c 4]]
set all [Calc::AllProcsRecursive]
verify "AllProcsRecursive" {1 1 1 0} [list [expr {[lsearch -exact $all ::tp49top] >= 0}] [expr {[lsearch -exact $all ::tp49::a] >= 0}] [expr {[lsearch -exact $all ::tp49::inner::c] >= 0}] [expr {[lsearch -exact $all ::tp49::nosuch] >= 0}]]
foreach {desc argv0 msg} {
    "no pattern"        {}           "Calc::transformProcs: no proc name pattern(s) given"
    "-preserve alone"   {-preserve}  "Calc::transformProcs: -preserve requires a value"
    "-listonly alone"   {-listonly}  "Calc::transformProcs: no proc name pattern(s) given"
} {
    verify "transformProcs: $desc" $msg [tryerr "Calc::transformProcs $argv0"]
}
proc tp49bad {} {
    = 1 + )
}
verify "transformProcs reports the proc" 1 [string match "Calc::transformProcs error compiling '::tp49bad'*expected start of expression but found ')'" [tryerr {Calc::transformProcs tp49bad}]]
proc tp49plain {a {b 2}} {expr {$a * $b}}
Calc::transformProcs tp49plain
verify "transformProcs on plain Tcl" {6 12} [list [tp49plain 3] [tp49plain 3 4]]

puts "379 ========== a fresh interpreter =========="
set ip [interp create]
$ip eval [list ::tcl::tm::path add [file dirname [info script]]]
$ip eval {package require colon}
$ip eval {set pt {3 4 5}}
set out {}
lappend out [$ip eval {= pt -> (a, b, c); list $a $b $c}]
lappend out [$ip eval {= pt => (d, e, f); list $d $e $f}]
lappend out [$ip eval {= 1 ? 2 : 3}] [$ip eval {= 1 && 0}] [$ip eval {= 0 || 2}]
lappend out [$ip eval {= ++n}] [$ip eval {= n++}] [$ip eval {= n}]
lappend out [$ip eval {proc= f {x} { = x * 2 }; f 4}]
interp delete $ip
verify "fresh interpreter, tuples first" {{3 4 5} {3 4 5} 2 0 1 1 1 2 8} $out

puts "395 ========== Preprocessor output, the debug hook, nesting, defaults =========="
proc esc53 {s} {string map [list \n {\n}] $s}
proc= ms1 {x} {
    = {a = x + 1; b = a * 2}
}
set body [info body ms1]
verify "multi statement braces use locals" {8 0 0 1 1 1} [list [ms1 3] [string match *loadStk* $body] [string match *storeStk* $body] [string match {*store a*} $body] [string match {*store b*} $body] [string match {*load x*} $body]]
set lst {p q r}
proc= ms2 {l} {
    = l -> w2
    return $w2
}
verify "single name target" {{p q r} {p q r} {p q r} {p q r} {x y} 1 1} \
    [list [= lst -> w] $w [= lst => v] $v [ms2 {x y}] [string match {*store w2*} [info body ms2]] [string match {*load l*} [info body ms2]]]
verify "tuple RHS is a full expression" {{1 2} 1 2 {1 2}} [list [= (a, b) = c = list(1, 2)] $a $b $c]
verify "1e- is not a number" "Calc: expected start of expression but found '1e'" [tryerr {= 1e-}]
verify "transform= defaults" {\n    if {0} {     = y = x + 1 } { tcl::unsupported::assemble {push y; push x; loadStk; push 1; add; storeStk; } }\n\n} [esc53 [Calc::transform= {} "\n    = y = x + 1\n"]]
verify "transform= unbraced" {{\n    tcl::unsupported::assemble {load x; push 1; add; store y; }\n\n} {\n    if {0} {     = y = x + 1 } { tcl::unsupported::assemble {load x; push 1; add; store y; } }\n\n}} \
    [list [esc53 [Calc::transform= {} "\n    = y = x + 1\n" 1 0]] [esc53 [Calc::transform= {} "\n    = y = x + 1\n" 1 1]]]
verify "transform= braced" {{\n    tcl::unsupported::assemble {load x; store a; pop; load a; push 1; add; store b; }\n\n} {\n    if {0} {     = {z = x} } { tcl::unsupported::assemble {load x; store z; } }\n\n} {\n    tcl::unsupported::assemble {load x; push 1; add; store a; pop; load a; push 2; mult; store b; }\n\n}} \
    [list [esc53 [Calc::transform= {} "\n    = \{\n      a = x;\n      b = a + 1\n    \}\n" 1 0]] \
          [esc53 [Calc::transform= {} "\n    = \{z = x\}\n" 1 1]] \
          [esc53 [Calc::transform= {} "\n    = \{a = x + 1; b = a * 2\}\n" 1 0]]]
verify "transform= inline braced" {{\n    set v [tcl::unsupported::assemble {load x; push 1; add; }] ; set w 5\n\n} {\n    set v [if {0} {[: { x + 1 }]} {tcl::unsupported::assemble {load x; push 1; add; }}]\n\n}} \
    [list [esc53 [Calc::transform= {} "\n    set v \[: \{\n x + 1\n \}\] ; set w 5\n" 1 0]] \
          [esc53 [Calc::transform= {} "\n    set v \[: \{ x + 1 \}\]\n" 1 1]]]
verify "transform= inline" {{\n    list [tcl::unsupported::assemble {load x; push 1; add; }] and [tcl::unsupported::assemble {load x; push 2; mult; }] end\n\n} {\n    list [if {0} {= x + 1} {tcl::unsupported::assemble {load x; push 1; add; }}] and [if {0} {: x * 2} {tcl::unsupported::assemble {load x; push 2; mult; }}] end\n\n} {[tcl::unsupported::assemble {load x; push 1; add; }] tail\n\n}} \
    [list [esc53 [Calc::transform= {} "\n    list \[= x + 1\] and \[: x * 2\] end\n" 1 0]] \
          [esc53 [Calc::transform= {} "\n    list \[= x + 1\] and \[: x * 2\] end\n" 1 1]] \
          [esc53 [Calc::transform= {} "\[= x + 1\] tail\n" 1 0]]]
verify "transform= passthrough" {\n    set q 1\n    puts hi\n\n} [esc53 [Calc::transform= {} "\n    set q 1\n    puts hi\n" 1 0]]

rename Calc::debug Calc::debug.saved
proc Calc::debug {args} {lappend ::dbgcalls [lindex $args 0]}
set ::dbgcalls {}
set rc [catch {
    proc= dbg1 {x} { = x + 1 } 2
    proc= dbg1n {x} { = x + 1 } -2
    proc= dbg1x {x} { = x + 1 } 1
    proc= dbg1z {x} { = x + 1 } 0
    calc= dbg2 { = zz = 1 } 2
    calc= dbg2x { = zz = 1 } 1
    oo::class create DC53
    oo::define DC53 { method= m {x} { = x + 1 } 2 }
    oo::define DC53 { method= m1 {x} { = x + 1 } 1 }
    proc dbg4 {x} { = x + 1 }
    Calc::transformProcs -preserve 2 dbg4
    Calc::transformProcs -preserve 1 dbg4
} msg]
rename Calc::debug {}
rename Calc::debug.saved Calc::debug
verify "debug above 1 calls Calc::debug" {0 {dbg1 dbg1n dbg2 m ::dbg4}} [list $rc $::dbgcalls]

oo::class create DC53b
oo::define DC53b { method= m1 {x} { = x + 1 } 1 }
oo::define DC53b { method= m0 {x} { = x + 1 } 0 }
oo::define DC53b { method= md {x} { = x + 1 } }
set out {}
foreach m {m1 m0 md} {
    set b [lindex [info class definition DC53b $m] 1]
    lappend out [string match *if* $b] [string match {*load x*} $b] [string match *loadStk* $b]
}
lappend out [[DC53b new] md 4]
verify "method= output" {1 1 0 0 1 0 1 1 0 5} $out

proc tp53d {x} { = x + 1 }
Calc::transformProcs tp53d
verify "transformProcs: each match once" {{::tp49::a ::tp49::b} {::tp49::a ::tp49::b} 1 2} \
    [list [Calc::transformProcs -listonly ::tp49::a ::tp49::*] [Calc::transformProcs -listonly ::tp49::b ::tp49::a] [string match {*if {0}*} [info body tp53d]] [tp53d 1]]
verify "transformProcs: -preserve and no pattern" {1 {Calc::transformProcs: no proc name pattern(s) given} 1 {Calc::transformProcs: no proc name pattern(s) given}} \
    [list [catch {Calc::transformProcs -preserve 0} m1] $m1 [catch {Calc::transformProcs -preserve 0 -listonly} m2] $m2]
namespace eval ::tq53 {
    proc pt {} {}
    namespace eval a { proc pa {} {}
        namespace eval b { proc pb {} {}
            namespace eval c { proc pc {} {} } }
        namespace eval d { proc pd {} {} } }
    namespace eval e { proc pe {} {} }
    namespace eval f { namespace eval g { proc pg {} {} } }
}
set all [Calc::AllProcsRecursive]
set out {}
foreach p {::tq53::pt ::tq53::a::pa ::tq53::a::b::pb ::tq53::a::b::c::pc ::tq53::a::d::pd ::tq53::e::pe ::tq53::f::g::pg} {
    lappend out [expr {[lsearch -exact $all $p] >= 0}]
}
verify "AllProcsRecursive deep and sibling namespaces" {1 1 1 1 1 1 1} $out
namespace delete ::tq53

puts "339 ========== short circuit && and || =========="
set ::scn 0
proc scbump {v} {incr ::scn; return $v}
verify "1 || 1/0" 1 [= 1 || 1/0]
verify "0 && 1/0" 0 [= 0 && 1/0]
set scn 0; = scbump(1) || scbump(1)
verify "|| right side skipped" 1 $scn
set scn 0; = scbump(0) || scbump(1)
verify "|| right side run" 2 $scn
set scn 0; = scbump(0) && scbump(1)
verify "&& right side skipped" 1 $scn
set scn 0; = scbump(1) && scbump(1)
verify "&& right side run" 2 $scn
set scn 0; = scbump(0) || scbump(0) || scbump(1) || scbump(1)
verify "|| chain stops at first true" 3 $scn
set x 0; set i 5; = 1 || (x = 9); = 0 && i++
verify "skipped assignment and ++" {0 5} [list $x $i]
set a 5; set b 1; set c 0; set d 1
proc scf {a b} {incr ::scn; expr {$a > $b}}
proc scg {c d} {incr ::scn; expr {$c > $d}}
set scn 0
verify "f(a,b) || g(c,d)" {1 1} [list [= scf(a,b) || scg(c,d)] $scn]
proc= scp {a b} {: a || b}
proc  sce {a b} {expr {$a || $b}}
proc scins {name} {set o {}; foreach l [split [::tcl::unsupported::disassemble proc $name] \n] {if {[regexp {^\s+\(\d+\) (\w+?)\d* } $l -> op]} {lappend o $op}}; set o}
verify "same instructions as expr" [scins sce] [scins scp]

puts "340 ========== nested ternary is right associative =========="
set bad {}
foreach a {0 1} {foreach b {0 1} {foreach c {0 2} {
    set d 7; set e 9
    foreach {t et} {
        {a ? b : c ? d : e}  {$a ? $b : $c ? $d : $e}
        {a ? b ? c : d : e}  {$a ? $b ? $c : $d : $e}
        {a || b ? c : d ? e : 1} {$a || $b ? $c : $d ? $e : 1}
    } {
        if {[= $t] != [expr $et]} {lappend bad [list $t $a $b $c]}
    }
}}}
verify "nested ternary against expr" {} $bad
proc= sgn3 {n} {: n < 0 ? -1 : n == 0 ? 0 : 1}
verify "sign by nested ternary in proc=" {-1 0 1} [list [sgn3 -5] [sgn3 0] [sgn3 8]]

puts "\n=========================================="
if {$failures == 0} {
    puts "ALL $::tests TESTS PASSED!"
} else {
    puts "TOTAL FAILURES: $failures"
}
puts "=========================================="
puts "\n========== timing tests ========== "

puts "expr  [time {test0 20} 10000]"
puts "proc  [time {test1 20} 10000]"
puts "proc= [time {test2 20} 10000]"

} ;# end doalltests

} err_code details] {
    puts "$err_code \n\n\n$details"
}
    unset -nocomplain details
    unset -nocomplain err_code




