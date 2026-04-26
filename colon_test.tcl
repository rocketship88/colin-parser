#
# test suite for colon module 
#
::tcl::tm::path add [file dirname [info script]]
package require colon
catch {console show}
# --------------------------- knobs -------------------------------------------
set doAllTests        t   ;# all verify tests
set putsAllTests      t  ;# puts on each verify test
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

puts "144 ========== Gather Function =========="
proc tcl::mathfunc::gather {args} { list {*}$args }

set result [: gather(10, 20, 30)]
verify "gather(10, 20, 30)" [list 10 20 30] $result

set x 5
set y 10
set coords [: gather(x*2, y*2)]
verify "gather(x*2, y*2)" [list [expr {$x*2}] [expr {$y*2}]] $coords

puts "146 ========== Complex: Assignment + Gather =========="
: {px = 100 ; py = 200}
set points [: gather(px, py, px+10, py+10)]
verify "gather with assigned vars" [list 100 200 110 210] $points




puts "147 ========== Logical AND (&&) Operator =========="
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

puts "153 ========== Logical OR (||) Operator =========="
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


puts "159 ========== Multi-Dimensional Arrays =========="
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
puts "164 ========== 3D Arrays =========="
array set cube {
    0,0,0 1000  0,0,1 1001  0,1,0 1010  0,1,1 1011
    1,0,0 2000  1,0,1 2001  1,1,0 2010  1,1,1 2011
}
set i 1
set j 0
set k 1
verify "cube(i,j,k)" [expr {$cube($i,$j,$k)}] [= cube(i,j,k)]
verify "cube(0,0,0) + cube(1,1,1)" [expr {$cube(0,0,0) + $cube(1,1,1)}] [= cube(0,0,0) + cube(1,1,1)]

puts "166 ========== Multi-Dim with Expressions =========="
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

    puts "183 ========== 9.x Relative Namespace Lookup =========="
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
    puts "183 ========== 8.6 Global Namespace Lookup =========="
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




