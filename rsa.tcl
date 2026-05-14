::tcl::tm::path add [file dirname [info script]]
package require colon
catch {console show}
#con+
#!/usr/bin/env wish
# =============================================================
#  RSA Interactive Tutor  -  Tcl/Tk + native bignum integers
#  Run with:  wish rsa_demo.tcl   (or  tclsh rsa_demo.tcl)
# =============================================================

package require Tk

# -------------------------------------------------------------
#  MATH LAYER  (pure Tcl, uses native arbitrary-precision ints)
# -------------------------------------------------------------

# Greatest common divisor (iterative Euclidean)
proc= gcd {a b} {
    incr ::_calls_gcd=
    : a0 = a ' b0 = b
    while {[: b != 0]} {
        lassign [: list(b ,a % b) ] a b
    }
    
    return $a
}  

# Extended Euclidean algorithm
# Returns list {g x y}  where  a*x + b*y = g = gcd(a,b)
proc= extended_gcd {a b} {
    incr ::_calls_extended_gcd=
    if {[: b == 0]} { return [: list(a, 1, 0)] }
    lassign [extended_gcd $b [: a % b] ] g x y
#    return [  list $g $y [expr {$x - ($a / $b) * $y}]   ]
    return [:  list( g, y, x - (a / b) * y )   ]
}  

# Modular inverse of e (mod phi)  - errors if none exists
proc= mod_inverse {e phi} {
    incr ::_calls_mod_inverse
    lassign [extended_gcd $e $phi] g x
    if {[: g != 1 ]} { error "gcd($e,$phi) = $g != 1, no inverse exists" }
#    return [expr {($x % $phi + $phi) % $phi}]
    return [: (x % phi + phi) % phi ]
}  

# Fast modular exponentiation via repeated squaring
# Handles Tcl bignums natively - no library needed
# Fast modular exponentiation via repeated squaring
# Handles Tcl bignums natively - no library needed
proc= mod_exp {base exp m} {
    incr ::_calls_mod_exp=
    : result = 1
    : base = base % m
    while {[: exp > 0]} {
        if {[: exp & 1]} {
            : result = result * base % m
        }
        : exp  = exp >> 1
        : base = base * base % m
    }
    return $result
}

# Small primes for trial-division pre-screen
set ::small_primes {
    2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
    73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151
    157 163 167 173 179 181 191 193 197 199 211 223 227 229 233
    239 241 251 257 263 269 271 277 281 283 293 307 311 313 317
}

# Miller-Rabin witness test for a single base a
proc= miller_rabin_witness {n a} {
    incr ::_calls_miller_rabin_witness=
    # Write n-1 = 2^r * d
    : d = n - 1
    : r = 0
    while {[: d % 2 == 0]} { 
        : d = d / 2; 
        incr r 
    }
 
    set x [mod_exp $a $d $n]
    if {[: x == 1 || x == n - 1]} { return 1 }   ;# probably prime
    for {set i 0} {[: i < r - 1]} {incr i} {
        set x [mod_exp $x 2 $n]
        if {[: x == n - 1]} { return 1 }           ;# probably prime
    }
    return 0                                       ;# definitely composite
}  
 
# Primality test: trial division pre-screen + Miller-Rabin
# rounds=20 gives false-prime probability < 4^-20 ~ 10^-12
proc= is_prime {n {rounds 20}} {
    incr ::_calls_is_prime=
    if {[: n < 2]}      { return 0 }
    if {[: n == 2]}     { return 1 }
    if {[: n % 2 == 0]} { return 0 }
    # Trial division by small primes (fast reject for most composites)
    foreach p $::small_primes {
        if {[: n == p]}     { return 1 }
        if {[: n % p == 0]} { return 0 }
    }
    # Miller-Rabin rounds with random bases
    for {set i 0} {[: i < rounds]} {incr i} {
        # Generate a random witness in range [2, n-2].
        # Cannot use int(rand()*n) - int() is limited to 64-bit.
        # Instead, generate a random bignum the same width as n
        # and reduce it into the valid range.
        set nbytes [expr {([string length [format %llx $n]] + 1) / 2 + 1}]
        set raw [os_random_bytes $nbytes]
        binary scan $raw cu* blist
        set a 0
        foreach b $blist { 
            : a = (a << 8) | b 
        }
        : a = 2 + (a % (n - 3))
        if {![miller_rabin_witness $n $a]} { return 0 }
    }
    return 1
}  


# Probe once at startup which entropy source is available.
# Priority: TWAPI CryptGenRandom > /dev/urandom > rand() fallback
proc= detect_entropy_source {} {
    incr ::calls_detect_entropy_source
    # 1. Try TWAPI CryptoAPI - CryptGenRandom (advapi32.dll)
    #    CryptAcquireContext flags: CRYPT_VERIFYCONTEXT = 0xF0000000
    #    means we want random bytes only - no key container needed.
    if {![catch {package require twapi}]} {
        if {![catch {
            set h [twapi::CryptAcquireContext "" "" 1 0xF0000000]
            twapi::CryptGenRandom $h 4
            twapi::CryptReleaseContext $h 0
        }]} {
            puts "Entropy source: TWAPI CryptGenRandom (Windows CryptoAPI)"
            return twapi_crypt
        } else {
            puts "TWAPI loaded but CryptGenRandom probe failed - check error above"
        }
    }

    # 2. /dev/urandom (Linux / macOS)
    if {[file exists /dev/urandom]} {
        puts "Entropy source: /dev/urandom"
        return urandom
    }

    # 3. Last resort
    puts "Entropy source: clock-seeded rand() (not cryptographically secure)"
    expr {srand([clock milliseconds] ^ [clock microseconds])}
    return rand_fallback
}
#------------------------------------
source "D:/stuff/vw debugging.tcl"
set ::___zz___(sel)	 smart
cooltip::tooltip off
set ::___zz___(lbp-tips) 0

#interp alias {} v {} vw+ ;# shorthands 
#interp alias {} g {} go+
#interp alias {} u {} util+
#interp alias {} i {} instrument+
#------------------------------------

#set ::entropy_source rand_fallback 
set ::entropy_source  [detect_entropy_source]

# Read n cryptographically random bytes using whichever source is available.
proc= os_random_bytes {n} {
    incr ::_calls_os_random_bytes
    if       { $::entropy_source eq "twapi_crypt" } {
            # Acquire a PROV_RSA_FULL context with CRYPT_VERIFYCONTEXT.
            # This gives us access to the Windows CSPRNG without needing
            # a key container - exactly what you want for raw random bytes.
            incr ::_calls_os_random_bytes1
            set h [twapi::CryptAcquireContext "" "" 1 0xF0000000]
            set raw [twapi::CryptGenRandom $h $n]
            twapi::CryptReleaseContext $h 0
            return $raw
    } elseif { $::entropy_source eq "urandom" } {
            incr ::_calls_os_random_bytes2
            set fd [open /dev/urandom rb]
            set raw [read $fd $n]
            close $fd
            return $raw
    } else {
            incr ::_calls_os_random_bytes3
            set bytes {}
            for {set i 0} { $i < $n } {incr i} {
                lappend bytes [: int(rand() * 256)]
            }
            return [binary format c* $bytes]
    }
}
#instrument+  os_random_bytes  
# Generate a random odd number of exactly `bits` bits using OS entropy.
# Top bit forced to 1 (ensures exact bit length).
# Bottom bit forced to 1 (ensures odd).
proc= random_odd {bits} {
    incr ::_calls_random_odd=
    : nbytes = (bits + 7) / 8
    set raw [os_random_bytes $nbytes]

    # Convert raw bytes to a big integer
    set n 0
    binary scan $raw cu* byte_list
    foreach b $byte_list {
        : n =  (n << 8) | b
    }

    # Mask down to exactly `bits` bits, then force top and bottom bits
    : n = n & ((1 << bits) - 1)   ;# mask to bit length
    : n = n | (1 << (bits - 1))   ;# set top bit
    : n = n | 1                   ;# set bottom bit (odd)
    return $n
}  
#instrument+  random_odd  
# Generate a random prime of exactly `bits` bits
proc= generate_prime {bits} {
    incr ::_calls_generate_prime
    : attempts = 0
    while 1 {
        set candidate [random_odd $bits]
        incr attempts
        if {[is_prime $candidate]} {
            puts "  Found prime after $attempts attempt(s): $candidate"
            return $candidate
        }
    }
}

# -------------------------------------------------------------
#  RSA CORE
# -------------------------------------------------------------

# Generate RSA key components from two primes
proc= rsa_keygen {p q} {
    incr ::_calls_rsa_keygen=
    if {![is_prime $p]}  { error "p = $p is not prime" }
    if {![is_prime $q]}  { error "q = $q is not prime" }
    if {[: p == q ]}        { error "p and q must be distinct" }

#    set n   [expr {$p * $q}]
#    set phi [expr {($p - 1) * ($q - 1)}]
    
    : { n = p * q;
        phi = (p-1) * (q-1)
      }

    # Choose public exponent e - prefer 65537 (common in practice)
    foreach candidate {65537 257 17 5 3} {
        if {$candidate < $phi && [gcd $candidate $phi] == 1} {
            set e $candidate
            break
        }
    }
    # Fall back: search from 3 upward
    if {![info exists e]} {
        set e 3
        while {[: e < phi ]} {
            if {[gcd $e $phi] == 1} break
            incr e 2
        }
    }

    set d [mod_inverse $e $phi]
    return [list $n $e $d $phi]
} 

# -------------------------------------------------------------
#  CALLBACK: Generate Random Primes
# -------------------------------------------------------------
proc= do_gen_primes {} {
    incr ::_calls_do_gen_primes
    set bits [string trim [.bentry get]]
    if {![string is integer -strict $bits] || $bits < 4} {
        puts "ERROR: bit size must be at least 4"
        return
    }
    puts ""
    puts "PRIME GENERATION  ($bits bits each)"
    puts "-----------------------------------------"
    puts "  Strategy: random candidate -> trial division"
    puts "            pre-screen -> Miller-Rabin ($bits-bit range)"
    puts "  Searching for p..."
    set p [generate_prime $bits]
    puts "  Searching for q..."
    set q [generate_prime $bits]
    # Ensure they differ
    while {[: q == p ]} {
        puts "  p == q, regenerating q..."
        set q [generate_prime $bits]
    }
    puts "  p = [string range $p 0 499]...[string length $p]"
    puts "  q = [string range $q 0 499]...[string length $q]"
    puts "-----------------------------------------"
    # Fill the prime entry boxes
    .pentry delete 0 end
    .pentry insert 0 $p
    .qentry delete 0 end
    .qentry insert 0 $q
    puts "  Entries filled. Click 'Generate Keys' to continue."
} 

# -------------------------------------------------------------
#  STATE  (shared across GUI callbacks)
# -------------------------------------------------------------
array set S {
    n 0  e 0  d 0  phi 0
    cipher 0  str_cipher {}  str_plain {}
}

# -------------------------------------------------------------
#  HELPER: write a line to the console
# -------------------------------------------------------------
proc log {msg {tag {}}} {
    if { [string length $msg] > 500 } {
    	puts "[string range $msg 0 499]...[string length $msg]"
    } else {
    	puts $msg 	
    } 
}

proc log_sep {} { puts "-----------------------------------------" }

# -------------------------------------------------------------
#  CALLBACK: Generate Keys
# -------------------------------------------------------------
proc do_keygen {} {
    incr ::calls_do_keygen
    global p q
    global S
    set p [string trim [.pentry get]]
    set q [string trim [.qentry get]]

    if {![string is integer -strict $p] || ![string is integer -strict $q]} {
        log "ERROR: p and q must be integers. if key size > 32 must use tcl 9.x" err
        return
    }

    if {[catch {rsa_keygen $p $q} result]} {
        log "ERROR: $result" err
        return
    }

    lassign $result S(n) S(e) S(d) S(phi)

    log "KEY GENERATION" head
    log_sep
    log "  p          = $p"
    log "  q          = $q"
    log "  n  = p*q   = $S(n)"
    log "  phi(n)       = (p-1)(q-1) = $S(phi)"
    set gcd_check [gcd $S(e) $S(phi)]
    set ed_check  [expr {($S(e) * $S(d)) % $S(phi)}]
    log "  e  (public) = $S(e)   (gcd(e,phi) = $gcd_check)"
    log "  d  (private)= $S(d)   (e*d mod phi = $ed_check)"
    log_sep
    log "  Public  key : (e=$S(e),  n=$S(n))" key
    log "  Private key : (d=$S(d), n=$S(n))" key
    log_sep
    log "Keys ready.  Enter a message number < [string range $S(n) 0 499]...[string length $S(n)] below." ok

    # Unlock encrypt section
    .mentry  configure -state normal
    .ebtn    configure -state normal
    .dbtn    configure -state normal
    .mentry  delete 0 end
    .clabel  configure -text "-"
    .mlabel  configure -text "-"
}

# -------------------------------------------------------------
#  CALLBACK: Encrypt
# -------------------------------------------------------------
proc= do_encrypt {} {
    incr ::_calls_do_encrypt
    global S
    set m [string trim [.mentry get]]

    if {![string is integer -strict $m] || $m < 0} {
        log "ERROR: message must be a non-negative integer." err
        return
    }
    if {$m >= $S(n)} {
        log "ERROR: message ($m) must be < n ($S(n))." err
        return
    }

    set S(cipher) [mod_exp $m $S(e) $S(n)]

    log "" 
    log "ENCRYPTION" head
    log_sep
    log "  Plaintext  m = $m"
    log "  c = m^e mod n"
    log "    = ${m}^$S(e) mod $S(n)"
    log "    = $S(cipher)" key
    log_sep

    .clabel  configure -text $S(cipher)
    .dbtn    configure -state normal
}

# -------------------------------------------------------------
#  CALLBACK: Decrypt
# -------------------------------------------------------------
proc do_decrypt {} {
    incr ::calls_do_decrypt
    global S
    set recovered [mod_exp $S(cipher) $S(d) $S(n)]

    log "DECRYPTION" head
    log_sep
    log "  Ciphertext c = $S(cipher)"
    log "  m = c^d mod n"
    log "    = $S(cipher)^$S(d) mod $S(n)"
    log "    = $recovered" key
    log_sep
    if {$recovered == [string trim [.mentry get]]} {
        log "  OK: Matches original plaintext!"
    } else {
        log "  MISMATCH: something went wrong."
    }

    .mlabel configure -text $recovered
}

# -------------------------------------------------------------
#  CALLBACK: Encrypt String
# -------------------------------------------------------------
proc do_str_encrypt {} {
    incr ::calls_do_str_encrypt
    global S
    set txt [.stentry get]

    if { $S(n) == 0 } {
        log ""
        log "ERROR: generate keys first." err
        return
    }
    if {$txt eq ""} {
        log ""
        log "ERROR: enter some text to encrypt." err
        return
    }
    if { $S(n) <= 127 } {
        log ""
        log "ERROR: n=$S(n) is too small (need n > 127 for ASCII)." err
        return
    }

    set ciphers {}
    foreach ch [split $txt {}] {
        scan $ch %c code
        lappend ciphers [mod_exp $code $S(e) $S(n)]
    }
    set S(str_cipher) $ciphers
    set S(str_plain)  $txt

    set ascii_codes {}
    foreach ch [split $txt {}] { scan $ch %c c; lappend ascii_codes $c }

    log ""
    log "STRING ENCRYPTION" head
    log_sep
    log "  Plaintext  : $txt"
    log "  ASCII codes: $ascii_codes"
    log "  Ciphertext : $ciphers" key
    log_sep

    .scresult configure -state normal
    .scresult delete 1.0 end
    .scresult insert end [join $ciphers "\n"]
    .scresult configure -state normal
    .sdbtn configure -state normal
}

# -------------------------------------------------------------
#  CALLBACK: Decrypt String
# -------------------------------------------------------------
proc do_str_decrypt {} {
    incr ::calls_do_str_decrypt
    global S

    set recovered_chars {}
    set recovered_codes {}
    foreach c $S(str_cipher) {
        set code [mod_exp $c $S(d) $S(n)]
        lappend recovered_codes $code
        lappend recovered_chars [format %c $code]
    }
    set recovered [join $recovered_chars {}]

    log "STRING DECRYPTION" head
    log_sep
    log "  Ciphertext : $S(str_cipher)"
    log "  ASCII codes: $recovered_codes"
    log "  Plaintext  : $recovered" key
    log_sep
    if {$recovered eq $S(str_plain)} {
        log "  OK: Matches original text!"
    } else {
        log "  MISMATCH: something went wrong."
    }

    .sdresult configure -state normal
    .sdresult delete 1.0 end
    .sdresult insert end $recovered
    .sdresult configure -state normal
}

# -------------------------------------------------------------
#  BUILD THE GUI
# -------------------------------------------------------------
catch {console show}

wm title . "RSA Interactive Tutor - Tcl/Tk"
wm resizable . 0 0
. configure -padx 16 -pady 12

# -- Title --------------------------------------------------
label .title \
    -text "RSA Algorithm - Interactive Demo" \
    -font {Helvetica 16 bold}
pack .title -fill x -pady {0 10}

# -- Prime Generation ---------------------------------------
labelframe .genf \
    -text " Step 0 . Generate Random Primes (optional) " \
    -font {Helvetica 10 bold} \
    -relief groove -bd 2
pack .genf -fill x -pady 4

frame .genf.row
pack  .genf.row -pady 6 -padx 8

label .blbl -text "Bit size:" -font {Helvetica 10}
pack  .blbl -in .genf.row -side left -padx {8 4}

spinbox .bentry -from 4 -to 4096 -width 5 -font {Courier 11} -justify center
.bentry delete 0 end
.bentry insert 0 32
pack .bentry -in .genf.row -side left -padx {0 8}

# Mousewheel bindings for bit-size spinbox
# Windows / macOS: <MouseWheel> with delta +/-120 per notch
bind .bentry <MouseWheel> {
    set cur [.bentry get]
    set step [expr {%D > 0 ? 1 : -1}]
    set nv [expr {max(4, min(4096, $cur + $step))}]
    .bentry delete 0 end
    .bentry insert 0 $nv
}
# Linux X11: Button-4 = scroll up, Button-5 = scroll down
bind .bentry <Button-4> {
    set nv [expr {min(4096, [.bentry get] + 1)}]
    .bentry delete 0 end; .bentry insert 0 $nv
}
bind .bentry <Button-5> {
    set nv [expr {max(4, [.bentry get] - 1)}]
    .bentry delete 0 end; .bentry insert 0 $nv
}

label .blbl2 -text "(4+ bits  --  64 bits ~ 19 digits, 256 bits ~ 77 digits, 1024 bits ~ 309 digits)" \
    -font {Helvetica 9}
pack  .blbl2 -in .genf.row -side left -padx {0 8}

button .gbtn -text "Generate p & q" \
    -command do_gen_primes \
    -font {Helvetica 10 bold} \
    -padx 10 -pady 4 -cursor hand2
pack .gbtn -in .genf.row -side left -padx 8

# -- Step 1: Primes -----------------------------------------
labelframe .pqf \
    -text " Step 1 . Choose Primes p and q " \
    -font {Helvetica 10 bold} \
    -relief groove -bd 2
pack .pqf -fill x -pady 4

frame .pqf.row
pack  .pqf.row -pady 6 -padx 8

foreach {lbl wname def} {
    "p =" .pentry 61
    "q =" .qentry 53
} {
    label ${wname}lbl -text $lbl -font {Helvetica 10}
    pack  ${wname}lbl -in .pqf.row -side left -padx {8 2}

    entry $wname -width 12 -font {Courier 11}
    $wname insert 0 $def
    pack  $wname -in .pqf.row -side left -padx {0 12}
}

button .kbtn -text "Generate Keys" \
    -command do_keygen \
    -font {Helvetica 10 bold} \
    -padx 10 -pady 4 -cursor hand2
pack .kbtn -in .pqf.row -side left -padx 8

label .hint -text "Try primes: 61 & 53  /  101 & 103  /  997 & 991  /  7919 & 7907" \
    -font {Helvetica 9}
pack .hint -pady {0 4}

# -- Step 2: Encrypt a Number --------------------------------
labelframe .encf \
    -text " Step 2 . Encrypt a Number " \
    -font {Helvetica 10 bold} \
    -relief groove -bd 2
pack .encf -fill x -pady 4

frame .encf.row
pack  .encf.row -pady 6 -padx 8

label .mlbl2 -text "Message m =" -font {Helvetica 10}
pack  .mlbl2 -in .encf.row -side left -padx {8 2}

entry .mentry -width 16 -font {Courier 11} -state normal
pack  .mentry -in .encf.row -side left -padx {0 12}

button .ebtn -text "Encrypt ->" \
    -command do_encrypt \
    -font {Helvetica 10 bold} \
    -padx 10 -pady 4 -cursor hand2 -state normal
pack .ebtn -in .encf.row -side left -padx 4

frame .encf.res
pack  .encf.res -padx 16 -pady {0 6}

label .clbl   -text "Ciphertext c =" -font {Helvetica 10}
pack  .clbl   -in .encf.res -side left
label .clabel -text "-" -font {Courier 11}
pack  .clabel -in .encf.res -side left -padx 8

# -- Step 3: Decrypt a Number --------------------------------
labelframe .decf \
    -text " Step 3 . Decrypt Number " \
    -font {Helvetica 10 bold} \
    -relief groove -bd 2
pack .decf -fill x -pady 4

frame .decf.row
pack  .decf.row -pady 6 -padx 8

button .dbtn -text "Decrypt <-" \
    -command do_decrypt \
    -font {Helvetica 10 bold} \
    -padx 10 -pady 4 -cursor hand2 -state normal
pack .dbtn -in .decf.row -side left -padx 8

frame .decf.res
pack  .decf.res -padx 16 -pady {0 6}

label .rlbl   -text "Recovered m =" -font {Helvetica 10}
pack  .rlbl   -in .decf.res -side left
label .mlabel -text "-" -font {Courier 11 bold}
pack  .mlabel -in .decf.res -side left -padx 8

# -- Step 4: Encrypt a String --------------------------------
labelframe .stencf \
    -text " Step 4 . Encrypt a Text String " \
    -font {Helvetica 10 bold} \
    -relief groove -bd 2
pack .stencf -fill x -pady 4

frame .stencf.row
pack  .stencf.row -pady 6 -padx 8

label .stlbl -text "Text =" -font {Helvetica 10}
pack  .stlbl -in .stencf.row -side left -padx {8 2}

entry .stentry -width 24 -font {Courier 11}
.stentry insert 0 "Hello now is the time for all good men to come to the aid of their party."
pack  .stentry -in .stencf.row -side left -padx {0 12}

button .sebtn -text "Encrypt ->" \
    -command do_str_encrypt \
    -font {Helvetica 10 bold} \
    -padx 10 -pady 4 -cursor hand2
pack .sebtn -in .stencf.row -side left -padx 4

frame .stencf.res
pack  .stencf.res -fill x -padx 16 -pady {0 6}

label .sclbl -text "Ciphertext =" -font {Helvetica 10}
pack  .sclbl -in .stencf.res -side left

text .scresult -width 40 -height 3 -font {Courier 9} \
     -relief sunken -bd 2 -state normal -wrap word
pack .scresult -in .stencf.res -side left -padx 8 -pady 4

# -- Step 5: Decrypt a String --------------------------------
labelframe .stdecf \
    -text " Step 5 . Decrypt Back to Text " \
    -font {Helvetica 10 bold} \
    -relief groove -bd 2
pack .stdecf -fill x -pady 4

frame .stdecf.row
pack  .stdecf.row -pady 6 -padx 8

button .sdbtn -text "Decrypt <-" \
    -command do_str_decrypt \
    -font {Helvetica 10 bold} \
    -padx 10 -pady 4 -cursor hand2 -state normal
pack .sdbtn -in .stdecf.row -side left -padx 8

frame .stdecf.res
pack  .stdecf.res -fill x -padx 16 -pady {0 6}

label .sdlbl -text "Recovered =" -font {Helvetica 10}
pack  .sdlbl -in .stdecf.res -side left

text .sdresult -width 24 -height 2 -font {Courier 11 bold} \
     -relief sunken -bd 2 -state normal
pack .sdresult -in .stdecf.res -side left -padx 8 -pady 4
