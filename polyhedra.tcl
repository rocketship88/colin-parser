#!/bin/env tclsh
# from the tcl wiki
# polyhedra.tcl 
# Author:      Gerard Sookahet
# Date:        30 Mai 2005 
# Description: Rotating polyhedra using a 'standard' tk canvas.
#              Flat shading and wireframe mode.

package require Tcl 8.5-
package require Tk 8.4-

#tcl::tm::path add {path/to/colon-module/dir} ;# point this to the module directory
::tcl::tm::path add [file dirname [info script]] ;# works if script and moudle in same directory
package require colon

bind all <Escape> {exit}

proc Barycenter {lcoords} {
    : { X = 0; Y = 0; n = llength(lcoords) }

    foreach vtx $lcoords {
        : { (x, y) = vtx; X = X + x; Y = Y + y }
    }
    : list(X/n, Y/n)
}

proc CrossProduct {x1 y1 z1 x2 y2 z2} {
    : {
        list(y1*z2 - y2*z1,
             z1*x2 - z2*x1,
             x1*y2 - x2*y1)
    }
}

proc DotProduct {x1 y1 z1 x2 y2 z2} {
    : x1*x2 + y1*y2 + z1*z2
}

proc MatrixVectorProduct {M V} {
    : {
        (x, y, z) = V;
        ((m00,m01,m02), (m10,m11,m12), (m20,m21,m22)) = M;
        list(m00*x + m10*y + m20*z,
             m01*x + m11*y + m21*z,
             m02*x + m12*y + m22*z)
    }
}

proc MatrixProduct {M1 M2} {
    set M {{0 0 0 0} {0 0 0 0} {0 0 0 0} {0 0 0 0}}

    for {set i 0} {$i<4} {incr i} {
        for {set j 0} {$j<4} {incr j} {
            : s = 0
            for {set k 0} {$k<4} {incr k} {
                : s = s + lindex(M1, i, k) * lindex(M2, k, j)
            }
            lset M $i $j $s
        }
    }
    return $M
}

proc MatrixRotation { ax ay az } {
    #puts "ax= |$ax| ay= |$ay| az= |$az| "
  : {
    # Rotation matrix around X axis with angle ax
    Mx = ((1, 0        , 0       , 0), 
          (0, cos( ax) , sin( ax), 0),
          (0, -sin( ax), cos( ax), 0), 
          (0, 0        , 0       , 1));
    # Rotation matrix around Y axis with angle ay
    My = ((cos( ay), 0, -sin( ay), 0), 
          (0       , 1, 0        , 0),
          (sin( ay), 0, cos( ay) , 0), 
          (0       , 0, 0        , 1));
    # Rotation matrix around Z axis with angle az
    Mz = ((cos( az) , sin( az), 0, 0), 
          (-sin( az), cos( az), 0, 0),
          (0        , 0       , 1, 0), 
          (0        , 0       , 0, 1));
    

      MatrixProduct( MatrixProduct( Mx, My), Mz)
    }
} 

# Compute normal vector and norm for each face
# -------------------------------------------------------------------
proc NormalVector {lvtx lcnx} {
    set lnv {} 
    set lmv {} 
  
    foreach face $lcnx {
        : {
            (i0, i1, i2) = face;
            (x0, y0, z0) = lindex(lvtx, i0);
            (x1, y1, z1) = lindex(lvtx, i1);
            (x2, y2, z2) = lindex(lvtx, i2);
            (nx, ny, nz) = CrossProduct(x1-x0, y1-y0, z1-z0, x2-x1, y2-y1, z2-z1)
        }
        lappend lnv [: (nx, ny, nz)]
        lappend lmv [: DotProduct(nx, ny, nz, nx, ny, nz)]
    }
    return [list $lnv $lmv]
}

# 2D projection
# -------------------------------------------------------------------
proc Projection {x y z M} {
    global scx scy vdist

    : {
        ((m00,m01,m02), (m10,m11,m12), (m20,m21,m22)) = M;
        nx = m00*x + m10*y + m20*z;
        ny = m01*x + m11*y + m21*z;
        nz = (m02*x + m12*y + m22*z + 10)/vdist;
        list(nx/nz + scx/2.0, ny/nz + scy/2.0)
    }
}

# Apply transformations to vertex coordinates
# -------------------------------------------------------------------
proc Transformations {lvtx lnv} {
    global ax ay az 
    set lnew {}
    set lvn {}
    # Compute matrix rotation
    : M = MatrixRotation(ax, ay, az)
    set i 0
    # Apply projection
    foreach vtx $lvtx {
        : (x, y, z) = vtx
        lappend lnew [: Projection(x, y, z, M)]
        incr i
    }
    # Normal vector rotation
    foreach v $lnv {lappend lvn [: MatrixVectorProduct(M, v)]}
  
    return [list $M $lnew $lvn]
}

# Compute color entensity for each face
# -------------------------------------------------------------------
proc Intensity {lnv lmv lvv} {
    set lclr {}
    : { (lx, ly, lz) = lvv; v = DotProduct(lx, ly, lz, lx, ly, lz) }
    set i 0
    foreach nv $lnv {
        : {
            (nx, ny, nz) = nv;
            a = DotProduct(nx, ny, nz, lx, ly, lz);
            b = sqrt(lindex(lmv, i) * v);
            clr = round(31*(a/b))
        }
        lappend lclr [: clr < 0 ? 31 : 32 - clr]
        incr i
    }
    return $lclr
}

# Start the display and rotation loop
# -------------------------------------------------------------------
proc DisplayModel {w s} {
    global stop
    global display
    global ax ay az tx ty tz
    global form
  
    $w.c delete all
    set stop 0
    global iterations
    : (ax, ay, az, tx, ty, tz) = (0.2, 0.1, 0.3, 0, 0, 0)
  
    set ::curmodel $s   ;# remembered so Shaded/Wireframe can restart it
    set d $display
    foreach {t lvtx lcnx lclr} [ReadData $s] {}
    $w.c create text 10 10 -anchor w -tags label -text $t \
        -fill [expr {$::bggray < 128 ? "white" : "black"}]
    foreach {lnv lmv} [NormalVector $lvtx $lcnx] {}
  
    set lpoly [DisplayInit $w $d $lcnx $lclr]

    after cancel $::run
    set ::run [after 0 [list Display$d $w $lpoly $lvtx $lcnx $lnv $lmv]]
}

# Data structure for models with vertices and connectivity
# -------------------------------------------------------------------
proc ReadData { n } {
  
    set lvtx {}
    set lcnx {}
    set lclr {}
    set txt ""
    
    switch $n {
        tetrahedron {
            set txt "tetrahedron: 4 faces 4 vertices 5 edges" 
            set a [expr {1.0/sqrt(3.0)}]
            set lvtx [list [list $a $a $a] [list $a -$a -$a] \
                                [list -$a $a -$a] [list -$a -$a $a]]
            set lcnx {{0 3 1} {2 0 1} {3 0 2} {1 3 2}}
        }
      
        cube {
            set txt "cube: 6 faces 8 vertices 12 edges"
            set lvtx {{0.7 0.7 0.7} {-0.7 0.7 0.7} {-0.7 -0.7 0.7} {0.7 -0.7 0.7} 
                         {0.7 0.7 -0.7} {-0.7 0.7 -0.7} {-0.7 -0.7 -0.7} {0.7 -0.7 -0.7}}
            set lcnx {{4 7 6 5} {0 1 2 3} {3 2 6 7} {4 5 1 0} {0 3 7 4} {5 6 2 1}}
        }
      
        octahedron {
            set txt "octahedron 8 faces  6 vertices 16 edges"
            set lvtx {{1 0 0} {0 1 0} {-1 0 0} {0 -1 0} {0 0 1} {0 0 -1}}
            set lcnx {{0 1 4} {1 2 4} {2 3 4} {3 0 4}
                         {1 0 5} {2 1 5} {3 2 5} {0 3 5}}
        }
      
        dodecahedron {
            set txt "dodecahedron 12 faces 20 vertices 30 edges"
            : {
                s3 = sqrt(3);
                s5 = sqrt(5);
                alpha = sqrt(2.0/(3 + s5))/s3;
                beta = (1.0 + sqrt(6.0/(3 + s5) - 2 + 2*sqrt(2.0/(3.0 + s5))))/s3;
                gamma = 1.0/s3
            }
            set lvtx [list \
                [list -$alpha 0 $beta] \
                [list  $alpha 0 $beta] \
                [list -$gamma -$gamma -$gamma] \
                [list -$gamma -$gamma  $gamma] \
                [list -$gamma  $gamma -$gamma] \
                [list -$gamma  $gamma  $gamma] \
                [list  $gamma -$gamma -$gamma] \
                [list  $gamma -$gamma  $gamma] \
                [list  $gamma  $gamma -$gamma] \
                [list  $gamma  $gamma  $gamma] \
                [list  $beta $alpha 0] \
                [list  $beta -$alpha 0] \
                [list -$beta $alpha  0] \
                [list -$beta -$alpha 0] \
                [list -$alpha 0 -$beta] \
                [list  $alpha 0 -$beta] \
                [list  0 $beta $alpha] \
                [list  0 $beta -$alpha] \
                [list  0 -$beta $alpha] \
                [list  0 -$beta -$alpha]]
            set lcnx {{0 1 9 16 5} {1 0 3 18 7} {1 7 11 10 9} {11 7 18 19 6} 
                         {8 17 16 9 10} {2 14 15 6 19} {2 13 12 4 14} {2 19 18 3 13} 
                         {3 0 5 12 13} {6 15 8 10 11} {4 17 8 15 14} {4 12 5 16 17}} 
        } 
      
        icosahedron {
            set txt "icosahedron: 20 faces 12 vertices 30 edges"
            set X 0.525731112119133606
            set Z 0.850650808352039932
            set lvtx [list [list -$X 0.0 $Z] [list $X 0.0 $Z] [list -$X 0.0 -$Z] \
                                [list $X 0.0 -$Z] [list 0.0 $Z $X] [list 0.0 $Z -$X] \
                                [list 0.0 -$Z $X] [list 0.0 -$Z -$X] [list $Z $X 0.0] \
                                [list -$Z $X 0.0] [list $Z -$X 0.0] [list -$Z -$X 0.0]]
            set lcnx {{4 0 1} {9 0 4} {5 9 4} {5 4 8}
                         {8 4 1} {10 8 1} {3 8 10} {3 5 8}
                         {2 5 3} {7 2 3} {10 7 3} {6 7 10}
                         {11 7 6} {0 11 6} {1 0 6} {1 6 10}
                         {0 9 11} {11 9 2} {2 9 5} {2 7 11}}
        }
    }
  
    for {set i 0} {$i <= [llength $lcnx]} {incr i} {
        lappend lclr "0000[format %2.2x 255]"
    }
    return [list $txt $lvtx $lcnx $lclr]
}

# Initialization of canvas with polygonal objects filled or not
# -------------------------------------------------------------------
proc DisplayInit {w d lcnx lclr} {
    set lpoly {}
    set i 0
    if {$d == "Shaded"} then {
        foreach cnx $lcnx {
            lappend lpoly [$w.c create polygon \
                              [string repeat " 0" [expr {2*[llength $cnx]}]] \
                              -fill "#[lindex $lclr $i]"]
            incr i
        }
    } else {
        foreach cnx $lcnx {
            lappend lpoly [$w.c create polygon \
                              [string repeat " 0" [expr {2*[llength $cnx]}]] \
                              -fill {} -outline blue]
        }
    }
    return $lpoly
}

# A blue of level v. Up to 255 only the blue channel grows; above that the
# excess goes into red and green too, so the color turns lighter, like an
# over-exposed blue, instead of stopping at pure blue
# -------------------------------------------------------------------
proc BlueColor {v} {
    : { v = int(v); b = max(0, min(255, v)); rg = max(0, min(255, v-255)) }
    return [format #%02x%02x%02x $rg $rg $b]
}

# Flat shaded display with gradient color
# -------------------------------------------------------------------
proc DisplayShaded {w lpoly lvtx lcnx lnv lmv} {
    if {$::stop} return

    global iterations
    global ax ay az contrast bright

    : { ax = ax - 0.02;
        ay = ay + 0.025;
        az = az + 0.02 }
    
    set lgradB {}
    foreach {M lnew lvn} [Transformations $lvtx $lnv] {}
    # Light vector is set to <1 1 -1> 
    foreach i [Intensity $lvn $lmv [list 1 1 -1]] {
        # blue level 100..254 from the light angle. The Contrast slider (percent,
        # 100 = original) stretches it away from the middle of the range that is
        # actually used (165), then the Brightness slider (percent) scales it
        lappend lgradB [: BlueColor((165 + (100 + 154*i/32 - 165)*contrast/100.0)*bright/100.0)]
    }
    set i 0
    foreach cnx $lcnx {
        set lcoords {}
        foreach j $cnx {lappend lcoords [lindex $lnew $j]}
        # Backface culing for hidden face. Not removed but only reduced to a point
        if {[lindex $lvn $i 2] < 0} {
            eval $w.c coords [lindex $lpoly $i] [join $lcoords]
            $w.c itemconfigure [lindex $lpoly $i] -fill [lindex $lgradB $i]
        } else {
            $w.c coords [lindex $lpoly $i] [string repeat " [join [Barycenter $lcoords]]" [llength $cnx]]
        }
        incr i
    }
    if {[incr ::iterations]} {
        set ::run [after $::speed [list DisplayShaded $w $lpoly $lvtx $lcnx $lnv $lmv]]
    } else {
        return 
    }
}

# Wireframe display 
# -------------------------------------------------------------------
proc DisplayWireframe {w lpoly lvtx lcnx lnv lmv} {
    if {$::stop} return
    global ax ay az bright

    : { ax = ax - 0.02;
        ay = ay + 0.025;
        az = az + 0.02 }
    foreach {M lnew lvn} [Transformations $lvtx $lnv] {}
    # Line color: blue level from the Brightness slider (percent, 100 = full blue)
    : edge = BlueColor(255*bright/100.0)
    set i 0
    foreach cnx $lcnx {
        set lcoords {}
        foreach j $cnx {lappend lcoords [lindex $lnew $j]}
        # Backface culing for hidden face. Not removed but only reduced to a point,
        # unless "Back edges" is on, then the hidden faces are drawn as well
        if {$::backedges || [lindex $lvn $i 2] < 0} {
            eval $w.c coords [lindex $lpoly $i] [join $lcoords]
        } else {
            $w.c coords [lindex $lpoly $i] [string repeat " [join [Barycenter $lcoords]]" [llength $cnx]]
        }
        $w.c itemconfigure [lindex $lpoly $i] -outline $edge
        incr i
    }
    if {[incr ::iterations] } {
        set ::run [after $::speed [list DisplayWireframe $w $lpoly $lvtx $lcnx $lnv $lmv]]
    } else {
        return 
    }
}

# Mouse wheel over any slider moves it, one notch = 2% of the slider's range;
# scrolling down increases the value, scrolling up decreases it.
# X is %X, Y is %Y (pointer position), D is the wheel direction/delta.
# -------------------------------------------------------------------
proc WheelScale {X Y D} {
    set s [winfo containing $X $Y]
    if {$s eq "" || [winfo class $s] ne "Scale"} return
    set step [expr {max(1, round(([$s cget -to] - [$s cget -from])/50.0))}]
    $s set [expr {[$s get] + ($D > 0 ? -$step : $step)}]   ;# wheel down = increase
}

# Shaded/Wireframe changed: restart the current model as if its button
# had been clicked again (does nothing until a model has been chosen)
# -------------------------------------------------------------------
proc RestartModel {w} {
    if {[info exists ::curmodel]} {DisplayModel $w $::curmodel}
}

# Background grey level from the slider; the title text switches between
# white and black so it stays readable
# -------------------------------------------------------------------
proc SetBackground {w v} {
    $w.c configure -bg [format #%02x%02x%02x $v $v $v]
    $w.c itemconfigure label -fill [expr {$v < 128 ? "white" : "black"}]
}

# -------------------------------------------------------------------
proc Main {} {
    global stop
    global display
    global scx scy vdist speed
    set ::run {}

    set w .tdc
    catch {destroy $w}
    toplevel $w
    wm withdraw .
    wm protocol $w WM_DELETE_WINDOW exit   ;# the close box exits
    # mouse wheel on the sliders (MouseWheel on Windows/macOS, buttons 4/5 on X11)
    bind $w <MouseWheel> {WheelScale %X %Y %D}
    bind $w <Button-4>   {WheelScale %X %Y 1}
    bind $w <Button-5>   {WheelScale %X %Y -1}
    wm title $w "Rotating polyhedra in Tk canvas "

    set display Shaded
    set scx 420
    set scy 420
    set vdist 1200
    set ::scaspeed 40
    set ::speed 40
    set ::bggray 0      ;# background grey level, 0 = black, 255 = white
    set ::bright 100    ;# brightness of the shaded blues / wireframe lines, percent
    set ::contrast 100  ;# contrast of the shaded blues, percent
    set ::backedges 1   ;# wireframe: 1 = also draw the hidden back edges

    pack [canvas $w.c -width $scx -height $scy -bg black -bd 0]
    $w.c delete all
    bind $w.c <Destroy> {
        after cancel $::run
    }

    set f1 [frame $w.f1 -relief sunken -borderwidth 2]
    pack $f1 -fill x
    button $f1.brun -text Stop -command {set stop 1}
    button $f1.bq   -text Quit -command exit
    label  $f1.l1   -text "  "
    radiobutton $f1.rbs -text "Shaded" -variable display -value Shaded \
                      -command [list RestartModel $w]
    radiobutton $f1.rbw -text "Wireframe" -variable display -value Wireframe \
                      -command [list RestartModel $w]
    checkbutton $f1.cbe -text "Back edges" -variable backedges
    pack {*}[winfo children $f1] -side left
    set f2 [frame $w.f2 -relief sunken -borderwidth 2]
    pack $f2 -fill x
    foreach i {tetrahedron cube octahedron dodecahedron icosahedron} {
        button $f2.b$i -text $i -command "DisplayModel $w $i"
    }
    pack {*}[winfo children $f2] -side left
    set f3 [frame $w.f3 -relief sunken -borderwidth 2]
    pack $f3 -fill x
    label $f3.l1 -text "View distance " -width 12 
    scale $f3.sca -from 300 -to 1600 -length 300 \
                      -orient horiz -bd 1 -showvalue true -variable vdist 
    pack {*}[winfo children $f3] -side left

    set f4 [frame $w.f4 -relief sunken -borderwidth 2]
    pack $f4 -fill x
    label $f4.l1 -text "Speed " -width 12 
    scale $f4.speed -from 1 -to 99 -length 300 \
                      -orient horiz -bd 1 -showvalue true -variable scaspeed \
                      -command {set speed [expr {100-$scaspeed}];#}
    pack {*}[winfo children $f4] -side left

    set f5 [frame $w.f5 -relief sunken -borderwidth 2]
    pack $f5 -fill x
    label $f5.l1 -text "Background " -width 12
    scale $f5.bg -from 0 -to 255 -length 300 \
                      -orient horiz -bd 1 -showvalue true -variable bggray \
                      -command [list SetBackground $w]
    pack {*}[winfo children $f5] -side left

    set f6 [frame $w.f6 -relief sunken -borderwidth 2]
    pack $f6 -fill x
    label $f6.l1 -text "Brightness " -width 12
    scale $f6.br -from 10 -to 200 -length 300 \
                      -orient horiz -bd 1 -showvalue true -variable bright
    pack {*}[winfo children $f6] -side left

    set f7 [frame $w.f7 -relief sunken -borderwidth 2]
    pack $f7 -fill x
    label $f7.l1 -text "Contrast " -width 12
    scale $f7.co -from 25 -to 300 -length 300 \
                      -orient horiz -bd 1 -showvalue true -variable contrast
    pack {*}[winfo children $f7] -side left
}

# Compile the Calc code (the : commands) in these procs, once, now that they
# are all defined and just before Main runs. This is the same result as
# defining each of them with proc=
# -------------------------------------------------------------------
Calc::transformProcs \
    Barycenter CrossProduct DotProduct \
    MatrixVectorProduct MatrixProduct MatrixRotation \
    NormalVector Projection Transformations Intensity \
    DisplayModel ReadData BlueColor \
    DisplayShaded DisplayWireframe

# Everything that uses Calc is compiled now, so remove the interpreted : and =
# commands. A : or = left in any code that was not compiled (a proc missing from
# the list above, say) now fails with 'invalid command name' and a traceback
# showing which proc and line, instead of quietly running the slow path and
# filling Calc::cache.
rename : {}
rename = {}

Main
