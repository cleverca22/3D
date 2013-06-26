DECLARE SUB FixScreen ()
DECLARE FUNCTION FindFarthestSquare% ()
DECLARE SUB SortSquares ()
DECLARE FUNCTION SquareDistance# (s%)
DECLARE SUB UpdateSquareDistance ()
DECLARE FUNCTION AddSquare% (p1%, p2%, p3%, p4%, colour%)
DECLARE SUB SetSquareCount (n%)
DECLARE FUNCTION SquareX# (Square%)
DECLARE FUNCTION SquareY# (Square%)
DECLARE FUNCTION InputKey$ ()
DECLARE SUB Setup ()
DECLARE SUB EditMode ()
DECLARE FUNCTION MainMenu$ ()
DECLARE FUNCTION SetupMenu$ ()
DECLARE FUNCTION EditModeMenu$ ()
DECLARE SUB SelectPoint (p%)
DECLARE SUB ExtraInit ()
DECLARE FUNCTION AddPoint% (x%, y%, z%)
DECLARE FUNCTION AddLine% (p1%, p2%)
DECLARE SUB ViewRaw ()
DECLARE SUB EditRaw ()
DECLARE SUB Refresh ()
DECLARE SUB SetLineCount (n%)
DECLARE SUB SetPointCount (n%)
DECLARE SUB Init ()
TYPE m3dPoint
x AS DOUBLE
y AS DOUBLE
z AS DOUBLE
END TYPE

TYPE m3dLine
one AS INTEGER
two AS INTEGER
END TYPE

TYPE m3dSquare
p1 AS INTEGER
p2 AS INTEGER
p3 AS INTEGER
p4 AS INTEGER
colour AS INTEGER
distance AS DOUBLE
sorted AS STRING * 1
END TYPE

DECLARE SUB ShowLine (l%, colour%)
DECLARE FUNCTION GetX# (p%)
DECLARE FUNCTION GetY# (p%)
DECLARE SUB ShowPoint (p%)
DECLARE SUB ShowSquare (p%)
'''''''''''''''''''''''''''''''program start''''''''''''''''''''''''''''''
OPTION BASE 0
DEFINT A-Z

DIM SHARED PointCount%
DIM SHARED LineCount%
DIM SHARED SquareCount%
PointCount% = 0
LineCount% = 0
SquareCount% = 0
DIM SHARED Points(PointCount%) AS m3dPoint
DIM SHARED Lines(LineCount%) AS m3dLine
DIM SHARED Squares(SquareCount%) AS m3dSquare
DIM SHARED SortedSquares(SquareCount%) AS INTEGER
DIM SHARED FileName$
DIM SHARED DisplayMode%
DIM SHARED DisplayLineCount%
DIM SHARED DisplayStartLine%
DIM SHARED me AS m3dPoint
DIM SHARED AngleA AS DOUBLE, AngleB AS DOUBLE
DIM SHARED zMin#
DIM SHARED Change AS DOUBLE
DIM s AS m3dSquare
DIM SHARED CurrentScreen%

Init
MainLoop:
SELECT CASE MainMenu
CASE "0"'exit
        PRINT "Are you shure?y/n ";
        c$ = InputKey
        IF c$ = "y" THEN SYSTEM
        IF c$ = "Y" THEN SYSTEM
        GOTO MainLoop
CASE "1"'load
        INPUT "Enter File Name:", FileName$
        ON ERROR GOTO LoadError
        ON ERROR GOTO LoadError
        OPEN FileName$ FOR INPUT AS 1
        ON ERROR GOTO 0
        SetPointCount 0
        SetLineCount 0
        SetSquareCount 0
        INPUT #1, p%
        SetPointCount p%
        FOR i = 1 TO PointCount%
                INPUT #1, Points(i).x, Points(i).y, Points(i).z
                Refresh
        NEXT i
        INPUT #1, l%
        SetLineCount l%
        FOR i = 1 TO LineCount%
                INPUT #1, Lines(i).one, Lines(i).two
                Refresh
        NEXT i
        INPUT #1, n%
        SetSquareCount n%
        FOR i = 1 TO SquareCount%
                INPUT #1, s.p1, s.p2, s.p3, s.p4, s.colour
                Squares(i) = s
                Refresh
        NEXT i
        CLOSE 1
        Refresh
        GOTO MainLoop
LoadError:
        PRINT "error #", ERR
        SHELL "PAUSE"
        FixScreen
        GOTO MainLoop
CASE "2"'save
        INPUT "Enter File Name:", FileName$
        OPEN FileName$ FOR OUTPUT AS 1
        PRINT #1, PointCount%
        FOR i = 1 TO PointCount%
                PRINT #1, Points(i).x, Points(i).y, Points(i).z
        NEXT i
        PRINT #1, LineCount%
        FOR i = 1 TO LineCount%
                PRINT #1, Lines(i).one, Lines(i).two
        NEXT i
        PRINT #1, SquareCount%
        FOR i = 1 TO SquareCount%
                s = Squares(i)
                PRINT #1, s.p1, s.p2, s.p3, s.p4, s.colour
        NEXT i
        CLOSE 1
        GOTO MainLoop
CASE "3"'view raw
        CALL ViewRaw
        GOTO MainLoop
CASE "4"'edit raw
        CALL EditRaw
        GOTO MainLoop
CASE "5"'set xyz
        INPUT me.x, me.y, me.z
        Refresh
        GOTO MainLoop
CASE "6"'moveMode
        KEY(0) ON
        DO
                k$ = INKEY$
                SELECT CASE k$
                CASE "+"
                        GOSUB F1
                CASE "-"
                        GOSUB F2
                CASE ""
                CASE CHR$(0) + "P"
                        GOSUB Down
                CASE CHR$(0) + "H"
                        GOSUB Up
                CASE CHR$(0) + "K"
                        GOSUB Left
                CASE CHR$(0) + "M"
                        GOSUB Right
                CASE ELSE
                        EXIT DO
                END SELECT
        LOOP
        KEY(0) OFF
        GOTO MainLoop
CASE "7"'edit mode
        EditMode
        GOTO MainLoop
CASE "8"'refresh
        CALL Refresh
        GOTO MainLoop
CASE "9"'setup
        CALL Setup
        GOTO MainLoop
CASE ELSE
        PRINT "Error! Please Select an Option From Above"
        DO WHILE INKEY$ = ""
        LOOP
        GOTO MainLoop
END SELECT
STOP
END
F1:
me.y = me.y + Change
Refresh
PRINT "At"; CSNG(me.x); CSNG(me.y); CSNG(me.z)
RETURN
F2:
me.y = me.y - Change
Refresh
PRINT "At"; CSNG(me.x); CSNG(me.y); CSNG(me.z)
RETURN
Up:
me.z = me.z + Change
Refresh
PRINT "At"; CSNG(me.x); CSNG(me.y); CSNG(me.z)
RETURN
Left:
me.x = me.x - Change
Refresh
PRINT "At"; CSNG(me.x); CSNG(me.y); CSNG(me.z)
RETURN
Right:
me.x = me.x + Change
Refresh
PRINT "At"; CSNG(me.x); CSNG(me.y); CSNG(me.z)
RETURN
Down:
me.z = me.z - Change
Refresh
PRINT "At"; CSNG(me.x); CSNG(me.y); CSNG(me.z)
RETURN

FUNCTION AddLine% (p1%, p2%)
SetLineCount LineCount% + 1
Lines(LineCount%).one = p1%
Lines(LineCount%).two = p2%
Refresh
AddLine% = LineCount%
END FUNCTION

FUNCTION AddPoint% (x AS INTEGER, y AS INTEGER, z AS INTEGER)
SetPointCount PointCount% + 1
Points(PointCount%).x = x
Points(PointCount%).y = y
Points(PointCount%).z = z
Refresh
AddPoint% = PointCount%
END FUNCTION

FUNCTION AddSquare (p1, p2, p3, p4, colour)
SetSquareCount SquareCount% + 1
Squares(SquareCount%).p1 = p1
Squares(SquareCount%).p2 = p2
Squares(SquareCount%).p3 = p3
Squares(SquareCount%).p4 = p4
Squares(SquareCount%).colour = colour
Refresh
AddSquare = SquareCount%
END FUNCTION

SUB EditMode
EditModeLoop:
SELECT CASE EditModeMenu
CASE "0"'exit
        EXIT SUB
CASE "1"'select Point
        INPUT p%
        SelectPoint (p%)
        DO WHILE INKEY$ = ""
        LOOP
        GOTO EditModeLoop
CASE "2"'add line
        INPUT "Enter Start, End ", o%, e%
        PRINT "Line Number Is " + STR$(AddLine(o%, e%))
        Refresh
        DO WHILE INKEY$ = ""
        LOOP
        GOTO EditModeLoop
CASE "3"'add square
        DIM s AS m3dSquare
        INPUT "Enter p1, p2, p3, p4", s.p1, s.p2, s.p3, s.p4
        INPUT "Enter Colour", s.colour
        PRINT "Square Number Is " + STR$(AddSquare(s.p1, s.p2, s.p3, s.p4, s.colour))
        Refresh
        DO WHILE INKEY$ = ""
        LOOP
        GOTO EditModeLoop
CASE ELSE
        PRINT "Error! Please Select an Option From Above"
        DO WHILE INKEY$ = ""
        LOOP
        GOTO EditModeLoop
END SELECT
STOP
END SUB

FUNCTION EditModeMenu$
'CLS 2
PRINT "1. Select Point"
PRINT "2. Add Line"
PRINT "3. Add Square"
PRINT "0. Main Menu"
LOCATE , , 1, 0, 31

PRINT "Please select a choice:";
EditModeMenu$ = InputKey
END FUNCTION

SUB EditRaw
STOP
END SUB

SUB ExtraInit
'PRINT PointCount%
a = AddSquare(1, 1, 2, 3, 5)
'PRINT PointCount%
a = AddSquare(10, 11, 19, 18, 7)
'PRINT PointCount%
'a = AddPoint(-1, 1, -1)
'PRINT PointCount%
'a = AddPoint(-1, 1, 1)
'PRINT PointCount%
'a = AddPoint(1, -1, -1)
'PRINT PointCount%
'a = AddPoint(1, -1, 1)
'PRINT PointCount%
'a = AddPoint(1, 1, -1)
'PRINT PointCount%
'a = AddPoint(1, 1, 1)
'PRINT PointCount%
CALL Refresh
END SUB

FUNCTION FindFarthestSquare
FOR i = 1 TO SquareCount%
        IF Squares(i).sorted = "F" THEN
                EXIT FOR
        END IF
NEXT i
FOR j = 1 TO SquareCount%
        IF Squares(j).distance > Squares(i).distance THEN
                IF Squares(j).sorted = "F" THEN
                        i = j
                END IF
        END IF
NEXT j
FindFarthestSquare = i
END FUNCTION

SUB FixScreen
VIEW (1, 1)-(638, 186), , 1
VIEW PRINT DisplayStartLine% TO DisplayStartLine% + DisplayLineCount%
WINDOW (-6.38, 1.86)-(6.38, -1.86)
CLS 2
END SUB

FUNCTION GetX# (p%)
'c# = p.x * p.x
'c# = c# + (p.z * p.z)
'GetX# = p.x / SQR(c#)
GetX# = (Points(p%).x - me.x) / (Points(p%).z - me.z)
END FUNCTION

FUNCTION GetY# (p%)
'STOP
'c# = p.y * p.y
'c# = c# + (p.z * p.z)
'GetY# = p.y / SQR(c#)
GetY# = (Points(p%).y - me.y) / (Points(p%).z - me.z)
END FUNCTION

DEFSNG A-Z
SUB Init
SetPointCount 0
SetLineCount 0
SetSquareCount 0
zMin# = .2
SCREEN 9, , 0, 0
DisplayLineCount% = 10
DisplayStartLine% = 15
FixScreen
DisplayMode% = 2
ON KEY(1) GOSUB F1
ON KEY(2) GOSUB F2
ON KEY(11) GOSUB Up
ON KEY(12) GOSUB Left
ON KEY(13) GOSUB Right
ON KEY(14) GOSUB Down
me.x = 0
me.y = 0
me.z = -2
Change = .01
END SUB

DEFINT A-Z
FUNCTION InputKey$
DO
        k$ = INKEY$
        IF k$ <> "" THEN
                InputKey = k$
                PRINT k$
                EXIT DO
        END IF
LOOP
END FUNCTION

DEFSNG A-Z
FUNCTION MainMenu$
PRINT "1. Load"
PRINT "2. Save"
PRINT "3. View Raw"
PRINT "4. Edit Raw"
PRINT "5. Set My X,Y,Z"
PRINT "6. Move Mode"
PRINT "7. Edit Mode"
PRINT "8. Refresh"
PRINT "9. Setup"
PRINT "0. Exit"
LOCATE , , 1, 0, 31

PRINT "Please select a choice:";
MainMenu$ = InputKey
END FUNCTION

SUB Refresh
IF CurrentScreen% = 0 THEN
        SCREEN 9, , 1, 0
        FixScreen
ELSE
        SCREEN 9, , 0, 1
        FixScreen
END IF
CLS 1
SELECT CASE DisplayMode%
CASE 1
        FOR p% = 1 TO PointCount%
                ShowPoint p%
        NEXT p%
CASE 2
        FOR l% = 1 TO LineCount%
                ShowLine l%, 1
        NEXT l%
        FOR p% = 1 TO PointCount%
                ShowPoint p%
        NEXT p%
CASE 3
        'STOP
        'temp =
        SortSquares
        delay% = 1
        FOR s% = 1 TO SquareCount%
                ShowSquare SortedSquares(s%)
                FOR i = 1 TO delay%
                NEXT i
        NEXT s%
'        FOR l% = 1 TO LineCount%
'                ShowLine l%, 1
'                FOR i = 1 TO delay%
'                NEXT i
'        NEXT l%
'        FOR s% = 1 TO SquareCount%
'                ShowSquare s%
'        NEXT s%
'        FOR p% = 1 TO PointCount%
'                ShowPoint p%
'                FOR i = 1 TO delay%
'                NEXT i
'        NEXT p%
END SELECT
IF CurrentScreen% = 0 THEN
        SCREEN 9, , 1, 1
        FixScreen
        CurrentScreen% = 1
ELSE
        SCREEN 9, , 0, 0
        FixScreen
        CurrentScreen% = 0
END IF
END SUB

DEFINT A-Z
SUB SelectPoint (p%)
CIRCLE (GetX(p%), GetY(p%)), .1
END SUB

SUB SetLineCount (n%)
DIM temp(LineCount%) AS m3dLine
IF n% < LineCount% THEN e = n% ELSE e = LineCount%
LineCount% = n%
FOR i = 0 TO e
        temp(i) = Lines(i)
NEXT i
REDIM Lines(LineCount%)
FOR i = 0 TO e
        Lines(i) = temp(i)
NEXT i
END SUB

DEFSNG A-Z
SUB SetPointCount (n%)
DIM temp(PointCount%) AS m3dPoint
IF n% < PointCount% THEN e = n% ELSE e = PointCount%
PointCount% = n%
FOR i = 0 TO e
        temp(i) = Points(i)
NEXT i
REDIM Points(PointCount%)
FOR i = 0 TO e
        Points(i) = temp(i)
NEXT i
END SUB

DEFINT A-Z
SUB SetSquareCount (n%)
DIM temp(SquareCount%) AS m3dSquare
IF n% < SquareCount% THEN e = n% ELSE e = SquareCount%
SquareCount% = n%
FOR i = 0 TO e
        temp(i) = Squares(i)
NEXT i
REDIM Squares(SquareCount%)
REDIM SortedSquares(SquareCount%)
FOR i = 0 TO e
        Squares(i) = temp(i)
NEXT i
END SUB

SUB Setup
SetupLoop:
SELECT CASE SetupMenu
CASE "0"'main menu
CASE "1"
        DisplayMode% = 1
        Refresh
        GOTO SetupLoop
CASE "2"
        DisplayMode% = 2
        Refresh
        GOTO SetupLoop
CASE "3"
        DisplayMode% = 3
        Refresh
        GOTO SetupLoop
CASE "4"
        STOP
CASE "5"
        STOP
CASE "6"
        STOP
CASE "7"
        STOP
CASE "8"
        STOP
CASE "9"
        STOP
END SELECT
END SUB

FUNCTION SetupMenu$
CLS 2
IF DisplayMode <> 1 THEN PRINT "1. Dot Mode"
IF DisplayMode <> 2 THEN PRINT "2. Line Mode"
IF DisplayMode <> 3 THEN PRINT "3. Graphic Mode"
PRINT "0. Main Menu"
LOCATE , , 1, 31, 0
PRINT "Please select a choice:";
SetupMenu$ = InputKey
END FUNCTION

DEFSNG A-Z
SUB ShowLine (l%, colour%)
DIM t AS m3dLine
t = Lines(l%)
z# = Points(t.one).z - me.z
IF z# > zMin# THEN
        z# = Points(t.two).z - me.z
        IF z# > zMin# THEN
                LINE (GetX(t.one), GetY(t.one))-(GetX(t.two), GetY(t.two)), colour%'t.colour
        END IF
END IF
END SUB

SUB ShowPoint (p%)
DIM z#
z# = Points(p%).z - me.z

IF z# > zMin# THEN
        PSET (GetX(p%), GetY(p%))
END IF
END SUB

DEFINT A-Z
SUB ShowSquare (p%)
IF SquareDistance(p%) > zMin# THEN
        x1# = GetX(Squares(p%).p1)
        y1# = GetY(Squares(p%).p1)
        x2# = GetX(Squares(p%).p2)
        y2# = GetY(Squares(p%).p2)
        x3# = GetX(Squares(p%).p3)
        y3# = GetY(Squares(p%).p3)
        x4# = GetX(Squares(p%).p4)
        y4# = GetY(Squares(p%).p4)
       
        x# = SquareX(p%)
        y# = SquareY(p%)

        DRAW "C2"
        DRAW "BM" + STR$(PMAP(x1#, 0)) + " , " + STR$(PMAP(y1#, 1))
        DRAW "M" + STR$(PMAP(x2#, 0)) + " , " + STR$(PMAP(y2#, 1))
        DRAW "M" + STR$(PMAP(x3#, 0)) + " , " + STR$(PMAP(y3#, 1))
        DRAW "M" + STR$(PMAP(x4#, 0)) + " , " + STR$(PMAP(y4#, 1))
        DRAW "M" + STR$(PMAP(x1#, 0)) + " , " + STR$(PMAP(y1#, 1))
        DRAW "BM" + STR$(PMAP(x#, 0)) + " , " + STR$(PMAP(y#, 1))
        DRAW "P" + STR$(Squares(p%).colour) + ",2"
        DRAW "C1"
        DRAW "BM" + STR$(PMAP(x1#, 0)) + " , " + STR$(PMAP(y1#, 1))
        DRAW "M" + STR$(PMAP(x2#, 0)) + " , " + STR$(PMAP(y2#, 1))
        DRAW "M" + STR$(PMAP(x3#, 0)) + " , " + STR$(PMAP(y3#, 1))
        DRAW "M" + STR$(PMAP(x4#, 0)) + " , " + STR$(PMAP(y4#, 1))
        DRAW "M" + STR$(PMAP(x1#, 0)) + " , " + STR$(PMAP(y1#, 1))
ELSE
        'STOP
END IF
END SUB

SUB SortSquares
UpdateSquareDistance
FOR i = 1 TO SquareCount%
        'SortedSquares(i) = i
        Squares(i).sorted = "F"
NEXT i
FOR i = 1 TO SquareCount%
        m = FindFarthestSquare
        SortedSquares(i) = m
        Squares(m).sorted = "T"
NEXT i
END SUB

FUNCTION SquareDistance# (s%)
z1# = Points(Squares(s%).p1).z - me.z
z2# = Points(Squares(s%).p2).z - me.z
z3# = Points(Squares(s%).p3).z - me.z
z4# = Points(Squares(s%).p4).z - me.z
z# = z1#
IF z2# < z# THEN z# = z2#
IF z3# < z# THEN z# = z3#
IF z4# < z# THEN z# = z4#
SquareDistance# = z#
END FUNCTION

FUNCTION SquareX# (Square%)
x1# = GetX(Squares(Square%).p1)
x2# = GetX(Squares(Square%).p2)
x3# = GetX(Squares(Square%).p3)
x4# = GetX(Squares(Square%).p4)
      
x# = ((x1# + x2# + x3# + x4#) / 4)
SquareX# = x#
END FUNCTION

FUNCTION SquareY# (Square%)
y1# = GetY(Squares(Square%).p1)
y2# = GetY(Squares(Square%).p2)
y3# = GetY(Squares(Square%).p3)
y4# = GetY(Squares(Square%).p4)
     
y# = ((y1# + y2# + y3# + y4#) / 4)
SquareY# = y#
END FUNCTION

SUB UpdateSquareDistance
FOR s% = 1 TO SquareCount%
        Squares(s%).distance = SquareDistance(s%)
NEXT s%
END SUB

SUB ViewRaw
j = 0
FOR i = 1 TO PointCount%
        PRINT i, Points(i).x, Points(i).y, Points(i).z
        j = j + 1
        IF j = DisplayLineCount% - 1 THEN
                PRINT "Press Any Key To Continue"
                SLEEP
                j = 0
        END IF
NEXT i
FOR i = 1 TO LineCount%
        PRINT i, Lines(i).one, Lines(i).two', Lines(i).colour
        j = j + 1
        IF j = DisplayLineCount% - 1 THEN
                PRINT "Press Any Key To Continue"
                SLEEP
                j = 0
        END IF
NEXT i
FOR i = 1 TO SquareCount%
        PRINT i, Squares(i).p1, Squares(i).p2, Squares(i).p3, Squares(i).p4, Squares(i).colour
        j = j + 1
        IF j = DisplayLineCount% - 1 THEN
                PRINT "Press Any Key To Continue"
                SLEEP
                j = 0
        END IF
NEXT i
DO UNTIL INKEY$ = ""
LOOP
DO WHILE INKEY$ = ""
LOOP
END SUB

