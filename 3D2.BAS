DECLARE FUNCTION AddPoint% (x AS DOUBLE, y AS DOUBLE, z AS DOUBLE)
DECLARE SUB quit ()
DECLARE SUB FreeTime ()
DECLARE SUB showFiles ()
DECLARE SUB ShowLocation ()
DECLARE SUB InitScreen ()
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
DECLARE FUNCTION AddLine% (p1%, p2%)
DECLARE SUB ViewRaw ()
DECLARE SUB EditRaw ()
DECLARE SUB Refresh ()
DECLARE SUB SetLineCount (n%)
DECLARE SUB SetPointCount (n%)
DECLARE SUB Init ()
DECLARE SUB ShowLine (l%, colour%)
DECLARE FUNCTION GetX# (p%)
DECLARE FUNCTION GetY# (p%)
DECLARE SUB ShowPoint (p%)
DECLARE SUB ShowSquare (p%)
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

TYPE tScreenSettings
ScreenMode AS INTEGER
ViewX1 AS INTEGER
ViewX2 AS INTEGER
ViewY1 AS INTEGER
ViewY2 AS INTEGER
DisplayStartLine AS INTEGER
DisplayLineCount AS INTEGER
WindowX1 AS DOUBLE
WindowX2 AS DOUBLE
WindowY1 AS DOUBLE
WindowY2 AS DOUBLE
END TYPE

CONST PI = 3.14
DIM lt AS DOUBLE
'''''''''''''''''''''''''''''''program start''''''''''''''''''''''''''''''
OPTION BASE 0
DEFINT A-Z

DIM SHARED smallest#
smallest# = 1
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
DIM SHARED DisplayMode%                         'dots/lines/squares
DIM SHARED me AS m3dPoint
DIM SHARED Hangle AS DOUBLE, Vangle AS DOUBLE
DIM SHARED zMin#
DIM SHARED Change AS DOUBLE
DIM s AS m3dSquare
DIM SHARED ScreenMode%                          'hide/show refresh
DIM SHARED ss(2) AS tScreenSettings
DIM SHARED css AS INTEGER
DIM SHARED PointSkip AS INTEGER

Init
MainLoop:
SELECT CASE MainMenu
CASE "0"'exit
        PRINT "Are you shure?y/n ";
        c$ = InputKey
        IF c$ = "y" THEN quit
        IF c$ = "Y" THEN quit
        GOTO MainLoop
CASE "1"'load
        KEY(0) OFF
        INPUT "Enter File Name:", FileName$
        KEY(0) ON
        IF FileName$ = "" THEN GOTO MainLoop
        FileName$ = FileName$ + ".3d1"
        PRINT "opening "; FileName$
        'ON ERROR GOTO LoadError
        ON ERROR GOTO LoadError
        OPEN FileName$ FOR INPUT AS 1
        lt = TIMER
        ON ERROR GOTO eror
'        SetPointCount 0
        SetLineCount 0
        SetSquareCount 0
1       INPUT #1, p%
        SetPointCount p%
        FOR i = 1 TO PointCount%
2               INPUT #1, Points(i).x, Points(i).y, Points(i).z
'                PointCount% = p%
                IF PointCount% < 300 THEN CALL Refresh
        NEXT i
3       INPUT #1, l%
        SetLineCount l%
        FOR i = 1 TO LineCount%
4               INPUT #1, Lines(i).one, Lines(i).two
                IF PointCount% < 250 THEN CALL Refresh
        NEXT i
5       INPUT #1, n%
        SetSquareCount n%
        oldmode% = DisplayMode%
        DisplayMode% = 3
        FOR i = 1 TO SquareCount%
6               INPUT #1, s.p1, s.p2, s.p3, s.p4, s.colour
                Squares(i) = s
                PRINT #10, "loading"; CINT((i / SquareCount%) * 100); "% done", i; "/"; SquareCount%; CHR$(13);
'                Refresh
        NEXT i
        CLOSE 1
        PRINT #10, ""
        PRINT #10, "total loading time"; TIMER - lt
        'a$ = InputKey
        DisplayMode% = oldmode%
        Refresh
        GOTO MainLoop
LoadError:
        PRINT "error #", ERR, ERL
        t$ = InputKey
        'FixScreen()
        ON ERROR GOTO eror
        GOTO MainLoop
CASE "2"'save
        KEY(0) OFF
        INPUT "Enter File Name:", FileName$
        KEY(0) ON
        FileName$ = FileName$ + ".3d1"
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
CASE "4"'dir
        CALL showFiles
        GOTO MainLoop
CASE "5"'set xyz
        KEY(0) OFF
        ShowLocation
        INPUT me.x, me.y, me.z, Hangle, Vangle
        KEY(0) ON
        Refresh
        GOTO MainLoop
CASE "6"'moveMode
'        KEY(0) ON
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
                CASE "1"
                        DisplayMode% = 1
                        Refresh
                CASE "2"
                        DisplayMode% = 2
                        Refresh
                CASE "3"
                        DisplayMode% = 3
                        Refresh
                CASE ELSE
                        EXIT DO
                END SELECT
        LOOP
'        KEY(0) OFF
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
ShowLocation
RETURN
F2:
me.y = me.y - Change
Refresh
ShowLocation
RETURN
Up:
me.z = me.z + Change
Refresh
ShowLocation
RETURN
Left:
me.x = me.x - Change
Refresh
ShowLocation
RETURN
Right:
me.x = me.x + Change
Refresh
ShowLocation
RETURN
Down:
me.z = me.z - Change
Refresh
ShowLocation
RETURN

TurnLeft:
Hangle = Hangle + Change
Refresh
ShowLocation
RETURN
TurnRight:
Hangle = Hangle - Change
Refresh
ShowLocation
RETURN

penAction:
DRAW "c0 bm" + STR$(x) + "," + STR$(y) + "d"
x = PEN(4) / 1
y = PEN(5) / 1
DRAW "c11 bm" + STR$(x) + "," + STR$(y) + "d"
RETURN

eror:
IF ERR = 61 THEN
        PRINT "disk full"
        PRINT "stoped at line "; ERL
        quit
END IF
PRINT ERR, ERL
PRINT "unkown error"
STOP
RESUME NEXT
SYSTEM

FUNCTION AddLine% (p1%, p2%)
SetLineCount LineCount% + 1
Lines(LineCount%).one = p1%
Lines(LineCount%).two = p2%
Refresh
AddLine% = LineCount%
END FUNCTION

FUNCTION AddPoint% (x AS DOUBLE, y AS DOUBLE, z AS DOUBLE)
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
        KEY(0) OFF
        INPUT "Enter p1, p2, p3, p4", s.p1, s.p2, s.p3, s.p4
        INPUT "Enter Colour", s.colour
        KEY(0) ON
        PRINT "Square Number Is " + STR$(AddSquare(s.p1, s.p2, s.p3, s.p4, s.colour))
        Refresh
        DO WHILE INKEY$ = ""
        LOOP
        GOTO EditModeLoop
CASE "4"'add point
        DIM np AS m3dPoint
        KEY(0) OFF
        INPUT "Enter x,y,z", np.x, np.y, np.z
        KEY(0) ON
        PRINT "Point Number is " + STR$(AddPoint(np.x, np.y, np.z))
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
PRINT "4. Add Point"
PRINT "0. Main Menu"
'LOCATE , , 1, 0, 31

PRINT "Please select a choice:";
EditModeMenu$ = InputKey
END FUNCTION

SUB EditRaw
STOP
END SUB

SUB ExtraInit
'PRINT PointCount%
'a = AddSquare(1, 1, 2, 3, 5)
'PRINT PointCount%
'a = AddSquare(10, 11, 19, 18, 7)
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
IF i > SquareCount% THEN STOP
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
VIEW (ss(css).ViewX1, ss(css).ViewY1)-(ss(css).ViewX2, ss(css).ViewY2), , 1
VIEW PRINT ss(css).DisplayStartLine TO ss(css).DisplayStartLine + ss(css).DisplayLineCount
WINDOW (ss(css).WindowX1, ss(css).WindowY1)-(ss(css).WindowX2, ss(css).WindowY2)
'CLS 2
END SUB

SUB FreeTime STATIC
'LOCATE 18, 40
'FOR i = 0 TO 9
'        PRINT i, PEN(i)
'        LOCATE , 40
'NEXT i
'VIEW (0, 0)-(600, 400), , 1
'WINDOW SCREEN (0, 0)-(639, 224)
'PRESET (x!, y!)
'PRESET STEP(0, 1)
'PRESET STEP(0, 1)
'PRESET STEP(0, 1)
'PRESET STEP(0, 1)
'PRESET STEP(0, 1)
'PRESET STEP(0, 1)
'PRESET STEP(0, 1)
'PRESET STEP(5, 0)
'PRESET STEP(-1, -1)
'PRESET STEP(-1, -1)
'PRESET STEP(-1, -1)
'IF (PEN(4) <> x) OR (PEN(5) <> y) THEN
'        DRAW "c0 bm" + STR$(x) + "," + STR$(y) + "d"
'        x = PEN(4) / 1
'        y = PEN(5) / 1
'        DRAW "c11 bm" + STR$(x) + "," + STR$(y) + "d"
'END IF
'PSET (x!, y!)
'PSET (x + 0!, y + 1!)
'PSET (x + 0!, y + 1!)
'PSET (x + 0!, y + 1!)
'PSET (x + 0!, y + 1!)
'PSET (x + 0!, y + y + 1!)
'PSET (x + 0!, y + 1!)
'PSET (x + 0!, y + 1!)
'PSET (x + 5!, y + 0!)
'PSET (x + -1!, y + -1!)
'PSET (x + -1!, y + -1!)
'PSET (x + -1!, y + -1!)
'VIEW (ss(css).ViewX1, ss(css).ViewY1)-(ss(css).ViewX2, ss(css).ViewY2), , 1
'WINDOW (ss(css).WindowX1, ss(css).WindowY1)-(ss(css).WindowX2, ss(css).WindowY2)
END SUB

DEFDBL A-Z
FUNCTION GetX# (p%)
'c# = p.x * p.x
'c# = c# + (p.z * p.z)
'GetX# = p.x / SQR(c#)

a = ATN((Points(p%).x - me.x) / (Points(p%).z - me.z)) * (180 / PI)
c = a - Hangle
e = TAN(c * PI / 180)
GetX# = e * 2'/ (Points(p%).z - me.z)
'PRINT a, c, e, (Points(p%).x - me.x) / (Points(p%).z - me.z)
'GetX# = (Points(p%).x - me.x) / (Points(p%).z - me.z)
END FUNCTION

FUNCTION GetY# (p%)
'STOP
'c# = p.y * p.y
'c# = c# + (p.z * p.z)
'GetY# = p.y / SQR(c#)

b = ATN((Points(p%).y - me.y) / (Points(p%).z - me.z)) * (180 / PI)
d = b - Vangle
f = TAN(d * PI / 180)
GetY# = f * 2'/ (Points(p%).z - me.z)
'PRINT b, d, f, (Points(p%).y - me.y) / (Points(p%).z - me.z)
'GetX# = (Points(p%).x - me.x) / (Points(p%).z - me.z)
'GetY# = (Points(p%).y - me.y) / (Points(p%).z - me.z)
END FUNCTION

DEFSNG A-Z
SUB Init
SetPointCount 0
SetLineCount 0
SetSquareCount 0
zMin# = .2
PointSkip = 1

css = 1
ss(0).ScreenMode = 9
ss(0).ViewX1 = 1
ss(0).ViewX2 = 638
ss(0).ViewY1 = 1
ss(0).ViewY2 = 186
ss(0).DisplayStartLine = 10
ss(0).DisplayLineCount = 10
ss(0).WindowX1 = -6.38
ss(0).WindowX2 = 6.38
ss(0).WindowY1 = 1.86
ss(0).WindowY2 = -1.86

ss(1).ScreenMode = 12
ss(1).ViewX1 = 1
ss(1).ViewX2 = 638
ss(1).ViewY1 = 1
ss(1).ViewY2 = 260
ss(1).DisplayStartLine = 18
ss(1).DisplayLineCount = 12
ss(1).WindowX1 = -6.38
ss(1).WindowX2 = 6.38
ss(1).WindowY1 = 2.6
ss(1).WindowY2 = -2.6

ss(2).ScreenMode = 12
ss(2).ViewX1 = 1
ss(2).ViewX2 = 638
ss(2).ViewY1 = 1
ss(2).ViewY2 = 260
ss(2).DisplayStartLine = 18
ss(2).DisplayLineCount = 12
ss(2).WindowX1 = -6.38
ss(2).WindowX2 = 6.38
ss(2).WindowY1 = 2.6
ss(2).WindowY2 = -2.6

InitScreen
'DisplayLineCount% = 10
'DisplayStartLine% = 10
DisplayMode% = 2
ON KEY(1) GOSUB F1
ON KEY(2) GOSUB F2
ON KEY(11) GOSUB Up
ON KEY(12) GOSUB TurnLeft
ON KEY(13) GOSUB TurnRight
ON KEY(14) GOSUB Down
KEY 15, CHR$(0) + CHR$(51)
ON KEY(15) GOSUB Left
KEY 16, CHR$(0) + CHR$(52)
ON KEY(16) GOSUB Right
KEY(0) ON
ON PEN GOSUB penAction
PEN ON
me.x = 0
me.y = 0
me.z = -2
Change = .1
'OPEN "COM1" FOR OUTPUT AS 10': 2400,N,8,1
OPEN "d:\3dlog.txt" FOR OUTPUT AS 10
'OPEN "con" FOR OUTPUT AS 10
'OPEN "LPT1" FOR OUTPUT AS 10
END SUB

DEFINT A-Z
SUB InitScreen
SCREEN ss(css).ScreenMode, , 0, 0
FixScreen
END SUB

FUNCTION InputKey$
DO
        k$ = INKEY$
        IF k$ <> "" THEN
                InputKey = k$
                PRINT k$
                EXIT DO
        END IF
        'CALL FreeTime
LOOP
END FUNCTION

DEFSNG A-Z
FUNCTION MainMenu$
PRINT "1. Load"
PRINT "2. Save"
PRINT "3. View Raw"
PRINT "4. Show Files"
PRINT "5. Set My X,Y,Z,Hangle,Vangle"
PRINT "6. Move Mode"
PRINT "7. Edit Mode"
PRINT "8. Refresh"
PRINT "9. Setup"
PRINT "0. Exit"
'LOCATE , , 1, 0, 31

PRINT "Please select a choice:";
MainMenu$ = InputKey
END FUNCTION

DEFINT A-Z
SUB quit
CLOSE #10
SYSTEM
END SUB

DEFSNG A-Z
SUB Refresh
DIM tt AS DOUBLE, t      AS DOUBLE
'IF CurrentScreen% = 0 THEN
'        SCREEN ss(css).ScreenMode, , 0, 0
'        FixScreen
'ELSE
'        SCREEN 9, , 0, 1
'        FixScreen
'END IF
'CLS 1
tt = TIMER
SELECT CASE DisplayMode%
CASE 1
7       CLS 1
8       FOR p% = 1 TO PointCount% STEP PointSkip
9               ShowPoint p%
10              LOCATE , 1
11              PRINT #10, p%; "/"; PointCount%; CHR$(13);
12      NEXT p%
        'LOCATE , 1
        PRINT #10, ""
CASE 2
        CLS 1

        FOR l% = 1 TO LineCount%
                ShowLine l%, 1
        NEXT l%
        FOR p% = 1 TO PointCount%
                ShowPoint p%
        NEXT p%
'        PRINT "Refreshed"
CASE 3
        'STOP
        'temp =
        SortSquares
        delay% = 1000
        CLS 1
        t = TIMER
        FOR s% = 1 TO SquareCount%
                ShowSquare SortedSquares(s%)
'                PRINT #10, "Showing Squares"; CINT((s% / SquareCount%) * 100); "% done"; CHR$(13);
                'LOCATE CSRLIN - 1, 1
                FOR i = 1 TO delay%
                        CALL FreeTime
                NEXT i
        NEXT s%
        PRINT #10, ""
        PRINT #10, "time spent showing squares"; TIMER - t
        FOR l% = 1 TO LineCount%
                ShowLine l%, 1
                'FOR i = 1 TO delay%
                'NEXT i
        NEXT l%
'        FOR s% = 1 TO SquareCount%
'                ShowSquare s%
'        NEXT s%
'        FOR p% = 1 TO PointCount%
'                ShowPoint p%
'                FOR i = 1 TO delay%
'                NEXT i
'        NEXT p%
END SELECT
'IF CurrentScreen% = 0 THEN
'        SCREEN 9, , 1, 1
'        FixScreen
'        CurrentScreen% = 1
'ELSE
'        SCREEN 9, , 0, 0
'        FixScreen
'        CurrentScreen% = 0
'END IF
PRINT #10, "total time spent refreshing"; TIMER - tt; "total points"; PointCount%
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
        'STOP
        PRINT "old value"; Change
        KEY(0) OFF
        INPUT "enter new value", Change
        KEY(0) ON
        GOTO SetupLoop
CASE "5"
        'STOP
        PRINT "Old skip value"; PointSkip
        KEY(0) OFF
        INPUT "Enter new value", PointSkip
        KEY(0) ON
        Refresh
        GOTO SetupLoop
CASE "6"
        STOP
CASE "7"
        STOP
'CASE "8"
'        STOP
'CASE "9"
'        STOP
CASE ELSE
        PRINT "Error! Please Select an Option From Above"
        DO WHILE INKEY$ = ""
        LOOP
        GOTO SetupLoop
END SELECT
END SUB

FUNCTION SetupMenu$
'CLS 2
IF DisplayMode <> 1 THEN PRINT "1. Dot Mode"
IF DisplayMode <> 2 THEN PRINT "2. Line Mode"
IF DisplayMode <> 3 THEN PRINT "3. Graphic Mode"
PRINT "4. Set Change"
PRINT "5. Set Pointe Skip"
PRINT "0. Main Menu"
'LOCATE , , 1, 31, 0
PRINT "Please select a choice:";
SetupMenu$ = InputKey
END FUNCTION

SUB showFiles
SHELL "dir *.3d1 /b > list.lst"
OPEN "list.lst" FOR INPUT AS #1
i = 0
DO UNTIL EOF(1)
        IF i = ss(css).DisplayLineCount THEN
                PRINT "Press any key to continu"
                a$ = InputKey
                i = 0
        END IF
        INPUT #1, a$
        PRINT a$
        i = i + 1
LOOP
CLOSE #1
a$ = InputKey
'STOP
END SUB

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

DEFINT A-Z
SUB ShowLocation
PRINT "At"; CSNG(me.x); CSNG(me.y); CSNG(me.z); "    "
PRINT "Looking"; CSNG(Hangle); CSNG(Vangle)
END SUB

DEFSNG A-Z
SUB ShowPoint (p%)
DIM z#
IF (Points(p%).x = 0) AND (Points(p%).y = 0) AND (Points(p%).z = 0) THEN EXIT SUB
z# = Points(p%).z - me.z

IF z# > zMin# THEN
        PSET (GetX(p%), GetY(p%))
END IF
END SUB

DEFINT A-Z
SUB ShowSquare (p%)
IF Squares(p%).p1 = 0 THEN EXIT SUB
IF SquareDistance(p%) > zMin# THEN
        x1# = GetX(Squares(p%).p1)
        y1# = GetY(Squares(p%).p1)
        x2# = GetX(Squares(p%).p2)
        y2# = GetY(Squares(p%).p2)
        x3# = GetX(Squares(p%).p3)
        y3# = GetY(Squares(p%).p3)
        x4# = GetX(Squares(p%).p4)
        y4# = GetY(Squares(p%).p4)
      
        side1# = SQR(ABS((PMAP(x1#, 0) - PMAP(x2#, 0)) * (PMAP(y1#, 1) - PMAP(y2#, 1))))
        side2# = SQR(ABS((PMAP(x2#, 0) - PMAP(x3#, 0)) * (PMAP(y2#, 1) - PMAP(y3#, 1))))
        side3# = SQR(ABS((PMAP(x3#, 0) - PMAP(x4#, 0)) * (PMAP(y3#, 1) - PMAP(y4#, 1))))
        side4# = SQR(ABS((PMAP(x4#, 0) - PMAP(x1#, 0)) * (PMAP(y4#, 1) - PMAP(y1#, 1))))
        'area# = side1# * side3# * side2# * side4#
        'PRINT area#;
        IF side1# > smallest# THEN good = 1
        IF side2# > smallest# THEN good = good + 1
        IF side3# > smallest# THEN good = good + 1
        IF side4# > smallest# THEN good = good + 1
        PRINT good; side1#; side2#; side3#; side4#
        good = 3
    IF good > 2 THEN
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
    END IF
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
DIM t AS DOUBLE
KEY(0) STOP
t = TIMER
UpdateSquareDistance
FOR i = 1 TO SquareCount%
        'SortedSquares(i) = i
        Squares(i).sorted = "F"
        PRINT #10, "sort Squares part 1"; CINT((i / SquareCount%) * 100); "% done"; CHR$(13);
'        LOCATE CSRLIN - 1, 1
NEXT i
PRINT #10, ""
FOR i = 1 TO SquareCount%
        m = FindFarthestSquare
        SortedSquares(i) = m
        Squares(m).sorted = "T"
        PRINT #10, "Sort Squares Part 2"; CINT((i / SquareCount%) * 100); "% done"; CHR$(13);
'        LOCATE CSRLIN - 1, 1
NEXT i
PRINT #10, ""
PRINT #10, "Seconds spent sorting"; TIMER - t, "total squares"; SquareCount%
KEY(0) ON
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

