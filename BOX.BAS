TYPE m3dPoint
x AS DOUBLE
y AS DOUBLE
z AS DOUBLE
END TYPE

TYPE m3dLine
one AS INTEGER
two AS INTEGER
END TYPE

DIM j AS INTEGER

'INPUT "a's start, stop, step ", d, e, f
'INPUT "b's start, stop, step ", g, h, i
'INPUT "radias", r
j = 1500
lineCountess = 3000
'PRINT j
DIM Points(1 TO j) AS m3dPoint
DIM Lines(1 TO lineCountess) AS m3dLine
c = 1
LineCount% = 1
FOR z = 0 TO 30 STEP 1
 last = 0
 FOR x = 0 TO 16 STEP 1
  Points(c).x = x
  Points(c).y = 0
  Points(c).z = z
  'PRINT x, 0, z
  IF last = 0 THEN
   last = c
  ELSE
   Lines(LineCount%).one = last
   Lines(LineCount%).two = c
   last = c
   LineCount% = LineCount% + 1
  END IF
  PRINT "roufly " + STR$(INT((c / j) * 100)) + "% of part 1 is done", c; j, a; B
  c = c + 1
 NEXT x
 last = 0
 FOR y = 0 TO 8 STEP 1
  Points(c).x = 0
  Points(c).y = y
  Points(c).z = z
  IF last = 0 THEN
   last = c
  ELSE
   Lines(LineCount%).one = last
   Lines(LineCount%).two = c
   last = c
   LineCount% = LineCount% + 1
  END IF
  PRINT "roufly " + STR$(INT((c / j) * 100)) + "% of part 1 is done", c; j, a; B
  c = c + 1
 NEXT y
 last = 0
 FOR y = 0 TO 8 STEP 1
  Points(c).x = 16
  Points(c).y = y
  Points(c).z = z
  IF last = 0 THEN
   last = c
  ELSE
   Lines(LineCount%).one = last
   Lines(LineCount%).two = c
   last = c
   LineCount% = LineCount% + 1
  END IF
  PRINT "roufly " + STR$(INT((c / j) * 100)) + "% of part 1 is done", c; j, a; B
  c = c + 1
 NEXT y
NEXT z
c = c - 1
FOR a = 1 TO c - 35
 Lines(LineCount%).one = a
 Lines(LineCount%).two = a + 35
 LineCount% = LineCount% + 1
NEXT a
PRINT LineCount%, lineCountess
PRINT "roufly " + STR$(INT((c / j) * 100)) + "% done", c, j

        INPUT "Enter File Name:", FileName$
        OPEN FileName$ FOR OUTPUT AS 1
        PRINT #1, c
        FOR i = 1 TO c
                PRINT #1, Points(i).x, Points(i).y, Points(i).z
                PRINT STR$(INT((i / (c)) * 100)) + "% done", i, c
        NEXT i
        PRINT #1, LineCount%
'        PRINT #1, 0
        FOR i = 1 TO LineCount%
                PRINT #1, Lines(i).one, Lines(i).two
        NEXT i
'        PRINT #1, SquareCount%
        PRINT #1, 0
'        FOR i = 1 TO SquareCount%
'                s = Squares(i)
'                PRINT #1, s.p1, s.p2, s.p3, s.p4, s.colour
'        NEXT i
        CLOSE 1

