DECLARE SUB calibate ()
DECLARE SUB SetPointCount (n%)
DECLARE SUB test ()
DECLARE SUB init ()
DECLARE SUB quit ()
DECLARE SUB answerRecevied (from AS INTEGER, answer AS STRING)
TYPE m3dPoint
x AS DOUBLE
y AS DOUBLE
z AS DOUBLE
sx AS DOUBLE
sy AS DOUBLE
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
''''''''''''''''''''''''''''''''program start''''''''''''''''''''''''''''''''
OPTION BASE 0
DIM answer AS STRING, char AS STRING * 1
DIM SHARED currentPoint(0 TO 253) AS INTEGER
DIM SHARED PointCount%
DIM SHARED LineCount%
DIM SHARED SquareCount%
PointCount% = 0
LineCount% = 0
SquareCount% = 0
DIM SHARED points(PointCount%) AS m3dPoint
DIM SHARED Lines(LineCount%) AS m3dLine
DIM SHARED Squares(SquareCount%) AS m3dSquare
DIM SHARED SortedSquares(SquareCount%) AS INTEGER


init
test
DO: LOOP WHILE INKEY$ = ""
quit

ComHandler1:
        PRINT "Something was typed at the terminal attached to COM1."
        INPUT #1, char
        PRINT char
        IF ASC(char) = 13 THEN
                answerRecevied 1, answer
                answer = ""
        ELSE
                answer = answer + char
        END IF
        RETURN

SUB answerRecevied (from AS INTEGER, answer AS STRING)
PRINT "answer recevied from "; from; "containing '"; answer; "'"
p% = INSTR(1, answer, " ")
comand$ = MID$(answer, 1, p% - 1)
arg$ = MID$(answer, p% + 1)
PRINT "'"; comand$; "'", "'"; arg$; "'", "'"; CVI(arg$); "'"
SELECT CASE comand$
CASE "currentPoint"
        currentPoint(from) = CVI(arg$)
        PRINT from; "'s current point set to "; CVI(arg$)
CASE "sx"
        points(currentPoint(from)).sx = CVI(arg$)
        PRINT "point "; currentPoint(from); "has had sx set to "; CVI(arg$)
CASE "sy"
        points(currentPoint(from)).sy = CVI(arg$)
        PRINT "point "; currentPoint(from); "has had sy set to "; CVI(arg$)
END SELECT
END SUB

SUB calibate
max& = 214748'3647
start! = TIMER
FOR i& = 1 TO max&
'IF (i& MOD 10000) = 0 THEN PRINT i&
NEXT i&
done! = TIMER
total! = done! - start!
PRINT total!
corect& = max& / total!
start! = TIMER
FOR i& = 1 TO corect&
'IF (i& MOD 10000) = 0 THEN PRINT i&
NEXT i&
done! = TIMER
total! = done! - start!
PRINT total!
END SUB

SUB init
COM(1) ON       'Enable event trapping on port 1.
ON COM(1) GOSUB ComHandler1
OPEN "COM1: 2400,N,8,1" FOR RANDOM AS 1
calibate
END SUB

SUB quit
COM(1) OFF
CLOSE 1
END
END SUB

SUB SetPointCount (n%)
DIM temp(PointCount%) AS m3dPoint
IF n% < PointCount% THEN e = n% ELSE e = PointCount%
PointCount% = n%
FOR i = 0 TO e
        temp(i) = points(i)
NEXT i
REDIM points(PointCount%)
FOR i = 0 TO e
        points(i) = temp(i)
NEXT i
END SUB

SUB test
SetPointCount 1
answerRecevied 1, "currentPower " + MKI$(1)
answerRecevied 1, "currentPoint " + MKI$(1)
answerRecevied 1, "sx " + MKI$(1)
answerRecevied 1, "sy " + MKI$(1)
END SUB

