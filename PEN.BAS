DECLARE SUB freetime ()
SCREEN 8
VIEW PRINT 14 TO 25
VIEW (1, 1)-(200, 100)
WINDOW SCREEN (0, 0)-(639, 199)
ON PEN GOSUB h
PEN ON
DO
freetime
LOOP

h:
LOCATE 14, 1
PRINT PEN(4), PEN(5)
RETURN

SUB freetime
LOCATE 15, 1
FOR i = 0 TO 9
        PRINT i, PEN(i)
NEXT i
IF (lastx <> PEN(4) OR lasty <> PEN(5)) THEN
        CLS 1
        PSET (PEN(4), PEN(5))
        lastx = PEN(4)
        lasty = PEN(5)
END IF
END SUB

