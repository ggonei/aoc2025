       IDENTIFICATION DIVISION.
       PROGRAM-ID. day3-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day3/inputtst'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction.
             02 nstr PIC X(100).

           WORKING-STORAGE SECTION.
            01 nbatts PIC 9(3) VALUE 12.
            01 posx.
             02 pos OCCURS 12 TIMES INDEXED BY posidx.
              03 posv PIC 9(1).
            01 eof PIC 9(1) VALUE 0.
            01 highn PIC s9(1).
            01 highestn PIC s9(1) VALUE 9.
            01 nmax PIC s9(1) VALUE 9.
            01 idx PIC 9(3).
            01 posi PIC 9(3).
            01 posn PIC 9(3).
            01 posf PIC 9(3).
            01 nstrtrunc PIC X(100).
            01 total PIC 9(18) VALUE 0.
            01 val PIC 9(12) VALUE 0.
            01 lengthc PIC 9(3) VALUE 100.
            01 curidx PIC 9(3) VALUE 100.
            01 divider PIC 9(2) VALUE 1.
            01 extrad PIC 9(3) VALUE 0.
            01 remchar PIC X(1).

       PROCEDURE DIVISION.
           MOVE 1 TO posidx
           OPEN INPUT inputfile.
           PERFORM UNTIL eof = 1
            READ inputfile
             AT END
              MOVE 1 TO eof
             NOT AT END
              IF FUNCTION NUMVAL(nstr) > 9
              DISPLAY nstr
              INITIALIZE posx
              MOVE nstr TO nstrtrunc
              MOVE nmax TO highestn
              MOVE 100 TO lengthc
              PERFORM VARYING posidx FROM 1 BY 1 UNTIL posidx > nbatts
               PERFORM VARYING highn FROM highestn BY -1 UNTIL highn < 1
                PERFORM Findhigh
               END-PERFORM
              END-PERFORM
              DISPLAY posx
              COMPUTE val = FUNCTION NUMVAL(posx)
              COMPUTE total = total + val
              END-IF
           END-PERFORM.
           CLOSE inputfile.
           DISPLAY total.
           STOP RUN.

           Findhigh.
            COMPUTE divider = FUNCTION INTEGER(
             FUNCTION LOG10(
              FUNCTION NUMVAL(nstrtrunc)
             )) + 1
            PERFORM VARYING idx FROM 1 BY 1
             UNTIL idx > LENGTH OF nstrtrunc - nbatts
      *      DISPLAY posx
      *      DISPLAY highn ":" nstrtrunc "," idx "(" posidx ")"
              IF nstrtrunc(idx:1) = highn THEN
      *      DISPLAY "1:" nstrtrunc(idx:)
               IF posv(posidx) = 0 OR posidx = nbatts + 1 THEN
      *      DISPLAY "2:"
                IF (lengthc - idx) >= (nbatts - posidx) THEN
      *      DISPLAY "3:" idx
                 SUBTRACT idx FROM lengthc
                 IF posidx = nbatts + 1 THEN
                  COMPUTE posidx = nbatts - idx + divider
                 END-IF
                 COMPUTE posv(posidx) = FUNCTION NUMVAL(
                  nstrtrunc(idx:1)
                 )
                 MOVE nmax TO highestn
                 MOVE highestn TO highn
                 ADD 1 to posidx
                 MOVE nstrtrunc(idx + 1:)
                  TO nstrtrunc
                 PERFORM Findhigh
                ELSE
                 IF divider < nbatts THEN
                  COMPUTE curidx = posidx
                  COMPUTE extrad = divider - (nbatts - posidx + 1)
                  PERFORM UNTIL extrad < 1
                   PERFORM VARYING posidx FROM 1 BY 1
                   UNTIL posidx > divider
                    IF nstrtrunc(posidx + 1:1) > nstrtrunc(posidx:1)
                    THEN
                     MOVE nstrtrunc(posidx:1) TO remchar
                     MOVE nstrtrunc(posidx + 1:1) TO nstrtrunc(posidx:1)
                     MOVE remchar TO nstrtrunc(posidx + 1:1)
                    END-IF
                   END-PERFORM
                   DISPLAY posx ":" nstrtrunc
                   SUBTRACT 1 FROM extrad
                  END-PERFORM
                  DISPLAY posx ":" nstrtrunc
                  PERFORM VARYING posidx FROM curidx BY 1
                   UNTIL posidx > nbatts
                   IF posv(posidx) = 0 THEN
                    MOVE nstrtrunc(posidx - curidx + 1:1)
                     TO posv(posidx)
                   END-IF
                  END-PERFORM
                 END-IF
                END-IF
               END-IF
              END-IF
            END-PERFORM.
