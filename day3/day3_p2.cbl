       IDENTIFICATION DIVISION.
       PROGRAM-ID. day3-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day3/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction.
             02 nstr PIC X(15).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(1) VALUE 0.
            01 highn PIC s9(1).
            01 highestn PIC s9(1) VALUE 9.
            01 nmax PIC s9(1) VALUE 9.
            01 idx PIC 9(3).
            01 posi PIC 9(3).
            01 posn PIC 9(3).
            01 posf PIC 9(3).
            01 posx.
             02 pos OCCURS 12 TIMES INDEXED BY posidx.
              03 posv PIC 9(1).
            01 nstrtrunc PIC X(15).
            01 total PIC 9(18) VALUE 0.
            01 val PIC 9(12) VALUE 0.
            01 lengthc PIC 9(2) VALUE 15.
            01 curidx PIC 9(2) VALUE 15.

       PROCEDURE DIVISION.
           MOVE 1 TO posidx
           OPEN INPUT inputfile.
           PERFORM UNTIL eof = 1
            READ inputfile
             AT END
              MOVE 1 TO eof
             NOT AT END
              IF nstr > 1
              DISPLAY nstr
              INITIALIZE posx
              MOVE nstr TO nstrtrunc
              MOVE nmax TO highestn
              MOVE 15 TO lengthc
              PERFORM VARYING posidx FROM 1 BY 1 UNTIL posidx > 12
               PERFORM VARYING highn FROM highestn BY -1 UNTIL highn < 2
                PERFORM Findhigh
               END-PERFORM
               MOVE nstrtrunc(2:LENGTH OF nstrtrunc - 2) TO nstrtrunc
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
            PERFORM VARYING idx FROM 1 BY 1
             UNTIL idx > (LENGTH OF nstrtrunc) - 12
              IF nstrtrunc(idx:1) = highn
               IF posv(posidx) = 0
                COMPUTE posv(posidx) = FUNCTION NUMVAL(
                 nstrtrunc(idx:1)
                )
                SUBTRACT idx FROM lengthc
                MOVE nmax TO highestn
                MOVE highestn TO highn
                IF lengthc > 12 - posidx THEN
                 ADD 1 to posidx
                 MOVE nstrtrunc(idx + 1:LENGTH OF nstrtrunc - idx + 1)
                  TO nstrtrunc
                 PERFORM Findhigh
                ELSE
                 MOVE nstrtrunc(LENGTH OF nstrtrunc - 13:12)
                  TO nstrtrunc
                 MOVE posidx TO curidx
                 PERFORM VARYING posidx FROM curidx BY 1
                 UNTIL posidx > 12
                  MOVE nstrtrunc(posidx - curidx + 1:1) TO posv(posidx)
                 END-PERFORM
                END-IF
               END-IF
              END-IF
            END-PERFORM.
