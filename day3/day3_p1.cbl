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
             02 nstr PIC X(102).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(1) VALUE 0.
            01 highn PIC s9(1).
            01 highestn PIC s9(1) VALUE 9.
            01 nmax PIC s9(1) VALUE 9.
            01 idx PIC 9(3).
            01 posi PIC 9(3).
            01 posf PIC 9(3).
            01 nstrtrunc PIC X(102).

       PROCEDURE DIVISION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eof = 1
            READ inputfile
             AT END
              MOVE 1 TO eof
             NOT AT END
              DISPLAY nstr
              MOVE nstr TO nstrtrunc
              MOVE 0 TO posi
              MOVE 0 TO posf
              MOVE nmax TO highestn
              PERFORM UNTIL posi NOT= 0 AND posf NOT= 0
               PERFORM VARYING highn FROM highestn BY -1 UNTIL highn < 0
                PERFORM Findhigh
               END-PERFORM
               IF posf = 0 THEN
                COMPUTE highestn = FUNCTION NUMVAL(nstr(posi:1)) - 1
                MOVE 0 TO posi
                MOVE nstr TO nstrtrunc
               ELSE
                DISPLAY "NEXT"
               END-IF
              END-PERFORM
           END-PERFORM.
           CLOSE inputfile.
           STOP RUN.

           Findhigh.
            PERFORM VARYING idx FROM 1 BY 1
             UNTIL idx > LENGTH OF nstrtrunc - 1
              IF nstrtrunc(idx:1) = highn
               IF posi = 0
                MOVE idx TO posi
                MOVE nstrtrunc(idx + 1:LENGTH OF nstrtrunc - posi + 1)
                 TO nstrtrunc
              MOVE nmax TO highestn
                MOVE highestn TO highn
                PERFORM Findhigh
               ELSE
                IF posf = 0
                 COMPUTE posf = posi + idx
                 DISPLAY nstr(posi:1) nstr(posf:1)
                END-IF
               END-IF
              END-IF
            END-PERFORM.
