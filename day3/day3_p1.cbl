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
            01 pos PIC 9(3) VALUE 1.
            01 highn PIC 9(1).
            01 idx PIC 9(3).
            01 posi PIC 9(3).

       PROCEDURE DIVISION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eof = 1
            READ inputfile
             AT END
              MOVE 1 TO eof
             NOT AT END
      *        PERFORM VARYING highn FROM 9 BY -1 UNTIL highn < 0
              PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > LENGTH OF nstr
               IF nstr(idx:1) = "9"
                MOVE idx TO posi
                EXIT PERFORM
               END-IF
              END-PERFORM
              DISPLAY nstr(1:posi)
      *        END-PERFORM
           END-PERFORM.
           CLOSE inputfile.
           STOP RUN.
