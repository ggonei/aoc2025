       IDENTIFICATION DIVISION.
       PROGRAM-ID. day3-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day4/inputtst'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction.
             02 gridx PIC X(10).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(11) VALUE 1.
            01 gridy.
             02 strip OCCURS 10 TIMES INDEXED BY posix.
              03 posy PIC X(10).
            01 posiy PIC s9(9).
            01 cnt PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eof > 10
            READ inputfile
             AT END
              ADD 1 TO eof
             NOT AT END
              MOVE gridx TO posy(eof)
              ADD 1 TO eof
           END-PERFORM.
           CLOSE inputfile.

           PERFORM VARYING posix FROM 1 BY 1 UNTIL posix > 10
            PERFORM VARYING posiy FROM 1 BY 1 UNTIL posiy > 10
             IF (posix = 1 OR posix = 10) AND (posiy = 1 OR posiy = 10)
             THEN
              IF posy(posix)(posiy:1) = "@" THEN DISPLAY "@" END-IF
             END-IF
             DISPLAY posix "," posiy
            END-PERFORM
           END-PERFORM.

           STOP RUN.
