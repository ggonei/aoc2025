       IDENTIFICATION DIVISION.
       PROGRAM-ID. day5-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day5/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(100).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(1) VALUE 0.
            01 section2 PIC 9(1) VALUE 0.
            01 min PIC 9(18) VALUE 0.
            01 max PIC 9(18) VALUE 0.
            01 numitems PIC 9(4) VALUE 0.
            01 counter PIC 9(4) VALUE 0.
            01 myarray.
             02 myitem OCCURS 500 TIMES INDEXED BY idx.
              03 itm PIC X(500).

       PROCEDURE DIVISION.
           OPEN INPUT inputfile.
           MOVE 1 TO idx
           PERFORM UNTIL eof > 0
            READ inputfile
             AT END
              ADD 1 TO eof
             NOT AT END
              IF instruction = SPACE THEN
               ADD 1 TO section2
               MOVE idx TO numitems
               MOVE 1 TO idx
              END-IF
              IF section2 = 0 THEN
               MOVE instruction TO myitem(idx)
               COMPUTE idx = idx + 1
              ELSE
               PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
                MOVE 0 TO min, max
                UNSTRING myitem(idx) DELIMITED BY "-" INTO min,max
      *         DISPLAY instruction ": " min ", " max
                IF (min <= FUNCTION NUMVAL(instruction)
                AND FUNCTION NUMVAL(instruction) <= max) THEN
                 ADD 1 TO counter
                 EXIT PERFORM
                END-IF
               END-PERFORM
              END-IF
           END-PERFORM.
           SUBTRACT 1 FROM counter.
           DISPLAY counter.
           CLOSE inputfile.
           STOP RUN.
