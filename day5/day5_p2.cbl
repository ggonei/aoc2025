       IDENTIFICATION DIVISION.
       PROGRAM-ID. day5-p2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day5/inputtst'
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
            01 min2 PIC 9(18) VALUE 0.
            01 max2 PIC 9(18) VALUE 0.
            01 diff PIC 9(18) VALUE 0.
            01 numitems PIC 9(4) VALUE 0.
            01 counter PIC 9(4) VALUE 0.
            01 myarray.
             02 myitem OCCURS 500 TIMES INDEXED BY idx, idx2.
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
               ADD 1 TO eof
               COMPUTE numitems = idx - 1
               MOVE 1 TO idx
               EXIT PERFORM
              END-IF
              MOVE instruction TO myitem(idx)
              COMPUTE idx = idx + 1
           END-PERFORM.

           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
            MOVE 0 TO min, max
            UNSTRING myitem(idx) DELIMITED BY "-" INTO min,max
            DISPLAY min "-" max
            PERFORM VARYING idx2 FROM 1 BY 1 UNTIL idx2 > numitems
             UNSTRING myitem(idx2) DELIMITED BY "-" INTO min2,max2
             IF max >= min2 AND max < max2 THEN
              COMPUTE max = min2 - 1
             END-IF
             IF min <= max2 AND min > min2 THEN
              COMPUTE min = max2 + 1
             END-IF
             STRING min "-" max
              INTO myitem(idx)
            END-PERFORM
           END-PERFORM.
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
            DISPLAY myitem(idx)
            MOVE 0 TO min, max
            UNSTRING myitem(idx) DELIMITED BY "-" INTO min,max
            COMPUTE diff = max - min + 1
            ADD diff TO counter
           END-PERFORM.

           DISPLAY counter.
           CLOSE inputfile.
           STOP RUN.
