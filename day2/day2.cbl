       IDENTIFICATION DIVISION.
       PROGRAM-ID. day2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
             'Users/georgeoneill/ess-dmsc/aoc2025/day2/inputtst'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(500).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(1) VALUE 0.
            01 newidx PIC 9(10) VALUE 0.
            01 min PIC 9(10) VALUE 0.
            01 max PIC 9(10) VALUE 0.
            01 myarray.
             02 myitem OCCURS 100 TIMES INDEXED BY idx.
              03 itm PIC X(50).
            01 numitems PIC 9(10) VALUE 0.
            01 startpos PIC 9(10) VALUE 1.
            01 endpos PIC 9(10) VALUE 0.
            01 diffpos PIC 9(10) VALUE 0.
            01 diffpos2 PIC 9(10) VALUE 0.
            01 counter PIC 9(10) VALUE 0.
            01 strcat PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
           MOVE 1 TO idx.
           OPEN INPUT inputfile.
           READ inputfile INTO instruction.

           PERFORM UNTIL startpos > LENGTH OF instruction

            PERFORM VARYING endpos FROM startpos BY 1
             UNTIL instruction(endpos:1) = ","
              OR endpos > LENGTH OF instruction
               CONTINUE
            END-PERFORM

            COMPUTE diffpos = endpos - startpos
            MOVE instruction(startpos:diffpos) TO myitem(idx)
            COMPUTE idx = idx + 1
            COMPUTE startpos = endpos + 1

           END-PERFORM.

           COMPUTE newidx = idx - 1.
           MOVE newidx TO numitems.

           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
            UNSTRING myitem(idx) DELIMITED BY "-" INTO min,max
            DISPLAY min " " max
            PERFORM VARYING startpos FROM min BY 1 UNTIL startpos > max
             COMPUTE newidx = 10 ** FUNCTION
                              INTEGER((FUNCTION LOG10(max) + 1) / 2) - 1
             PERFORM VARYING endpos FROM 1 BY 1 UNTIL endpos > newidx
              MOVE 0 TO diffpos
              MOVE 0 TO diffpos2
              INSPECT endpos TALLYING diffpos FOR LEADING ZEROES
              INSPECT startpos TALLYING diffpos2
               FOR ALL endpos(diffpos + 1:)
              IF diffpos2 > 1 THEN
               MOVE SPACES TO strcat
               PERFORM UNTIL diffpos2 < 1
                STRING strcat DELIMITED BY SPACE
                       endpos(diffpos + 1:) DELIMITED BY SPACE
                 INTO strcat
                END-STRING
                SUBTRACT 1 FROM diffpos2
               END-PERFORM
               DISPLAY strcat
               DISPLAY startpos
               IF FUNCTION NUMVAL(strcat) = FUNCTION NUMVAL(startpos)
               THEN
                DISPLAY "X"
              END-IF
             END-PERFORM
            END-PERFORM
           END-PERFORM.

           DISPLAY counter.
           CLOSE inputfile.
           STOP RUN.
