       IDENTIFICATION DIVISION.
       PROGRAM-ID. day2-p2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
             'Users/georgeoneill/ess-dmsc/aoc2025/day2/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(500).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(1) VALUE 0.
            01 myarray.
             02 myitem OCCURS 100 TIMES INDEXED BY idx.
              03 itm PIC X(50).
            01 mytable.
             02 myentry OCCURS 10000 TIMES INDEXED BY tidx.
              03 myval PIC X(10).
            01 curentries PIC 9(4) VALUE 0.
            01 iter PIC 9(4).
            01 startpos PIC 9(4) VALUE 1.
            01 endpos PIC 9(4) VALUE 1.
            01 diffpos PIC 9(4) VALUE 0.
            01 newidx PIC 9(4) VALUE 1.
            01 numitems PIC 9(4) VALUE 0.
            01 min PIC 9(10) VALUE 0.
            01 max PIC 9(10) VALUE 0.
            01 echoed PIC 9(10) VALUE 0.
            01 checkmax PIC 9(6) VALUE 0.
            01 divider PIC 9(10) VALUE 0.
            01 leadzs PIC 9(5) VALUE 0.
            01 repeat PIC 9(6) VALUE 0.
            01 counter PIC 9(18) VALUE 0.
            01 strcat PIC X(10) VALUE SPACES.
            01 repval PIC 9(6) VALUE 0.
            01 unique PIC 9(1) VALUE 1.

       PROCEDURE DIVISION.
           MOVE 1 TO idx.
           MOVE 1 TO tidx.
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
            DISPLAY min "->" max
            PERFORM Loop
           END-PERFORM.
      *     DISPLAY mytable.

           DISPLAY counter.
           CLOSE inputfile.
           STOP RUN.

           Loop.
            COMPUTE divider = FUNCTION INTEGER(FUNCTION LOG10(max)) + 1
            COMPUTE checkmax = max / divider
            PERFORM VARYING repval FROM 1 BY 1 UNTIL repval > checkmax
             MOVE 0 TO leadzs
             INSPECT repval TALLYING leadzs FOR LEADING ZEROES
             MOVE repval(leadzs + 1:) TO strcat
           IF
             FUNCTION MOD(divider, (LENGTH OF repval) - leadzs) <= 1
            THEN
              PERFORM VARYING repeat FROM 2 BY 1 UNTIL repeat > divider
               STRING strcat DELIMITED BY SPACE
                      repval(leadzs + 1:) DELIMITED BY SPACE
                INTO strcat
               END-STRING
               MOVE FUNCTION NUMVAL(strcat) TO echoed
               IF min <= echoed AND echoed <= max THEN
                PERFORM Adddata
                EXIT PERFORM
               END-IF
              END-PERFORM
            END-IF
            END-PERFORM.

           Adddata.
            MOVE 1 TO unique
            PERFORM VARYING iter FROM 1 UNTIL iter > curentries
             IF myval(iter) = echoed
              MOVE 0 TO unique
              DISPLAY echoed " already written"
             END-IF
            END-PERFORM
            IF unique = 1
             ADD 1 TO curentries
             MOVE echoed TO myval(curentries)
             ADD echoed TO counter
            END-IF.
