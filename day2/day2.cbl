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
            01 myarray.
             02 myitem OCCURS 100 TIMES INDEXED BY idx.
              03 itm PIC X(50).
            01 startpos PIC 9(4) VALUE 1.
            01 endpos PIC 9(4) VALUE 1.
            01 diffpos PIC 9(4) VALUE 0.
            01 newidx PIC 9(4) VALUE 1.
            01 numitems PIC 9(4) VALUE 0.
            01 min PIC 9(10) VALUE 0.
            01 max PIC 9(10) VALUE 0.
            01 echoed PIC 9(10) VALUE 0.
            01 iterval PIC 9(10) VALUE 0.
            01 checkmin PIC 9(5) VALUE 0.
            01 checkmax PIC 9(5) VALUE 0.
            01 leadzs PIC 9(2) VALUE 0.
            01 repeats PIC 9(2) VALUE 0.
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
            DISPLAY min "->" max
            COMPUTE checkmin = min / ( 10 **
             (
              FUNCTION INTEGER(FUNCTION LOG10(min)) + 1 -
              FUNCTION INTEGER(
               (FUNCTION INTEGER(FUNCTION LOG10(min)) + 1)
              / 2)
             )
            )
            COMPUTE checkmax = max / ( 10 **
             (
              FUNCTION INTEGER(FUNCTION LOG10(max)) -
              FUNCTION INTEGER(
               (FUNCTION INTEGER(FUNCTION LOG10(max)))
              / 2)
             )
            )
            DISPLAY checkmin "->" checkmax
            PERFORM VARYING iterval FROM min BY 1 UNTIL iterval > max
             PERFORM VARYING echoed FROM checkmin BY 1
             UNTIL echoed > checkmax
              MOVE 0 TO leadzs
              MOVE 0 TO repeats
              INSPECT echoed TALLYING leadzs FOR LEADING ZEROES
              INSPECT iterval TALLYING repeats
               FOR ALL echoed(leadzs + 1:)
              DISPLAY iterval ":" echoed
              IF repeats = 2 AND
               FUNCTION LOG10(checkmax) < LENGTH OF endpos(leadzs + 1:)
              THEN
               MOVE SPACES TO strcat
               STRING echoed(leadzs + 1:) DELIMITED BY SPACE
                      echoed(leadzs + 1:) DELIMITED BY SPACE
                INTO strcat
               END-STRING
               IF FUNCTION NUMVAL(strcat) = FUNCTION NUMVAL(iterval)
                THEN
                 ADD iterval TO counter
               END-IF
              END-IF
             END-PERFORM
            END-PERFORM
           END-PERFORM.

           DISPLAY counter.
           CLOSE inputfile.
           STOP RUN.
