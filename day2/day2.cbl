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
            01 numitems            PIC 9(10) VALUE 0.
            01 startpos             PIC 9(10) VALUE 1.
            01 endpos               PIC 9(10) VALUE 0.
            01 diffpos               PIC 9(10) VALUE 0.

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
             COMPUTE newidx = (FUNCTION LOG10(max) + 1) / 2
             DISPLAY newidx
      *     PERFORM VARYING endpos FROM 1 UNTIL endpos > newidx
      *       DISPLAY endpos
      *      END-PERFORM
            END-PERFORM
           END-PERFORM.

           CLOSE inputfile.
           STOP RUN.
