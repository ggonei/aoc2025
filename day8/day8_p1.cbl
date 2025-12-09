       IDENTIFICATION DIVISION.
       PROGRAM-ID. day8-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day8/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(18).

           WORKING-STORAGE SECTION.
            01 eofile PIC 9(1) VALUE 0.
            01 broken PIC 9(1) VALUE 1.
            01 closest PIC s9(6) VALUE 1.
            01 cnt PIC s9(6) VALUE 0.
            01 circuitn PIC s9(7) VALUE 0.
            01 hd PIC s9(18) VALUE 0.
            01 hdsmallest PIC s9(18) VALUE 9999999999999999.
            01 ans1 PIC s9(6) VALUE 0.
            01 ans2 PIC s9(6) VALUE 0.
            01 ans3 PIC s9(6) VALUE 0.
            01 idx PIC s9(6) VALUE 0.
            01 idx2 PIC s9(6) VALUE 0.
            01 starti PIC s9(6) VALUE 0.
            01 numitems PIC s9(7) VALUE 0.
            01 pairs PIC s9(5) VALUE 1000.
            01 tmpstring PIC X(1001) VALUE SPACES.
            01 positions.
             02 place OCCURS 100001 TIMES.
              03 posi PIC s9(7) OCCURS 3 TIMES.
            01 closearr.
             02 pidx OCCURS 990001 TIMES.
              03 distance PIC s9(18) VALUE 9999999999999999.
              03 origpos PIC s9(6) VALUE 999999.
              03 neighbour PIC s9(6) VALUE 999999.
              03 circuit PIC s9(7) VALUE 0.

       PROCEDURE DIVISION.
      *Annoying because no comparisons between ints and signed ints
           OPEN INPUT inputfile.
           PERFORM UNTIL eofile > 0
            READ inputfile
             AT END
              ADD 1 TO eofile
             NOT AT END
              ADD 1 TO idx
              MOVE idx TO numitems
              UNSTRING instruction DELIMITED BY "," INTO
               posi(idx,1)
               posi(idx,2)
               posi(idx,3)
           END-PERFORM.
           CLOSE inputfile.
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
            ADD 1 TO idx GIVING starti
            PERFORM VARYING idx2 FROM idx BY 1 UNTIL idx2 > numitems
             IF idx NOT= idx2 THEN
              COMPUTE hd = (posi(idx,1) - posi(idx2,1)) ** 2
               + (posi(idx,2) - posi(idx2,2)) ** 2
               + (posi(idx,3) - posi(idx2,3)) ** 2
              MOVE hd TO distance(closest)
              IF hd < hdsmallest THEN MOVE hd TO hdsmallest END-IF
              MOVE idx TO origpos(closest)
              MOVE idx2 TO neighbour(closest)
              ADD 1 TO closest
             END-IF
            END-PERFORM
           END-PERFORM.
           DISPLAY "numitems:" numitems ",pairs:" closest ";" hdsmallest
           MOVE 0 TO idx
           SORT pidx ON ASCENDING KEY distance.
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > pairs
            PERFORM VARYING idx2 FROM 1 BY 1 UNTIL idx2 > idx
             IF (origpos(idx) = origpos(idx2)
             OR origpos(idx) = neighbour(idx2)
             OR neighbour(idx) = origpos(idx2)
             OR neighbour(idx) = neighbour(idx2))
             THEN
              IF circuit(idx) > circuit(idx2) OR circuit(idx) = 0 THEN
               MOVE circuit(idx2) TO circuit(idx)
              END-IF
             END-IF
            END-PERFORM
            IF circuit(idx) = 0 THEN
             ADD 1 TO circuitn
             MOVE circuitn TO circuit(idx)
            END-IF
           END-PERFORM.
           PERFORM VARYING starti FROM 1 BY 1 UNTIL starti > 19
           DISPLAY starti
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > pairs
            PERFORM VARYING idx2 FROM 1 BY 1 UNTIL idx2 > pairs
             IF (origpos(idx) = origpos(idx2)
             OR origpos(idx) = neighbour(idx2)
             OR neighbour(idx) = origpos(idx2)
             OR neighbour(idx) = neighbour(idx2))
             THEN
              IF circuit(idx) > circuit(idx2) OR circuit(idx) = 0 THEN
               DISPLAY pidx(idx) ", " pidx(idx2)
               MOVE circuit(idx2) TO circuit(idx)
              END-IF
             END-IF
            END-PERFORM
           END-PERFORM
           END-PERFORM.
           SORT pidx ON DESCENDING KEY circuit.
           PERFORM VARYING starti FROM 1 BY 1 UNTIL starti > pairs + 1
           DISPLAY pidx(starti)
            IF circuit(starti) NOT= circuit(starti - 1) THEN
             MOVE 0 TO cnt
             INSPECT tmpstring TALLYING cnt FOR ALL "X"
             DISPLAY cnt
             IF ans1 < cnt THEN
              MOVE ans2 TO ans3
              MOVE ans1 TO ans2
              MOVE cnt TO ans1
             ELSE IF ans2 < cnt THEN
               MOVE ans2 TO ans3
               MOVE cnt TO ans2
              ELSE IF ans3 < cnt THEN
                MOVE cnt TO ans3
               END-IF
              END-IF
             END-IF
             MOVE SPACES TO tmpstring
             MOVE "X" TO tmpstring(origpos(starti):1)
             MOVE "X" TO tmpstring(neighbour(starti):1)
             DISPLAY FUNCTION TRIM(tmpstring)
            ELSE
             MOVE "X" TO tmpstring(origpos(starti):1)
             MOVE "X" TO tmpstring(neighbour(starti):1)
             DISPLAY FUNCTION TRIM(tmpstring)
            END-IF
           END-PERFORM.
           DISPLAY ans3.
           DISPLAY ans2.
           DISPLAY ans1.
           COMPUTE hd = ans1 * ans2 * ans3.
           DISPLAY hd.
           STOP RUN.
