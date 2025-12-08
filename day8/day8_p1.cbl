       IDENTIFICATION DIVISION.
       PROGRAM-ID. day8-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day8/inputtst'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(18).

           WORKING-STORAGE SECTION.
            01 eofile PIC 9(1) VALUE 0.
            01 resetter PIC 9(1) VALUE 0.
            01 closest PIC s9(5) VALUE 1.
            01 cnt PIC 9(4) VALUE 0.
            01 hd PIC s9(16) VALUE 0.
            01 hdsmallest PIC s9(16) VALUE 0.
            01 ans1 PIC s9(5) VALUE 0.
            01 ans2 PIC s9(5) VALUE 0.
            01 ans3 PIC s9(5) VALUE 0.
            01 tmp PIC s9(5) VALUE 0.
            01 tmp2 PIC s9(5) VALUE 0.
            01 idx PIC s9(5) VALUE 0.
            01 idx2 PIC s9(5) VALUE 0.
            01 starti PIC s9(5) VALUE 0.
            01 numitems PIC s9(5) VALUE 0.
            01 pairs PIC s9(5) VALUE 10.
            01 filled PIC X(30) VALUE SPACES.
            01 positions.
             02 place OCCURS 30 TIMES.
              03 posi PIC 9(5) OCCURS 3 TIMES.
            01 closearr.
             02 pidx OCCURS 200 TIMES.
              03 distance PIC s9(8) VALUE 99999999.
              03 origpos PIC s9(5) VALUE 9999.
              03 neighbour PIC s9(5) VALUE 9999.
              03 connected PIC X(30) VALUE SPACES.
            01 closearr2.
             02 pidx2 OCCURS 200 TIMES.
              03 distance2 PIC s9(8) VALUE 99999999.
              03 origpos2 PIC s9(5) VALUE 9999.
              03 neighbour2 PIC s9(5) VALUE 9999.
              03 connected2 PIC X(30) VALUE SPACES.

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
            PERFORM VARYING idx2 FROM starti BY 1 UNTIL idx2 > numitems
             COMPUTE hd = (posi(idx,1) - posi(idx2,1)) ** 2
              + (posi(idx,2) - posi(idx2,2)) ** 2
              + (posi(idx,3) - posi(idx2,3)) ** 2
             MOVE hd TO distance(closest)
             MOVE idx TO origpos(closest)
             MOVE idx2 TO neighbour(closest)
             ADD 1 TO closest
            END-PERFORM
           END-PERFORM.
           MOVE 0 TO idx
           MOVE closearr TO closearr2.
           SORT pidx2 ON ASCENDING KEY distance2.
           PERFORM VARYING starti FROM 1 BY 1 UNTIL starti > pairs
            MOVE starti TO idx
            DISPLAY pidx2(idx)
            IF (origpos2(starti - 1) = neighbour2(starti)
            AND neighbour2(starti - 1) = origpos2(starti))
            THEN ADD 1 TO pairs ELSE
      *      IF (filled(origpos2(idx):1) = "X"
      *      AND filled(neighbour2(idx):1) = "X")
      *      THEN
      *       ADD 0 TO pairs
      *       DISPLAY "HERE"
      *      END-IF
              MOVE "X" TO connected2(origpos2(idx))
              (neighbour2(idx):1)
              MOVE "X" TO connected2(origpos2(idx))
              (origpos2(idx):1)
              MOVE "X" TO connected2(neighbour2(idx))
              (origpos2(idx):1)
              MOVE "X" TO connected2(neighbour2(idx))
              (neighbour2(idx):1)
      *       MOVE "X" TO filled(origpos2(idx):1)
      *       MOVE "X" TO filled(neighbour2(idx):1)
            END-IF
           END-PERFORM.
           PERFORM VARYING starti FROM 1 BY 1 UNTIL starti > numitems
            MOVE 0 TO resetter
            PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
             IF connected2(starti)(idx:1) = "X" THEN
              PERFORM VARYING idx2 FROM 1 BY 1 UNTIL idx2 > numitems
               IF connected2(idx)(idx2:1) = "X" THEN
                IF connected2(starti)(idx2:1) NOT= "X"
                THEN MOVE 1 TO resetter END-IF
                MOVE connected2(idx)(idx2:1)
                 TO connected2(starti)(idx2:1)
               END-IF
              END-PERFORM
             END-IF
            END-PERFORM
            IF resetter = 1 THEN SUBTRACT 1 FROM starti END-IF
           END-PERFORM.
           PERFORM VARYING starti FROM 1 BY 1 UNTIL starti > numitems
            MOVE 0 TO cnt
            MOVE 0 TO idx2
            DISPLAY "       " filled
            INSPECT connected2(starti) TALLYING cnt FOR ALL "X"
            DISPLAY starti ":" connected2(starti) ", " cnt
            PERFORM VARYING idx FROM 1 BY 1
             UNTIL idx > LENGTH OF connected2(starti)
             IF connected2(starti)(idx:1) = "X"
              IF filled(idx:1) = "X" THEN ADD 1 TO idx2
              ELSE MOVE connected2(starti)(idx:1) TO filled(idx:1)
              END-IF
             END-IF
            END-PERFORM
            IF idx2 < cnt THEN
             MOVE cnt TO tmp
             IF tmp > ans1 THEN
              MOVE ans1 TO tmp
             END-IF
             COMPUTE ans1 = FUNCTION MAX(cnt, ans1)
             IF tmp > ans2 THEN
              MOVE ans2 TO tmp2
             ELSE
              MOVE tmp TO tmp2
             END-IF
             COMPUTE ans2 = FUNCTION MAX(tmp, ans2)
             COMPUTE ans3 = FUNCTION MAX(tmp2, ans3)
            END-IF
           END-PERFORM.
           COMPUTE hd = ans1 * ans2 * ans3.
           DISPLAY hd " = " ans1 " * " ans2 " * " ans3.
           STOP RUN.
