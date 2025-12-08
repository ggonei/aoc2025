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
            01 closest PIC s9(5) VALUE 0.
            01 cnt PIC 9(4) VALUE 0.
            01 hd PIC s9(16) VALUE 0.
            01 hdsmallest PIC s9(16) VALUE 0.
            01 idx PIC s9(5) VALUE 0.
            01 idx2 PIC s9(5) VALUE 0.
            01 starti PIC s9(5) VALUE 0.
            01 numitems PIC s9(5) VALUE 0.
            01 positions.
             02 place OCCURS 30 TIMES.
              03 posi PIC 9(5) OCCURS 3 TIMES.
            01 closearr.
             02 pidx OCCURS 30 TIMES.
              03 origpos PIC s9(5) VALUE 9999.
              03 distance PIC 9(18) VALUE 9999999999999999.
              03 neighbour PIC s9(5) VALUE 9999.
              03 connected PIC X(30) VALUE SPACE.
            01 closearr2.
             02 pidx2 OCCURS 30 TIMES.
              03 origpos2 PIC s9(5) VALUE 9999.
              03 distance2 PIC 9(18) VALUE 9999999999999999.
              03 neighbour2 PIC s9(5) VALUE 9999.
              03 connected2 PIC X(30) VALUE SPACE.

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
            MOVE 0 TO hdsmallest
            PERFORM VARYING idx2 FROM 1 BY 1 UNTIL idx2 > numitems
             COMPUTE hd = (posi(idx,1) - posi(idx2,1)) ** 2
              + (posi(idx,2) - posi(idx2,2)) ** 2
              + (posi(idx,3) - posi(idx2,3)) ** 2
             IF hd NOT= 0 THEN
              IF hdsmallest = 0 THEN
               MOVE hd TO hdsmallest
               MOVE idx2 TO closest
              ELSE
               COMPUTE hdsmallest = FUNCTION MIN(hd, hdsmallest)
               IF hd = hdsmallest THEN MOVE idx2 TO closest END-IF
              END-IF
             END-IF
            END-PERFORM
            DISPLAY idx ":" closest "; " hdsmallest
            MOVE hdsmallest TO distance(idx)
            MOVE closest TO neighbour(idx)
           END-PERFORM.
           MOVE 0 TO idx
           MOVE closearr TO closearr2.
           SORT pidx2 ON ASCENDING KEY distance2.
           PERFORM VARYING starti FROM 1 BY 1 UNTIL starti > numitems
            MOVE starti TO idx
            PERFORM UNTIL connected(idx)(neighbour(idx):1) = "X"
             MOVE "X" TO connected(idx)(neighbour(idx):1)
             MOVE "X" TO connected(neighbour(idx))(idx:1)
             MOVE neighbour(idx) TO idx
            END-PERFORM
           END-PERFORM.
           PERFORM VARYING starti FROM 1 BY 1 UNTIL starti > numitems
            MOVE 0 TO resetter
            PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
             IF connected(starti)(idx:1) = "X" THEN
              PERFORM VARYING idx2 FROM 1 BY 1 UNTIL idx2 > numitems
               IF connected(idx)(idx2:1) = "X" THEN
                IF connected(starti)(idx2:1) NOT= "X"
                THEN MOVE 1 TO resetter END-IF
                MOVE connected(idx)(idx2:1) TO connected(starti)(idx2:1)
               END-IF
              END-PERFORM
             END-IF
            END-PERFORM
            IF resetter = 1 THEN SUBTRACT 1 FROM starti END-IF
           END-PERFORM.
           PERFORM VARYING starti FROM 1 BY 1 UNTIL starti > numitems
            MOVE 0 TO cnt
            INSPECT connected(starti) TALLYING cnt FOR ALL "X"
            DISPLAY starti ":" connected(starti) ", " cnt
           END-PERFORM.
      *    DISPLAY closearr.
           STOP RUN.
