       IDENTIFICATION DIVISION.
       PROGRAM-ID. day8-p2.

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
      *Day 8 summary:
      *Really hard and frustrating. Storing the circuit in COBOL was not
      *trivial but I eventually managed it using a loop and differing
      *circuits.  In the end I needed a loop over everything to get it
      *to close successfully and provide some nice answers.
      *The second part was super easy, and just meant removing a lot of
      *the loops for the circuits.
      *I also got tripped up by trying and failing to compare signed int
      *and just time limitations (had a nice circuit implementation but
      *it took way too long for large N).
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
           PERFORM VARYING starti FROM 1 BY 1 UNTIL cnt = numitems
           DISPLAY pidx(starti)
            MOVE 0 TO cnt
            MOVE "X" TO tmpstring(origpos(starti):1)
            MOVE "X" TO tmpstring(neighbour(starti):1)
            DISPLAY FUNCTION TRIM(tmpstring, TRAILING)
            INSPECT tmpstring TALLYING cnt FOR LEADING "X"
            IF cnt = numitems THEN
             COMPUTE hd = posi(origpos(starti), 1)
              * posi(neighbour(starti), 1)
            END-IF
           END-PERFORM.
           DISPLAY hd.
           STOP RUN.
