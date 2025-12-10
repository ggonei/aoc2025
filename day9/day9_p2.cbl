       IDENTIFICATION DIVISION.
       PROGRAM-ID. day9-p2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
             FILE-CONTROL.
               SELECT inputfile ASSIGN TO '/'-
               'Users/georgeoneill/ess-dmsc/aoc2025/day9/input'
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
             FD inputfile.
             01 instruction PIC X(12).

           WORKING-STORAGE SECTION.
             01 eofile PIC 9(1) VALUE 0.
             01 bigx PIC s9(6) VALUE 0.
             01 bigy PIC s9(6) VALUE 0.
             01 limx PIC s9(6) VALUE 0.
             01 limy PIC s9(6) VALUE 0.
             01 posbx PIC s9(6) VALUE 0.
             01 posby PIC s9(6) VALUE 0.
             01 cnt PIC s9(6) VALUE 0.
             01 idx PIC s9(6) VALUE 0.
             01 rect PIC s9(18) VALUE 0.
             01 positions.
               02 coord OCCURS 1001 TIMES.
                 03 posix PIC s9(6) VALUE 0.
                 03 posiy PIC s9(6) VALUE 0.

       PROCEDURE DIVISION.
      *Day 9 summary:
      *Not that bad! Spent a lot of time on the algorithm today
      *Anything better than yesterday...
      *A little surprised this result worked as I expected to need to
      *work out from 3 choices.  In adding lines for the complex poly,
      *I seem to have broken the test file pattern matching, as there is
      *no longer a 'best of' loop.
      *Algorithm reasoning:
      *Since the line is stated to be continuous it's not got holes in.
      *So only need to  worry about edge conditions.
      *The differential of the curve is not 'continuous' and at any area
      * you, and it can be seen in the data that it, can go any
      * combination of directions.
      *This implies there must be maximal areas that are bounded within.
      *If you parse through the entire file on first pass you get the
      *longest continuous edge simply by computing the difference of
      * n, n+1
      *since the red squares are all on the corners (none lie on lines).
      *If this point is more than half the biggest space available
      *(ideally the map space, so 99999x999 here, or else the biggest
      *distances between any two points) you have the biggest side of
      *the biggest area.
      *Once you have those two points you are bound with a side, and can
      *'draw' the limit line parallel to that which bounds the rectangle
      *and that limit line is assisted given that the other side of it
      *has a red tile on it.

           OPEN INPUT inputfile.
           PERFORM UNTIL eofile > 0
             READ inputfile
             AT END
               ADD 1 TO eofile
             NOT AT END
               ADD 1 TO cnt
               UNSTRING instruction DELIMITED BY "," INTO
                 posix(cnt)
                 posiy(cnt)
           END-PERFORM.
           CLOSE inputfile.
           PERFORM VARYING idx FROM 2 BY 1 UNTIL idx > cnt
             IF FUNCTION ABS(posix(idx - 1) - posix(idx)) > bigx THEN
               MOVE FUNCTION ABS(posix(idx - 1) - posix(idx)) TO bigx
               COMPUTE posbx = idx - 1
             END-IF
             IF FUNCTION ABS(posiy(idx - 1) - posiy(idx)) > bigy THEN
               MOVE FUNCTION ABS(posiy(idx - 1) - posiy(idx)) TO bigy
               COMPUTE posby = idx - 1
             END-IF
           END-PERFORM.
           DISPLAY "Testing " posbx "(" bigx ")," posby "(" bigy ")"
      *    Get the vertical position in the region where we cannot go
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > cnt
             IF posix(posbx) > posix(idx) THEN
               COMPUTE limx = idx - 1
               IF limx = 0 THEN MOVE 1 TO limx END-IF
               MOVE cnt TO idx
             END-IF
           END-PERFORM.
      *    Get the horizontal position in the region where we cannot go
           PERFORM VARYING idx FROM limx BY 1 UNTIL idx > cnt
             IF posiy(idx) < posiy(limx) THEN
               COMPUTE limy = idx
      *    In case there is a more horizontal position at that height
               PERFORM UNTIL posiy(idx) NOT= posiy(limy)
                 ADD 1 TO limy
               END-PERFORM
      *    And bump it back again
               SUBTRACT 1 FROM limy
               MOVE cnt TO idx
             END-IF
           END-PERFORM.
           IF limy = 0 THEN MOVE 1 TO limy END-IF.
           DISPLAY limx ":" posix(posby) "," posiy(posby).
           DISPLAY limy ":" posix(limy) "," posiy(limy).
           COMPUTE rect =
            (FUNCTION ABS(posix(posbx) - posix(limy)) + 1) *
            (FUNCTION ABS(posiy(posby) - posiy(limy)) + 1).
           DISPLAY "Limited rectangle:" rect.
           STOP RUN.
