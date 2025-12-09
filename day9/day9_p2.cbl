       IDENTIFICATION DIVISION.
       PROGRAM-ID. day9-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
             FILE-CONTROL.
               SELECT inputfile ASSIGN TO '/'-
               'Users/georgeoneill/ess-dmsc/aoc2025/day9/input'
                 ORGANIZATION IS LINE SEQUENTIAL.
               SELECT tmpfile ASSIGN TO tmpwork.
               SELECT sortedfile ASSIGN TO '/'-
               'Users/georgeoneill/ess-dmsc/aoc2025/day9/output'.

       DATA DIVISION.
           FILE SECTION.
             FD inputfile.
             01 instruction PIC X(12).
             FD sortedfile.
             01 instructions PIC X(12).
             SD tmpfile.
             01 instructiont PIC X(12).

           WORKING-STORAGE SECTION.
             01 eofile PIC 9(1) VALUE 0.
             01 bigside PIC 9(5) VALUE 0.
             01 bigx PIC 9(5) VALUE 0.
             01 bigy PIC 9(5) VALUE 0.
             01 posbx PIC s9(5) VALUE 0.
             01 posby PIC s9(5) VALUE 0.
             01 posf PIC s9(5) VALUE 0.
             01 cnt PIC 9(5) VALUE 0.
             01 idx PIC 9(5) VALUE 0.
             01 idx2 PIC 9(5) VALUE 0.
             01 rectbiggest PIC 9(18) VALUE 0.
             01 rect PIC 9(18) VALUE 0.
             01 positions.
               02 coord OCCURS 1000 TIMES.
                 03 posix PIC s9(5) VALUE 0.
                 03 posiy PIC s9(5) VALUE 0.

       PROCEDURE DIVISION.
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
      *    IF bigx > bigy THEN
      *      MOVE posbx TO posf
      *      MOVE FUNCTION ABS(posix(posbx) - posix(posbx + 1) +1)
      *      TO bigside
      *    ELSE IF bigy > bigx THEN
      *        MOVE posby TO posf
      *        MOVE FUNCTION ABS(posiy(posby) - posiy(posby + 1) +1)
      *        TO bigside
      *      ELSE
      *        DISPLAY "Looks like you have a square"
      *      END-IF
      *    END-IF
           DISPLAY "Testing with " posbx "," posby
             PERFORM VARYING idx FROM 1 BY 2 UNTIL idx > cnt
               COMPUTE rect =
                (FUNCTION ABS(posix(idx) - posix(posbx)) + 1) *
                (FUNCTION ABS(posiy(idx) - posiy(posbx)) + 1)
               IF rect > rectbiggest THEN
                 DISPLAY "THIS RECTANGLE IS " rect
                 DISPLAY
                 "(" posix(idx) "," posiy(idx) "),"
                 "(" posix(posbx) "," posiy(idx) "),"
                 "(" posix(posbx) "," posiy(posbx) "),"
                 "(" posix(idx) "," posiy(posbx) ")"
                 DISPLAY " " posix(idx + 1) "," posiy(idx + 1)
                 DISPLAY " " posix(idx - 1) "," posiy(idx - 1)
                 DISPLAY " " posix(posbx + 1) "," posiy(posbx + 1)
                 DISPLAY " " posix(posbx - 1) "," posiy(posbx - 1)
                 MOVE rect TO rectbiggest
               END-IF
      *        IF posix(idx) = posix(posbx)
      *        AND posix(idx + 1) = posix(posbx + 1)
      *        THEN DISPLAY "SuccessX: " idx
      *        IF posiy(idx) = posiy(posby)
      *        AND posiy(idx + 1) = posiy(posby + 1)
      *        THEN DISPLAY "SuccessY: " idx
           END-PERFORM.
           STOP RUN.
      *      COMPUTE rect =
      *       (FUNCTION ABS(posix(idx) - posix(idx2)) + 1) *
      *       (FUNCTION ABS(posiy(idx) - posiy(idx2)) + 1)
      *      IF rect > rectbiggest THEN
      *        DISPLAY "THIS RECTANGLE IS " rect
      *        DISPLAY
      *        "(" posix(idx) "," posiy(idx) "),"
      *        "(" posix(idx2) "," posiy(idx) "),"
      *        "(" posix(idx2) "," posiy(idx2) "),"
      *        "(" posix(idx) "," posiy(idx2) ")"
      *        IF posix(idx) > posix(idx2) THEN
      *          IF posiy(idx) > posiy(idx2) THEN

      *          ELSE

      *          END-IF
      *        ELSE
      *        END-IF
      *        DISPLAY " " posix(idx + 1) "," posiy(idx + 1)
      *        DISPLAY " " posix(idx - 1) "," posiy(idx - 1)
      *        DISPLAY " " posix(idx2 + 1) "," posiy(idx2 + 1)
      *        DISPLAY " " posix(idx2 - 1) "," posiy(idx2 - 1)
      *        MOVE rect TO rectbiggest
      *      END-IF
