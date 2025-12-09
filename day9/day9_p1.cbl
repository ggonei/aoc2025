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
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > cnt
             PERFORM VARYING idx2 FROM 1 BY 1 UNTIL idx2 > cnt
               DISPLAY idx "," idx2
               COMPUTE rect =
                (FUNCTION ABS(posix(idx) - posix(idx2)) + 1) *
                (FUNCTION ABS(posiy(idx) - posiy(idx2)) + 1)
               IF rect > rectbiggest THEN
                 DISPLAY "THIS RECTANGLE IS " rect
                 MOVE rect TO rectbiggest
               END-IF
             END-PERFORM
           END-PERFORM.
           STOP RUN.
