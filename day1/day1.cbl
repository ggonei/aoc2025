       IDENTIFICATION DIVISION.
       PROGRAM-ID. day1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day1/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction.
             02 direction PIC A(1).
             02 magnitude PIC 9(3).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
           OPEN INPUT inputfile
           PERFORM UNTIL eof = 1
            READ inputfile
             AT END
              MOVE 1 TO eof
             NOT AT END
              IF direction = "R" THEN
               DISPLAY "R"
              ELSE
               IF direction = "L" THEN
                DISPLAY "L"
               END-IF
              END-IF
              DISPLAY magnitude
           END-PERFORM.
           CLOSE inputfile.
           STOP RUN.
