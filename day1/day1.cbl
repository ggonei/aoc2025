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
             02 rawmagnitude PIC X(4).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(1) VALUE 0.
            01 counter PIC s9(9) VALUE 50.
            01 magnitude PIC 9(4) VALUE 0.
            01 rotation PIC s9(1) VALUE 0.

       PROCEDURE DIVISION.
           OPEN INPUT inputfile
           PERFORM UNTIL eof = 1
            READ inputfile
             AT END
              MOVE 1 TO eof
             NOT AT END
              IF direction = "R" THEN
               MOVE 1 TO rotation
              ELSE
               IF direction = "L" THEN
                MOVE -1 TO rotation
               END-IF
              END-IF
              MOVE rawmagnitude TO magnitude
              COMPUTE counter = counter + (rotation * magnitude)
              IF counter = 0 THEN DISPLAY counter
           END-PERFORM.
           CLOSE inputfile.
           STOP RUN.
