       IDENTIFICATION DIVISION.
       PROGRAM-ID. day1-p2.

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
            01 clicks PIC 9(4) VALUE 0.
            01 counter PIC 9(7) VALUE 0.
            01 magnitude PIC s9(5) VALUE 0.
            01 posi PIC s9(4) VALUE 50.
            01 posinew PIC s9(4) VALUE 0.
            01 rotation PIC s9(1) VALUE 0.

       PROCEDURE DIVISION.
      *Day 1 summary:
      *Not being able to just DISPLAY changes the COMPUTE step isn't fun
      *Nor is not being able to assign things using =! Only equality
      *Reading files though is great, with FILE SECTION being clear

           OPEN INPUT inputfile.
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
              COMPUTE magnitude = rotation * magnitude
              DIVIDE 100 INTO magnitude GIVING clicks
               REMAINDER magnitude
              COMPUTE counter = counter + clicks
              COMPUTE posinew = posi + magnitude
              IF posinew = 0 OR
              (
               (FUNCTION SIGN(posinew) NOT= FUNCTION SIGN(posi))
               AND posi NOT= 0
              )
              THEN
               COMPUTE counter = counter + 1
              END-IF
              DIVIDE 100 INTO posinew GIVING clicks REMAINDER posi
              COMPUTE posi = FUNCTION MOD(posi, 100)
              COMPUTE counter = counter + clicks
           END-PERFORM.
           CLOSE inputfile.
           DISPLAY "Dial was on 0: " counter
           STOP RUN.
