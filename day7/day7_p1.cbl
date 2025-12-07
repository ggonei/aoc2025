       IDENTIFICATION DIVISION.
       PROGRAM-ID. day7-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day7/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(141).

           WORKING-STORAGE SECTION.
            01 eofile PIC 9(1) VALUE 0.
            01 beam PIC X(1) VALUE SPACE.
            01 beamidx PIC 9(3) VALUE 1.
            01 idx PIC 9(3) VALUE 1.
            01 maxlength PIC 9(3) VALUE 141.
            01 previnstruc PIC X(141).
            01 split PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eofile > 0
            READ inputfile
             AT END
              ADD 1 TO eofile
             NOT AT END
              IF beam NOT="S" THEN
               PERFORM VARYING idx FROM 1 BY 1 UNTIL beam = "S"
                IF instruction(idx:1) = "S" THEN
                 MOVE "S" TO beam
                 MOVE idx TO beamidx
                END-IF
               END-PERFORM
              ELSE
               PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > maxlength
                IF instruction(idx:1) = "^" THEN
                 IF previnstruc(idx:1) = "|" THEN
                  MOVE "|" TO instruction(idx - 1:1)
                  MOVE "|" TO instruction(idx + 1:1)
                  ADD 1 TO split
                 END-IF
                ELSE
                 IF previnstruc(idx:1) = "|" OR "S" THEN
                  MOVE "|" TO instruction(idx:1)
                 END-IF
                END-IF
               END-PERFORM
               DISPLAY instruction
              END-IF
              MOVE instruction TO previnstruc
           END-PERFORM.
           DISPLAY split.
           CLOSE inputfile.
           STOP RUN.
