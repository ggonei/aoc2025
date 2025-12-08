       IDENTIFICATION DIVISION.
       PROGRAM-ID. day7-p2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day7/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(142).

           WORKING-STORAGE SECTION.
            01 eofile PIC 9(1) VALUE 0.
            01 beam PIC X(1) VALUE SPACE.
            01 idx PIC 9(3) VALUE 1.
            01 maxlength PIC 9(3) VALUE 141.
            01 previnstruc PIC X(141) VALUE SPACES.
            01 split PIC 9(18) VALUE 0.
            01 myarray.
             02 chstr PIC Z(18) VALUE 0 OCCURS 141 TIMES INDEXED BY po.

       PROCEDURE DIVISION.
      *Day 7 summary:
      *Tried hard to avoid an array here but the numbers just got big
      *Clever use of CHAR and ORD let me represent 0-200 in 1 char space
      *But the end result is very big!  So had to switch to array usage.
      *In the end much nicer but without the visual result.
      *Used integers dropping the leading zero this time to help debug.
           OPEN INPUT inputfile.
           PERFORM UNTIL eofile > 0
            READ inputfile
             AT END
              ADD 1 TO eofile
             NOT AT END
              IF beam NOT="S" THEN
               PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > maxlength
                IF instruction(idx:1) = "." THEN
                 MOVE "0" TO instruction(idx:1)
                ELSE
                 IF instruction(idx:1) = "S" THEN
                  MOVE "S" TO beam
                  MOVE "1" TO chstr(idx)
                 END-IF
                END-IF
               END-PERFORM
              ELSE
               PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > maxlength
                IF instruction(idx:1) = "^" THEN
                 COMPUTE chstr(idx - 1) =
                   FUNCTION NUMVAL(chstr(idx))
                   + FUNCTION NUMVAL(chstr(idx - 1))
                 COMPUTE chstr(idx + 1) =
                   FUNCTION NUMVAL(chstr(idx))
                   + FUNCTION NUMVAL(chstr(idx + 1))
                  MOVE 0 TO chstr(idx)
                ELSE
                 IF instruction(idx:1) = "S" THEN
                  MOVE "1" TO chstr(idx)
                 END-IF
                END-IF
               END-PERFORM
              END-IF
           END-PERFORM.
           CLOSE inputfile.
           DISPLAY myarray.
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > maxlength
            COMPUTE split = split + FUNCTION NUMVAL(chstr(idx))
           END-PERFORM.
           DISPLAY split.
           STOP RUN.
