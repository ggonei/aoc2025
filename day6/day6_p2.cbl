       IDENTIFICATION DIVISION.
       PROGRAM-ID. day6-p2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day6/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(5000).

           WORKING-STORAGE SECTION.
            01 eofile PIC 9(1) VALUE 0.
            01 ans PIC 9(18) VALUE 0.
            01 eoline PIC 9(1) VALUE 0.
            01 numitems PIC 9(4) VALUE 0.
            01 ptr PIC 9(5) VALUE 1.
            01 tmp PIC X(10) VALUE SPACES.
            01 myarray.
             02 inputs OCCURS 5 TIMES INDEXED BY linen.
              03 item PIC X(10) OCCURS 2000 TIMES INDEXED BY idx.

       PROCEDURE DIVISION.
           OPEN INPUT inputfile.
           MOVE 1 TO linen
           PERFORM UNTIL eofile > 0
            READ inputfile
             AT END
              ADD 1 TO eofile
             NOT AT END
              MOVE 0 TO eoline
              MOVE 1 TO ptr
              MOVE 1 TO idx
      *       DISPLAY instruction
              PERFORM UNTIL eoline > 1
               UNSTRING instruction DELIMITED BY ALL SPACES INTO
               tmp WITH POINTER ptr
               IF tmp = SPACES THEN
                ADD 1 TO eoline
               ELSE
                ADD 1 TO idx
                MOVE tmp TO item(linen, idx)
                MOVE SPACES to tmp
                MOVE 0 TO eoline
                MOVE idx TO numitems
               END-IF
              END-PERFORM
              ADD 1 TO linen
              IF linen = 6 THEN ADD 1 TO eofile END-IF
           END-PERFORM.
           MOVE 1 TO idx
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
            DISPLAY
             item(1, idx)
             item(5, idx)
             item(2, idx)
             item(5, idx)
             item(3, idx)
             item(5, idx)
             item(4, idx)
            IF item(5, idx) = "*" THEN
             COMPUTE ans = ans +
              FUNCTION NUMVAL(item(1, idx)) *
              FUNCTION NUMVAL(item(2, idx)) *
              FUNCTION NUMVAL(item(3, idx)) *
              FUNCTION NUMVAL(item(4, idx))
            ELSE
             IF item(5, idx) = "+" THEN
             COMPUTE ans = ans +
              FUNCTION NUMVAL(item(1, idx)) +
              FUNCTION NUMVAL(item(2, idx)) +
              FUNCTION NUMVAL(item(3, idx)) +
              FUNCTION NUMVAL(item(4, idx))
             ELSE DISPLAY idx "WTF"
             END-IF
            END-IF
           DISPLAY ans
           END-PERFORM.
           CLOSE inputfile.
           STOP RUN.
