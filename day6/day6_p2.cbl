       IDENTIFICATION DIVISION.
       PROGRAM-ID. day6-p1.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day6/inputtst'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(5000).

           WORKING-STORAGE SECTION.
            01 ans PIC 9(18) VALUE 0.
            01 eofile PIC 9(1) VALUE 0.
            01 eoline PIC 9(1) VALUE 0.
            01 len PIC 9(4) VALUE 0.
            01 nlines PIC 9(1) VALUE 4.
            01 numitems PIC 9(4) VALUE 0.
            01 ptr PIC 9(5) VALUE 1.
            01 tmp PIC X(10) VALUE SPACES.
            01 myarray.
             02 inputs OCCURS 5 TIMES INDEXED BY linen.
              03 item PIC X(10) OCCURS 2000 TIMES INDEXED BY idx.
           01 myptrarray.
             02 ptrinputs OCCURS 5 TIMES INDEXED BY lineptr.
              03 ptritem PIC 9(5) OCCURS 2000 TIMES INDEXED BY ptridx.

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
              PERFORM UNTIL eoline > 1
               UNSTRING instruction DELIMITED BY ALL SPACES INTO
               tmp WITH POINTER ptr
               IF tmp = SPACES THEN
                ADD 1 TO eoline
               ELSE
                MOVE tmp TO item(linen, idx)
                MOVE ptr TO ptritem(linen, idx + 1)
                MOVE SPACES to tmp
                MOVE 0 TO eoline
                ADD 1 TO idx
                MOVE idx TO numitems
               END-IF
              END-PERFORM
              ADD 1 TO linen
              IF linen = nlines + 1 THEN ADD 1 TO eofile END-IF
           END-PERFORM.
           PERFORM VARYING linen FROM 1 BY 1 UNTIL linen > nlines
            MOVE 1 TO ptritem(linen, 1)
           END-PERFORM
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > numitems
            IF item(nlines, idx) = "*" THEN
             DISPLAY idx "* @ " ptritem(nlines, idx)
            ELSE
             IF item(nlines, idx) = "+" THEN
              DISPLAY idx "+ @ " ptritem(nlines, idx)
             ELSE DISPLAY idx "+" item(nlines, idx)
              "@" ptritem(nlines, idx)
             END-IF
            END-IF
           END-PERFORM.
           CLOSE inputfile.
           STOP RUN.
