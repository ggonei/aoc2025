       IDENTIFICATION DIVISION.
       PROGRAM-ID. day4-p2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day4/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction.
             02 gridx PIC X(140).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(11) VALUE 1.
            01 gridy.
             02 strip OCCURS 140 TIMES INDEXED BY posix.
              03 posy PIC X(140).
            01 posiy PIC s9(9).
            01 poscnt PIC 9(1) VALUE 0.
            01 domx PIC X(1) VALUE "Y".
            01 dopx PIC X(1) VALUE "Y".
            01 domy PIC X(1) VALUE "Y".
            01 dopy PIC X(1) VALUE "Y".
            01 cnt PIC 9(5) VALUE 1.
            01 totcnt PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
      *Day 4 summary:
      *This felt very easy, could be getting the hang of it...
      *Made a mistake of 100x100 not 140x140 which took some time
      *No string manipulation (only character) helped this a lot
      *I like the COBOL loop syntax
      *Could be vastly improved but good to have an easier win...
      *First exercise correct first time (with correct input sizes...)
           OPEN INPUT inputfile.
           PERFORM UNTIL eof > 140
            READ inputfile
             AT END
              ADD 1 TO eof
             NOT AT END
              MOVE gridx TO posy(eof)
              ADD 1 TO eof
              DISPLAY gridx
           END-PERFORM.
           CLOSE inputfile.

           PERFORM UNTIL cnt = 0
           MOVE 1 TO cnt
           PERFORM VARYING posix FROM 1 BY 1 UNTIL posix > 140
            PERFORM VARYING posiy FROM 1 BY 1 UNTIL posiy > 140
             IF posy(posix)(posiy:1) = "/" THEN
              MOVE "." TO posy(posix)(posiy:1)
             END-IF
            END-PERFORM
           END-PERFORM
           PERFORM VARYING posix FROM 1 BY 1 UNTIL posix > 140
            PERFORM VARYING posiy FROM 1 BY 1 UNTIL posiy > 140
             MOVE 0 TO poscnt
             IF posy(posix)(posiy:1) = "@" THEN
              IF posix > 1 THEN MOVE "Y" TO domx ELSE MOVE "N" TO domx
              END-IF
              IF posix < 140 THEN MOVE "Y" TO dopx ELSE MOVE "N" TO dopx
              END-IF
              IF posiy > 1 THEN
               IF domx = "Y"
                AND (
                 posy(posix - 1)(posiy - 1:1) = "/" OR
                 posy(posix - 1)(posiy - 1:1) = "@"
                ) THEN
                ADD 1 TO poscnt
               END-IF
               IF (
                 posy(posix)(posiy - 1:1) = "/" OR
                 posy(posix)(posiy - 1:1) = "@"
                ) THEN
                ADD 1 TO poscnt
               END-IF
               IF dopx = "Y"
                AND (
                 posy(posix + 1)(posiy - 1:1) = "/" OR
                 posy(posix + 1)(posiy - 1:1) = "@"
                ) THEN
                ADD 1 TO poscnt
               END-IF
              END-IF
              IF posiy < 140 THEN
               IF domx = "Y"
                AND (
                 posy(posix - 1)(posiy + 1:1) = "/" OR
                 posy(posix - 1)(posiy + 1:1) = "@"
                ) THEN
                ADD 1 TO poscnt
               END-IF
               IF (
                 posy(posix)(posiy + 1:1) = "/" OR
                 posy(posix)(posiy + 1:1) = "@"
                ) THEN
                ADD 1 TO poscnt
               END-IF
               IF dopx = "Y"
                AND (
                 posy(posix + 1)(posiy + 1:1) = "/" OR
                 posy(posix + 1)(posiy + 1:1) = "@"
                ) THEN
                ADD 1 TO poscnt
               END-IF
              END-IF
              IF domx = "Y"
                AND (
                 posy(posix - 1)(posiy:1) = "/" OR
                 posy(posix - 1)(posiy:1) = "@"
                ) THEN
               ADD 1 TO poscnt
              END-IF
              IF dopx = "Y"
                AND (
                 posy(posix + 1)(posiy:1) = "/" OR
                 posy(posix + 1)(posiy:1) = "@"
                ) THEN
               ADD 1 TO poscnt
              END-IF
              IF poscnt < 4 THEN
               MOVE "/" TO posy(posix)(posiy:1)
               ADD 1 TO cnt
              END-IF
             END-IF
            END-PERFORM
           END-PERFORM
           ADD cnt TO totcnt
           SUBTRACT 1 FROM totcnt
           DISPLAY totcnt
           IF cnt = 1 THEN MOVE 0 TO cnt END-IF
           END-PERFORM.
           DISPLAY totcnt.

           STOP RUN.
