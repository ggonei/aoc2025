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
            01 hstr PIC X(1) VALUE SPACE.
            01 hval PIC 9(3) VALUE 1.
            01 idx PIC 9(3) VALUE 1.
            01 maxlength PIC 9(3) VALUE 141.
            01 previnstruc PIC X(141) VALUE SPACES.
            01 split PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
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
                 END-IF
               END-IF
               END-PERFORM
              ELSE
               PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > maxlength
                IF instruction(idx:1) = "^" THEN
                 IF previnstruc(idx:1) NOT= "0" THEN
                 COMPUTE hval = FUNCTION ORD(
                   FUNCTION NUMVAL(previnstruc(idx:1))
                   + FUNCTION NUMVAL(instruction(idx - 1:1))
                  )
                  MOVE FUNCTION CHAR(hval + 48) TO hstr
                  MOVE hstr TO instruction(idx - 1:1)
                 COMPUTE hval = FUNCTION ORD(
                   FUNCTION NUMVAL(previnstruc(idx:1))
                   + FUNCTION NUMVAL(previnstruc(idx + 1:1))
                   + FUNCTION NUMVAL(instruction(idx + 1:1))
                  )
                  MOVE FUNCTION CHAR(hval + 48) TO hstr
                  MOVE hstr TO instruction(idx + 1:1)
                 END-IF
                ELSE
                 IF previnstruc(idx:1) = "S" THEN
                  MOVE "1" TO instruction(idx:1)
                 ELSE
                  IF instruction(idx:1) = "."
                   IF previnstruc(idx:1) = "^" THEN
                    MOVE 0 TO instruction(idx:1)
                   ELSE
                    MOVE previnstruc(idx:1) TO instruction(idx:1)
                   END-IF
                  END-IF
                 END-IF
                END-IF
               END-PERFORM
              END-IF
              MOVE instruction TO previnstruc
              DISPLAY previnstruc
           END-PERFORM.
           DISPLAY "Ready?"
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > maxlength
            IF instruction(idx:1) NOT= "0" THEN
             DISPLAY FUNCTION ORD(instruction(idx:1))
             COMPUTE split = split
              + FUNCTION ORD(instruction(idx:1))
              - 49
            END-IF
           END-PERFORM.
           DISPLAY split.
           CLOSE inputfile.
           STOP RUN.
