       IDENTIFICATION DIVISION.
       PROGRAM-ID. day2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT inputfile ASSIGN TO '/'-
           'Users/georgeoneill/ess-dmsc/aoc2025/day2/input'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
            FD inputfile.
            01 instruction PIC X(500).

           WORKING-STORAGE SECTION.
            01 eof PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
           OPEN INPUT inputfile.
           READ inputfile.
           CLOSE inputfile.
           DISPLAY instruction
           STOP RUN.
