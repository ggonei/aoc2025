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
            02 magnitude PIC 9(2).

       PROCEDURE DIVISION.
           OPEN INPUT inputfile
           READ inputfile INTO instruction
           CLOSE inputfile
           DISPLAY instruction.
           STOP RUN.
