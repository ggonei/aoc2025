       IDENTIFICATION DIVISION.
       PROGRAM-ID. helper.

       DATA DIVISION.
           LINKAGE SECTION.
           01 digit PIC 9(1).
           01 digits PIC 9(1).
           01 inputn PIC 9(4).
           01 min PIC 9(1).
           01 max PIC 9(1).
           01 pmin PIC 9(1).
           01 pmax PIC 9(1).
           01 result PIC 9(8).

       PROCEDURE DIVISION USING digit inputn pmin pmax min max result.
           COMPUTE digits = FUNCTION INTEGER(
            FUNCTION LOG10(
             FUNCTION NUMVAL(inputn)
            )) + 1
            IF pmin = min THEN
             result = inputn(digit:1)
            END-IF







           COMPUTE divider2 = FUNCTION INTEGER(
            FUNCTION LOG10(
             FUNCTION NUMVAL(input2)
            )) + 1
           COMPUTE divider3 = FUNCTION INTEGER(
            FUNCTION LOG10(
             FUNCTION NUMVAL(input3)
            )) + 1
           COMPUTE divider4 = FUNCTION INTEGER(
            FUNCTION LOG10(
             FUNCTION NUMVAL(input4)
            )) + 1
           IF divider4 > 0 THEN COMPUTE m4 = 10      END-IF.
           IF input3 > 0 THEN COMPUTE m3 = 10 * m3 END-IF.
           IF input2 > 0 THEN
            IF m3 > 1 THEN
             COMPUTE m2 = 10 * m3 ELSE
             COMPUTE m2 = 10 * m4
            END-IF
           END-IF.
           IF input1 > 0 THEN
            IF m2 > 1 THEN
             COMPUTE m1 = 10 * m2 ELSE
             IF m3 > 1 THEN
              COMPUTE m1 = 10 * m3 ELSE
              COMPUTE m1 = 10 * m4 ELSE
             END-IF
            END-IF
           END-IF.
           COMPUTE result =
           FUNCTION MOD(input1, 10 * modo) / modo * m1 +
           FUNCTION MOD(input2, 10 * modo) / modo * m2 +
           FUNCTION MOD(input3, 10 * modo) / modo * m3 +
           FUNCTION MOD(input4, 10 * modo) / modo * m4.
           GOBACK.