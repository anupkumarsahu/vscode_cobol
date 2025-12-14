 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  rec.
     05  scale-18           PIC V9(18)      VALUE  .123456789012345678.
     05  scale-4            PIC 9(14)V9(4)  VALUE  12345678901234.1234.
     05  scale-2            PIC 9(16)V99    VALUE  1234567890123456.12.
     05  scale-1            PIC 9(17)V9     VALUE  12345678901234567.1.
     05  scale-0            PIC 9(5)        VALUE  12345.
     05  scale-neg-1        PIC 9(7)P       VALUE  12345670.
     05  scale-neg-2        PIC 9(7)PP      VALUE  123456700.
     05  scale-neg-4        PIC 9(7)P(4)    VALUE  12345670000.
     05  scale-neg-17       PIC 9P(17)      VALUE  100000000000000000.

 01  rec-2.
     05  edited-7           PIC 999,999.
     05  edited-8           PIC XXX,XXX.

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".
   Display "End main".

 END PROGRAM main.
