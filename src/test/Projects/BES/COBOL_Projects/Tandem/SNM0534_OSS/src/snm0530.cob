*Test cobol/nld file access when mulitple objects are linked together.
*SNM0534 is compiled first and then this file.
?SYMBOLS
?MAIN main
?SEARCH XNM05310
 IDENTIFICATION DIVISION.
 PROGRAM-ID. MAIN.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 PROCEDURE DIVISION.
 main-start.
   Call Nest4.
 END PROGRAM MAIN.

 IDENTIFICATION DIVISION.
 PROGRAM-ID. Nest4.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 rec.
    02  filename  PIC X(8)  VALUE "C053COB0".
    02  proc      PIC X(10) VALUE "***Nest4**".
    02  statement PIC X(5)  VALUE "*Zero".
 PROCEDURE DIVISION.
 Nest4-start.
   Move "*One*" to statement of rec.
?source SNM0531.cob(nest2)
   Move "*One*" to statement of rec.
   Call pdr.
 END PROGRAM Nest4.

?SOURCE SNM0531.cob(pdr2)
 IDENTIFICATION DIVISION.
 PROGRAM-ID. pdr.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 rec.
    02  filename  PIC X(8)  VALUE "C053COB0".
    02  proc      PIC X(10) VALUE "***Pdr****".
    02  statement PIC X(5)  VALUE "*None".
 PROCEDURE DIVISION.
 pdr-start.
   Move "*One*" to statement of rec.
?source SNM0531.cob(nest2)
   Call pdr2.
   Call FirstCompilation.
   Display "pdr-end".
 END PROGRAM pdr.


