?section nest2
   Move "*Two*" to statement of rec.
?source SNM0532.cob(nest3)
   Move "*Two*" to statement of rec.
?section pdr2
 IDENTIFICATION DIVISION.
 PROGRAM-ID. pdr2.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 rec.
    02  filename  PIC X(8)  VALUE "C053COB1".
    02  proc      PIC X(10) VALUE "Pdr2Sheryl".
    02  statement PIC X(5)  VALUE "*Crow".
 PROCEDURE DIVISION.
 pdr2-start.
   Move "*One*" to statement of rec.
?source SNM0532.cob(nest3)
   Display "pdr2-end".
 END PROGRAM pdr2.

