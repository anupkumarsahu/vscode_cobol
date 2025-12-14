?section nest3
   Move "Three" to statement of rec.
?source SNM0533.cob(nest4)
   Move "Three" to statement of rec.
?section FirstComp2
 IDENTIFICATION DIVISION.
 PROGRAM-ID. FirstComp2.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 rec.
    02  filename  PIC X(8)  VALUE "C053COB2".
    02  proc      PIC X(10) VALUE "FirstComp2".
    02  statement PIC X(6)  VALUE "SickOf".
 LINKAGE SECTION.
 PROCEDURE DIVISION.
 FirstComp2-start.
   Move "Tsting" to statement.
?source SNM0532.cob(nest3)
   Display "FirstComp1-end".
 END PROGRAM FirstComp2.
