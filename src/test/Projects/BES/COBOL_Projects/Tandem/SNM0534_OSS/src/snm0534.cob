*Test cobol/nld file access when mulitple objects are linked together.
*This file is compiled first and then file SNM0530.
?SYMBOLS
 IDENTIFICATION DIVISION.
 PROGRAM-ID. FirstCompilation.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 rec.
    02  filename  PIC X(8)  VALUE "C053COB4".
    02  proc      PIC X(10) VALUE "FirstComp*".
    02  statement PIC X(5)  VALUE "EmmaA".
 LINKAGE SECTION.
 PROCEDURE DIVISION.
 main SECTION.
 FirstCompilation-start.
   Move "!One!" to statement of rec.
?source SNM0531.cob(nest2)
   Move "!One!" to statement of rec.
   Call FirstComp2.
 END PROGRAM FirstCompilation.

?SOURCE SNM0532.cob(FirstComp2)
