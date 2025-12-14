 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 77  dummy  NATIVE-2 VALUE  17.

  77  char  PIC XX  VALUE  "EM".
      88 cha        VALUE  "A".

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".
   Move "A" to char.
   Move 22 to dummy.
   Display "End main".

 END PROGRAM main.

