 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01 s2  NATIVE-2 VALUE 22.
  01 tov  native-2 value 36.

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".
   Move 12 to s2.
   Move 45 to tov.
   Display tov.
   Display "End main".

 END PROGRAM main.
