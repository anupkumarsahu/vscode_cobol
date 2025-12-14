 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

* signed short
  01  mi  NATIVE-2 VALUE 22.

* use REDEFINE
  01  redef.
      02 name PIC A(20).
      02 names REDEFINES name.
         03 first-name PIC A(10).
         03 last-name  PIC A(10).
      66 fn RENAMES first-name.
      66 ln RENAMES last-name.

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 rec-start.

   Display "Start main".

   Move "Reva      " to first-name.
   Move "Cuthbertso" to last-name.

   Display "End main".

 END PROGRAM main.

