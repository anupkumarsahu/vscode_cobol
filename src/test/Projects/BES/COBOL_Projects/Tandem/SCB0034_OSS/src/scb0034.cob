* Test same name in different records
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

* signed short
  01  mi  NATIVE-2 VALUE 22.

  01  rec.
*      02 sti NATIVE-2 VALUE 23.
      02 stcom NATIVE-2 VALUE 24.

  01  rec2 global.
*      02 stj NATIVE-2 VALUE 25.
      02 stcom NATIVE-2 VALUE 26.

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 rec-start.

   Display "Start rec".

   Move 17 to mi.

*   Move 123 to sti.
   Move 124 to stcom of rec.
*   Move 125 to stj.
   Move 126 to stcom of rec2.

   Display "End main".

 END PROGRAM main.

