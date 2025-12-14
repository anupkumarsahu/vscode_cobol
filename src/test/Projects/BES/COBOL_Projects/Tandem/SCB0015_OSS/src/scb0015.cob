 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  em PIC x  VALUE "E".

* signed short
  01  s2  NATIVE-2 VALUE 22.
  01  s4  NATIVE-4 VALUE 44.

* Index variables are automatically declared by the compiler
  01 rec.
     02 arr NATIVE-2 occurs 4 times indexed by cobolix.

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".

   Move 17 to s2.
   Move 1 to arr(1).
   Move 2 to arr of rec(2).
   Move 3 to arr(3).
   Move 4 to arr(4).

   Set cobolix to 3.
   Move 33 to arr(cobolix).

   ENTER TAL DEBUG.
   Move 18 to s2.

   Display "End main".

 END PROGRAM main.
