 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* covers test cases for
* level 1 symbol
* record field with no dimension info and parent has none.
* symbol is array but no parent has dimension info.

  01  em PIC x(4)  VALUE "Emma".
  01  s2  NATIVE-2 VALUE 22.

* simple array
  01 rec.
     02 arr NATIVE-2 occurs 4 times.
     02 ema NATIVE-2.

* one record array
  01 rec2.
     02 ra2 occurs 2 times.
        03  em2 NATIVE-2.

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".

   Move 17 to s2.
   Move 1 to arr(1).
   Move 2 to arr(2).
   Move 3 to arr(3).
   Move 4 to arr(4).

   Display "End main".

 END PROGRAM main.
