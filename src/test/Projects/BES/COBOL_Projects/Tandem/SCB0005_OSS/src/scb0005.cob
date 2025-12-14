 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01  em PIC x(4)  VALUE "Emma".
  01  s2  NATIVE-2 VALUE 22.

* f211 gets dim info of rec21.
* rec 212 gets the dim info of rec21 first and then its own.
* simple array within a record array
  01 rec2.
     02 rec21 occurs 5 times.
        03 f211 NATIVE-2 value 17.
        03 a212 NATIVE-2 occurs 6 times.

* one record array within a record array
  01 rec3.
     02 rec31 occurs 4 times.
        03 f311 NATIVE-2 value 22.
        03 ra312 NATIVE-2 occurs 3 times.
           04 f3121 NATIVE-2.

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".

   Move 17 to s2.

   Display "End main".

 END PROGRAM main.
