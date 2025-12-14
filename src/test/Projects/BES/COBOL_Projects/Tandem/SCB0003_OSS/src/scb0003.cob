 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01  em PIC x(4)  VALUE "Emma".

* signed short
  01  s2  NATIVE-2 VALUE 22.


* arrays must be inside a record.
* Also, the first record level cannot have bounds.
* In other words, all arrays must be a substruct or an element of a
* struct or substruct.
* these are OK...
* 77 arrx  NATIVE-2.
* 01 arrxy NATIVE-2.
* These give compilation errors...
* 77 arry NATIVE-2 occurs 4 times.
* 01 arry NATIVE-2 occurs 4 times.

  01 rec EXTERNAL.
     02 arr NATIVE-2 occurs 4 times.

  01 recx.
     02 arrx NATIVE-2 occurs 4 times.
        88 nata       VALUE  1.
        88 natb       VALUE  2.

  01 upper-bound NATIVE-2 VALUE 7.

  01 rec-interesting.
     02 arr-interesting NATIVE-2 occurs 5 to 10 times depending on upper-bound.

* f211 gets dim info of rec21.
* rec 212 gets the dim info of rec21 first and then its own.
  01 rec2.
     02 rec21 occurs 5 times.
        03 f211 NATIVE-2 value 17.
        03 a212 NATIVE-2 occurs 6 times.

* f211 gets dim info of rec21.
  01 rec3.
     02 rec31 occurs 2 times.
        03 f311 NATIVE-2 value 22.
        03 rec312.
           04 f3121 NATIVE-2 value 23.
           04 a3122 NATIVE-2 occurs 3 times.

* max nesting is 7 dimensions
  01 rec7.
     02 r71 OCCURS 2.
        03 r72 OCCURS 3.
           04 r73 OCCURS 2.
              05 r74 OCCURS 4.
                 06 r75 OCCURS 3.
                    07 r76 OCCURS 2.
                       08 r77 OCCURS 3.
                          09 emma PIC X(2).

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
