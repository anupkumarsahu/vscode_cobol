 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  em PIC x(4)  VALUE "Emma".

* signed short
  01  s  NATIVE-2 VALUE 3.
  01  s2 NATIVE-2 VALUE 6.

* Array must be inside of records
* Simplest depending on with upper bound outside of the record
  01 rec.
     02 arr NATIVE-2 occurs 1 to 4 times depending on s.

* Array must be inside of records
* Simplest depending on with upper bound inside of the record
  01 rc.
     02 rcs NATIVE-2 VALUE 2.
     02 rca NATIVE-2 occurs 1 to 3 times depending on rcs.

  01 rec2.
*    strange that cobol does not emit an error when a struct is given a type
*    this is a feature.  it indicates that the group is type native-2
*    instead of default of display.
*    02 arr2 NATIVE-2 occurs 1 to 10 times depending on s2.
     02 arr2          occurs 1 to 10 times depending on s2.
        03 x native-2 value 17.
        03 y native-2 value 18.

* above test used depending on variable outside the record
* this test has it inside the record.

  01 rec4.
     02 r41.
        03 r411 NATIVE-2 VALUE 2.
        03 arr4          occurs 1 to 4 times depending on r411.
           04 xx native-2 value 19.
           04 yy native-2 value 20.

* this has depending on symbol within the record but the
* array is just an array of shorts
  01 rec5.
     02 r51.
        03 r511 NATIVE-2 VALUE 5.
        03 arr5 NATIVE-2 occurs 1 to 7 times depending on r511.

* can depending on be nested? No.
* ** Error 165 **  Variable occurrences not permitted for subordinate table

 PROCEDURE DIVISION.
 main-start.

   Display "Start main".

   Move 2 to s.
   Move 1 to arr(1).
   Move 2 to arr(2).
   Move 3 to arr(3).
   Move 4 to arr(4).

   Move 11 to rca(1).
   Move 12 to rca(2).

   Move 3 to rcs.
   Move 21 to rca(1).
   Move 22 to rca(2).
   Move 23 to rca(3).

   Display "End main".

 END PROGRAM main.
