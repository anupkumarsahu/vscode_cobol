* VI loops for INFO ID same, when "same" is in two records
* and there is a nested routine.
* Test case created for IS4469.
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

 01 em PIC x(4)  VALUE "Emma".

* Symbol same name as level 1 routine
* not allowed
* 01 main PIC x(4) VALUE "MAIN".


*INFO ID bb loops. bb is also in next record.
*INFO ID cc loops. cc is the name of another record.
*can we have "main" within record?  No.
 01 rec.
    02 bb   PIC 99 VALUE 01.
    02 cc   PIC X(4) VALUE "Emma".
*   02 main PIC X(4) VALUE "main".

 01 rec2.
    02 bb   PIC 99 VALUE 01.
    02 dd   PIC X(3) VALUE "r21".

 01 cc.
    02 sara PIC x(4) VALUE "sara".

 PROCEDURE DIVISION.
 start-main.

   DISPLAY "MAIN: Congratulations!".
   Move 2 to bb of rec.
   Move 3 to bb of rec2.
   Move "lisa" to sara.

 Call-Dog.
   Call "dog".
   DISPLAY "Returned from dog".
   DISPLAY "Exiting MAIN".
   STOP RUN.

* Begin DOG...
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  ds  NATIVE-2 VALUE 122.
  01  dg  NATIVE-2 VALUE 123 GLOBAL.
  01  bb  NATIVE-2 VALUE 124.

  01  maindog NATIVE-2 VALUE 124.

 PROCEDURE DIVISION.
 Start-Dog.

   DISPLAY "In dog".
   Move 117 to ds.
   Move 118 to dg.
   Move 119 to bb.

 END PROGRAM dog.
 END PROGRAM main.
