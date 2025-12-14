* After Pual R. fixed a bug with GLOBAL initialization, I got an eld
* fatal error using CBGLOB.  I think the problem is related to nested routines.
?MAIN main
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01 em PIC x(4) VALUE "Emma".

 01 mg NATIVE-2 VALUE 22 GLOBAL.

 PROCEDURE DIVISION.
 start-main.

   DISPLAY "In main".
   Move 32 to mg.

 Call-Dog.
   Call "dog".

* Dog is a nested program of main
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01 dg NATIVE-2 GLOBAL VALUE 132.

 PROCEDURE DIVISION.
 Start-Dog.

   DISPLAY "In dog".
   Move  42 to mg.

 END PROGRAM dog.
 END PROGRAM main.


