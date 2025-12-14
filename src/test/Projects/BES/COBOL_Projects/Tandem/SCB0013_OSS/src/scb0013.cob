* test unreferenced global.
* global is only referenced in level 2 or 3 routine but non in level 1.
* Compiler was not emitting location info for these globals.
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

 01  em PIC x(4)  VALUE "Emma".

* unreferenced in main but referenced in dog and cat
  01  mgur  NATIVE-2 VALUE 30 GLOBAL.

* only refd in dog
  01  mgurd NATIVE-2 VALUE 31 GLOBAL.

* only refd in cat.
  01  mgurc NATIVE-2 VALUE 32 GLOBAL.

 PROCEDURE DIVISION.
 start-main.

   DISPLAY "MAIN: Congratulations!".

 Call-Dog.
   Call "dog".
   DISPLAY "Returned from dog".
   DISPLAY "Exiting MAIN".
   STOP RUN.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* unreferenced in dog but referenced in cat
  01  dgurc  NATIVE-2 VALUE 40 GLOBAL.

 PROCEDURE DIVISION.
 Start-Dog.

   DISPLAY "In dog".
   Move 120 to mgur.
   Move 121 to mgurd.

 Call-Cat.
   Call "cat".
   DISPLAY "Returned from cat".
   DISPLAY "Leaving dog".

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  cat.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01  cs  NATIVE-2 VALUE 222.

 PROCEDURE DIVISION.
 Start-Cat.

   DISPLAY "In cat".

   Move 221 to mgur.
   Move 222 to mgurc.
   Move 223 to dgurc.

   DISPLAY "Leaving cat".

 END PROGRAM cat.
 END PROGRAM dog.
 END PROGRAM main.
