* one 3rd level routine: main.dog.cat
* Test access to a single global from within nested routine.
* Test access to referenced and unrefd globals within level 1 routine.
?MAIN main
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01 em PIC x(4) VALUE "Emma".
 01 sh NATIVE-2 VALUE 42.

 01 mg NATIVE-2 VALUE 22 GLOBAL.

* This global is not referenced at all.
 01 mu NATIVE-2 VALUE 17 GLOBAL.
 01 recu GLOBAL.
    02 ru1 NATIVE-2 VALUE 18.

* This global is only referenced in dog but not in main.
 01 mrdog NATIVE-2 VALUE 117 GLOBAL.
 01 recdog GLOBAL.
    02 rd1 NATIVE-2 VALUE 118.

 PROCEDURE DIVISION.
 start-main.

   DISPLAY "In main".
   Move 32 to mg.

 Call-Dog.
   Call "dog".

 Call-Sun.
   Call "sun".

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
   Move 142 to dg.
   Move 217 to mrdog.
   Move 218 to rd1.

 Call-Cat.
   Call "cat".

* Cat is a nested program of dog
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  cat.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* not a global since cat is deepest routine.
* v for vanilla
 01 cv NATIVE-2 VALUE 242.

 PROCEDURE DIVISION.
 Start-Cat.

   DISPLAY "In cat".
   Move  52 to mg.
   Move 152 to dg.
   Move 252 to cv.

 END PROGRAM cat.
 END PROGRAM dog.
 END PROGRAM main.


 IDENTIFICATION DIVISION.
 PROGRAM-ID.  Sun.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short
  01 sg NATIVE-2 GLOBAL VALUE 17.

 PROCEDURE DIVISION.
 sun-start.

   DISPLAY "In sun".
   Move 27 to sg.
   Call "moon".

* moon is a nested program of main
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  moon.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01 moong NATIVE-2 GLOBAL VALUE 127.

 PROCEDURE DIVISION.
 Start-Moon.

   DISPLAY "In moon".
   Move   37 to sg.
   Move  137 to moong.

 END PROGRAM moon.
 END PROGRAM sun.
