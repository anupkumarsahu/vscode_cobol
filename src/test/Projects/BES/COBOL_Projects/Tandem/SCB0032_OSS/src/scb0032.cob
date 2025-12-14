* Test pointer type and linkage section variables that are not parameters
?MAIN main
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short parameter
  01  mi  NATIVE-2 VALUE 22.

* rec param
  01 mrec.
     02 mr11 NATIVE-2.
     02 mr12 NATIVE-2.

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 main-start.
   Display "Start of main".
   Display mi.
   Move 17 to mr11.
   Move 18 to mr12.
   Display "mr11 = " mr11.
   Display mr12.

 Call-Dog.
   Display "calling all dogs".
   Call dog using mrec mi.
   Display "Back from dog".
   Display "End of main".
 END PROGRAM main.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01  di  NATIVE-2 VALUE 32.
  01  dp  POINTER.

  01 drec.
     02 dr11 NATIVE-2 VALUE 33.
     02 dr12 NATIVE-2 VALUE 34.

 LINKAGE SECTION.

* parameters
  01 pi NATIVE-2.

  01 prec.
     02 pr11 NATIVE-2.
     02 pr12 NATIVE-2.

  01 lsi NATIVE-2.

 PROCEDURE DIVISION using prec pi.
 dog-start.

   DISPLAY "In dog".
   DISPLAY "Param pi is " pi.
   DISPLAY "Param pr11 is " pr11.
   DISPLAY "Param pr12 is " pr12.
   set dp to address of di.
   set address of lsi to address of di.
   Display lsi.
   Move pi to di.
   Move prec to drec.
   DISPLAY di.
   DISPLAY drec.
   DISPLAY "Leaving dog".

 END PROGRAM dog.
