* Test to use multiple params.
* there was a problem where eCOBOL claimed all params were at same offset
* from R12.
?MAIN main
 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

* signed short parameter
  01  mi  NATIVE-2 VALUE 22.
  01  mj  NATIVE-2 VALUE 23.
  01  mk  NATIVE-2 VALUE 24.
  01  ml  NATIVE-2 VALUE 25.
  01  mm  NATIVE-2 VALUE 26.

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 main-start.
   Display "Start of main".
   Display mi.
   Display mj.

 Call-Dog.
   Display "calling all dogs".
   Call dog using mi mj mk ml mm.
   Display "Back from dog".
   Display "End of main".
 END PROGRAM main.

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  dog.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01  di  NATIVE-2 VALUE 32.
  01  dj  NATIVE-2 VALUE 33.

 LINKAGE SECTION.

* parameters
  01 pi NATIVE-2.
  01 pj NATIVE-2.
  01 pk NATIVE-2.
  01 pl NATIVE-2.
  01 pm NATIVE-2.

 PROCEDURE DIVISION using pi pj pk pl pm.
 dog-start.

   DISPLAY "In dog".
   DISPLAY di.
   DISPLAY dj.
   DISPLAY "Param pi is " pi.
   DISPLAY "Param pj is " pj.
   Move pi to di.
   Move pj to dj.
   DISPLAY di.
   DISPLAY dj.
   DISPLAY "Leaving dog".

 END PROGRAM dog.
