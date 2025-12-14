 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 77  dummy  NATIVE-2 VALUE  17.

 77  nat  NATIVE-2 VALUE  1.
     88 nata       VALUE  1.
     88 natb       VALUE  2.
     88 natc       VALUE  3 thru 4.
     88 natd       VALUE  5 6 thru 7.
     88 nate       VALUE  8 9 thru 10 11.
     88 natf       VALUE  100 thru 104 17000 thru 22000.

 01  rec2.
     02  r21 NATIVE-2 VALUE 500.
         88  r211  value 1.
         88  r212  value 17.
         88  r213  value 22.
         88  r214  value 500.
     02  r22 PIC X(9) VALUE "Hi, Dale!".


 PROCEDURE DIVISION.
 main-start.

   Display "Start main".
   Move 22 to dummy.
   Move 2  to nat.
   Display "End main".

 END PROGRAM main.

