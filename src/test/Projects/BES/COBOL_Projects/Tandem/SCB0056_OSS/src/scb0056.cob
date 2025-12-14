*******************************************************************************
*
* Program: XCB56P0
*
* Purpose:
*
?symbols

?SECTION main-program-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  main-program.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

   01 main-alpha-a  PIC A.

 PROCEDURE DIVISION.

?SECTION main-begin-code
 main-begin-code SECTION.
   main-program-begin.

     DISPLAY "PARAGRAPH: main-program-begin OF main-begin-code".

     PERFORM main-program-set-data.

     PERFORM main-program-display.

     PERFORM main-ambig-01 OF main-ambig-01-01.

     CALL nested-sub-main-program USING main-alpha-a.

     PERFORM main-program-display.

     PERFORM main-ambig-01 OF main-ambig-01-02.

     CALL nested-sub-main-program USING main-alpha-a.

     PERFORM main-program-display.

     PERFORM main-ambig-01 OF main-ambig-01-03.

     CALL sub-program-one USING main-alpha-a.

     PERFORM main-program-display.

     PERFORM main-ambig-02 OF main-ambig-02-01.

     CALL sub-program-two USING main-alpha-a.

     PERFORM main-program-display.

     PERFORM main-ambig-02 OF main-ambig-02-02.

     CALL sub-program-two USING main-alpha-a.

     PERFORM main-program-display.

     PERFORM main-ambig-02 OF main-ambig-02-03.

     PERFORM main-program-exit.

?SECTION main-exit-code
 main-exit-code SECTION.
   main-program-exit.

     DISPLAY "PARAGRAPH: main-program-exit OF main-exit-code".

     STOP RUN.

?SECTION main-set-data-code
 main-set-data-code SECTION.
   main-program-set-data.

     DISPLAY "PARAGRAPH: main-program-set-data OF main-set-data-code".

     MOVE "A" TO main-alpha-a.

?SECTION main-display-code
 main-display-code SECTION.
   main-program-display.

     DISPLAY "PARAGRAPH: main-program-display OF main-display-code".

     DISPLAY main-alpha-a.

?SECTION main-tp-01-code
 main-tp-01-code SECTION.
   main-program-tp-01.

     DISPLAY "PARAGRAPH: main-program-tp-01 OF main-tp-01-code".

?SECTION main-ambig-01-01
 main-ambig-01-01 SECTION.
   main-ambig-01.

     DISPLAY "PARAGRAPH: main-ambig-01 OF main-ambig-01-01".

?SECTION main-ambig-01-02
 main-ambig-01-02 SECTION.
   main-ambig-01.

     DISPLAY "PARAGRAPH: main-ambig-01 OF main-ambig-01-02".

?SECTION main-ambig-01-03
 main-ambig-01-03 SECTION.
   main-ambig-01.

     DISPLAY "PARAGRAPH: main-ambig-01 OF main-ambig-01-03".

?SECTION main-ambig-02-01
 main-ambig-02-01 SECTION.
   main-ambig-02.

     DISPLAY "PARAGRAPH: main-ambig-02 OF main-ambig-02-01".

?SECTION main-ambig-02-02
 main-ambig-02-02 SECTION.
   main-ambig-02.

     DISPLAY "PARAGRAPH: main-ambig-02 OF main-ambig-02-02".

?SECTION main-ambig-02-03
 main-ambig-02-03 SECTION.
   main-ambig-02.

     DISPLAY "PARAGRAPH: main-ambig-02 OF main-ambig-02-03".

*****************************************************************************
?SECTION nested-sub-main-program-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-main-program.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-main-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-main-alpha-a.

?SECTION nested-sub-main-begin-code
 nested-sub-main-begin-code SECTION.
   nested-sub-main-program-begin.

     DISPLAY "PARAGRAPH: nested-sub-main-program-begin OF nested-sub-main-begin-code".

     PERFORM nested-sub-main-display.

     PERFORM nested-sub-main-ambig-01 OF nested-sub-main-ambig-01-02.

     PERFORM nested-sub-main-set-data.

     PERFORM nested-sub-main-display.

     PERFORM nested-sub-main-ambig-01 OF nested-sub-main-ambig-01-01.

     PERFORM nested-sub-main-exit.

?SECTION nested-sub-main-exit-code
 nested-sub-main-exit-code SECTION.
   nested-sub-main-exit.

     DISPLAY "PARAGRAPH: nested-sub-main-exit OF nested-sub-main-exit-code".

     EXIT PROGRAM.

?SECTION nested-sub-main-set-data-code
 nested-sub-main-set-data-code SECTION.
   nested-sub-main-set-data.

     DISPLAY "PARAGRAPH: nested-sub-main-set-data OF nested-sub-main-set-data-code".

     MOVE "B" TO n-s-main-alpha-a.

?SECTION nested-sub-main-display-code
 nested-sub-main-display-code SECTION.
   nested-sub-main-display.

     DISPLAY "PARAGRAPH: nested-sub-main-display OF nested-sub-main-display-code".

     DISPLAY n-s-main-alpha-a.

?SECTION nested-sub-main-tp-01-code
 nested-sub-main-tp-01-code SECTION.
   nested-sub-main-tp-01.

     DISPLAY "PARAGRAPH: nested-sub-main-tp-01 OF nested-sub-main-tp-01-code".

?SECTION nested-sub-main-ambig-01-01
 nested-sub-main-ambig-01-01 SECTION.
   nested-sub-main-ambig-01.

     DISPLAY "PARAGRAPH: nested-sub-main-ambig-01 OF nested-sub-main-ambig-01-01".

?SECTION nested-sub-main-ambig-01-02
 nested-sub-main-ambig-01-02 SECTION.
   nested-sub-main-ambig-01.

     DISPLAY "PARAGRAPH: nested-sub-main-ambig-01 OF nested-sub-main-ambig-01-02".

 END PROGRAM nested-sub-main-program.
*****************************************************************************

 END PROGRAM main-program.
******************************************************************************
?SECTION sub-program-one-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  sub-program-one.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 s-p-one-alpha-a  PIC A.

 PROCEDURE DIVISION USING s-p-one-alpha-a.

?SECTION sub-one-begin-code
 sub-one-begin-code SECTION.
   sub-program-one-begin.

     DISPLAY "PARAGRAPH: sub-program-one-begin OF sub-one-begin-code".

     PERFORM sub-program-one-display.

     PERFORM sub-one-ambig-01 OF sub-one-ambig-01-03.

     CALL nested-sub-program-levelOne USING s-p-one-alpha-a.

     PERFORM sub-program-one-display.

     PERFORM sub-one-ambig-01 OF sub-one-ambig-01-02.

     PERFORM sub-program-one-set-data.

     PERFORM sub-program-one-display.

     PERFORM sub-one-ambig-01 OF sub-one-ambig-01-01.

     PERFORM sub-program-one-exit.

?SECTION sub-one-exit-code
 sub-one-exit-code SECTION.
   sub-program-one-exit.

     DISPLAY "PARAGRAPH: sub-program-one-exit OF sub-one-exit-code".

     EXIT PROGRAM.

?SECTION sub-one-set-data-code
 sub-one-set-data-code SECTION.
   sub-program-one-set-data.

     DISPLAY "PARAGRAPH: sub-program-one-set-data OF sub-one-set-data-code".

     MOVE "C" TO s-p-one-alpha-a.

?SECTION sub-one-display-code
 sub-one-display-code SECTION.
   sub-program-one-display.

     DISPLAY "PARAGRAPH: sub-program-one-display OF sub-one-display-code".

     DISPLAY s-p-one-alpha-a.

?SECTION sub-one-tp-01-code
 sub-one-tp-01-code SECTION.
   sub-program-one-tp-01.

     DISPLAY "PARAGRAPH: sub-program-one-tp-01 OF sub-one-tp-01-code".

?SECTION sub-one-ambig-01-01
 sub-one-ambig-01-01 SECTION.
   sub-one-ambig-01.

     DISPLAY "PARAGRAPH: sub-one-ambig-01 OF sub-one-ambig-01-01".

?SECTION sub-one-ambig-01-02
 sub-one-ambig-01-02 SECTION.
   sub-one-ambig-01.

     DISPLAY "PARAGRAPH: sub-one-ambig-01 OF sub-one-ambig-01-02".

?SECTION sub-one-ambig-01-03
 sub-one-ambig-01-03 SECTION.
   sub-one-ambig-01.

     DISPLAY "PARAGRAPH: sub-one-ambig-01 OF sub-one-ambig-01-03".

*****************************************************************************
?SECTION nested-sub-levelOne-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-program-levelOne.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-p-one-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-p-one-alpha-a.

?SECTION nested-levelOne-begin-code
 nested-levelOne-begin-code SECTION.
   nested-sub-levelOne-begin.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-begin OF nested-levelOne-begin-code".

     PERFORM nested-sub-levelOne-display.

     PERFORM nested-sub-levelOne-ambig-01 OF nested-levelOne-ambig-01-01.

     CALL nested-sub-program-levelTwo USING n-s-p-one-alpha-a.

     PERFORM nested-sub-levelOne-display.

     PERFORM nested-sub-levelOne-ambig-01 OF nested-levelOne-ambig-01-02.

     PERFORM nested-sub-levelOne-set-data.

     PERFORM nested-sub-levelOne-display.

     PERFORM nested-sub-levelOne-ambig-01 OF nested-levelOne-ambig-01-01.

     PERFORM nested-sub-levelOne-exit.

?SECTION nested-levelOne-exit-code
 nested-levelOne-exit-code SECTION.
   nested-sub-levelOne-exit.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-exit OF nested-levelOne-exit-code".

     EXIT PROGRAM.

?SECTION nested-levelOne-set-data-code
 nested-levelOne-set-data-code SECTION.
   nested-sub-levelOne-set-data.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-set-data OF nested-levelOne-set-data-code".

     MOVE "D" TO n-s-p-one-alpha-a.

?SECTION nested-levelOne-display-code
 nested-levelOne-display-code SECTION.
   nested-sub-levelOne-display.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-display OF nested-levelOne-display-code".

     DISPLAY n-s-p-one-alpha-a.

?SECTION nested-levelOne-tp-01-code
 nested-levelOne-tp-01-code SECTION.
   nested-sub-levelOne-tp-01.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-tp-01 OF nested-levelOne-tp-01-code".

?SECTION nested-levelOne-ambig-01-01
 nested-levelOne-ambig-01-01 SECTION.
   nested-sub-levelOne-ambig-01.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-ambig-01 OF nested-levelOne-ambig-01-01".

?SECTION nested-levelOne-ambig-01-02
 nested-levelOne-ambig-01-02 SECTION.
   nested-sub-levelOne-ambig-01.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-ambig-01 OF nested-levelOne-ambig-01-02".

?SECTION nested-levelOne-ambig-02-01
 nested-levelOne-ambig-02-01 SECTION.
   nested-sub-levelOne-ambig-02.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-ambig-02 OF nested-levelOne-ambig-02-01".

?SECTION nested-levelOne-ambig-02-02
 nested-levelOne-ambig-02-02 SECTION.
   nested-sub-levelOne-ambig-02.

     DISPLAY "PARAGRAPH: nested-sub-levelOne-ambig-02 OF nested-levelOne-ambig-02-02".

*****************************************************************************
?SECTION nested-sub-levelTwo-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-program-levelTwo.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-p-two-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-p-two-alpha-a.

?SECTION nested-levelTwo-begin-code
 nested-levelTwo-begin-code SECTION.
   nested-sub-levelTwo-begin.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-begin OF nested-levelTwo-begin-code".

     PERFORM nested-sub-levelTwo-display.

     PERFORM nested-sub-levelTwo-ambig-02 OF nested-levelTwo-ambig-02-04.

     PERFORM nested-sub-levelTwo-set-data.

     PERFORM nested-sub-levelTwo-display.

     PERFORM nested-sub-levelTwo-ambig-01 OF nested-levelTwo-ambig-01-03.

     PERFORM nested-sub-levelTwo-exit.

?SECTION nested-levelTwo-exit-code
 nested-levelTwo-exit-code SECTION.
   nested-sub-levelTwo-exit.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-exit OF nested-levelTwo-exit-code".

     EXIT PROGRAM.

?SECTION nested-levelTwo-set-data-code
 nested-levelTwo-set-data-code SECTION.
   nested-sub-levelTwo-set-data.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-set-data OF nested-levelTwo-set-data-code".

     MOVE "E" TO n-s-p-two-alpha-a.

?SECTION nested-levelTwo-display-code
 nested-levelTwo-display-code SECTION.
   nested-sub-levelTwo-display.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-display OF nested-levelTwo-display-code".

     DISPLAY n-s-p-two-alpha-a.

?SECTION nested-levelTwo-tp-01-code
 nested-levelTwo-tp-01-code SECTION.
   nested-sub-levelTwo-tp-01.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-tp-01 OF nested-levelTwo-tp-01-code".

?SECTION nested-levelTwo-ambig-01-01
 nested-levelTwo-ambig-01-01 SECTION.
   nested-sub-levelTwo-ambig-01.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-ambig-01 OF nested-levelTwo-ambig-01-01".

?SECTION nested-levelTwo-ambig-01-02
 nested-levelTwo-ambig-01-02 SECTION.
   nested-sub-levelTwo-ambig-01.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-ambig-01 OF nested-levelTwo-ambig-01-02".

?SECTION nested-levelTwo-ambig-01-03
 nested-levelTwo-ambig-01-03 SECTION.
   nested-sub-levelTwo-ambig-01.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-ambig-01 OF nested-levelTwo-ambig-01-03".

?SECTION nested-levelTwo-ambig-01-04
 nested-levelTwo-ambig-01-04 SECTION.
   nested-sub-levelTwo-ambig-01.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-ambig-01 OF nested-levelTwo-ambig-01-04".

?SECTION nested-levelTwo-ambig-02-01
 nested-levelTwo-ambig-02-01 SECTION.
   nested-sub-levelTwo-ambig-02.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-ambig-02 OF nested-levelTwo-ambig-02-01".

?SECTION nested-levelTwo-ambig-02-02
 nested-levelTwo-ambig-02-02 SECTION.
   nested-sub-levelTwo-ambig-02.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-ambig-02 OF nested-levelTwo-ambig-02-02".

?SECTION nested-levelTwo-ambig-02-03
 nested-levelTwo-ambig-02-03 SECTION.
   nested-sub-levelTwo-ambig-02.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-ambig-02 OF nested-levelTwo-ambig-02-03".

?SECTION nested-levelTwo-ambig-02-04
 nested-levelTwo-ambig-02-04 SECTION.
   nested-sub-levelTwo-ambig-02.

     DISPLAY "PARAGRAPH: nested-sub-levelTwo-ambig-02 OF nested-levelTwo-ambig-02-04".

 END PROGRAM nested-sub-program-levelTwo.
*****************************************************************************

 END PROGRAM nested-sub-program-levelOne.
*****************************************************************************

 END PROGRAM sub-program-one.
*****************************************************************************
?SECTION sub-program-two-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  sub-program-two.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 s-p-two-alpha-a  PIC A.

 PROCEDURE DIVISION USING s-p-two-alpha-a.

?SECTION sub-two-begin-code
 sub-two-begin-code SECTION.
   sub-program-two-begin.

     DISPLAY "PARAGRAPH: sub-program-two-begin OF sub-two-begin-code".

     PERFORM sub-program-two-display.

     PERFORM sub-two-ambig-01 OF sub-two-ambig-01-01.

     PERFORM sub-two-ambig-02 OF sub-two-ambig-02-02.

     PERFORM sub-two-ambig-03 OF sub-two-ambig-03-03.

     PERFORM sub-program-two-set-data.

     PERFORM sub-program-two-display.

     PERFORM sub-two-ambig-04 OF sub-two-ambig-04-04.

     PERFORM sub-two-ambig-05 OF sub-two-ambig-05-05.

     PERFORM sub-two-ambig-06 OF sub-two-ambig-06-06.

     PERFORM sub-program-two-exit.

?SECTION sub-two-exit-code
 sub-two-exit-code SECTION.
   sub-program-two-exit.

     DISPLAY "PARAGRAPH: sub-program-two-exit OF sub-two-exit-code".

     EXIT PROGRAM.

?SECTION sub-two-set-data-code
 sub-two-set-data-code SECTION.
   sub-program-two-set-data.

     DISPLAY "PARAGRAPH: sub-program-two-set-data OF sub-two-set-data-code".

     MOVE "F" to s-p-two-alpha-a.

?SECTION sub-two-display-code
 sub-two-display-code SECTION.
   sub-program-two-display.

     DISPLAY "PARAGRAPH: sub-program-two-display OF sub-two-display-code".

     DISPLAY s-p-two-alpha-a.

?SECTION sub-two-tp-01-code
 sub-two-tp-01-code SECTION.
   sub-program-two-tp-01.

     DISPLAY "PARAGRAPH: sub-program-two-tp-01 OF sub-two-tp-01-code".

?SECTION sub-two-ambig-01-01
 sub-two-ambig-01-01 SECTION.
   sub-two-ambig-01.

     DISPLAY "PARAGRAPH: sub-two-ambig-01 OF sub-two-ambig-01-01".

?SECTION sub-two-ambig-01-02
 sub-two-ambig-01-02 SECTION.
   sub-two-ambig-01.

     DISPLAY "PARAGRAPH: sub-two-ambig-01 OF sub-two-ambig-01-02".

?SECTION sub-two-ambig-01-03
 sub-two-ambig-01-03 SECTION.
   sub-two-ambig-01.

     DISPLAY "PARAGRAPH: sub-two-ambig-01 OF sub-two-ambig-01-03".

?SECTION sub-two-ambig-02-01
 sub-two-ambig-02-01 SECTION.
   sub-two-ambig-02.

     DISPLAY "PARAGRAPH: sub-two-ambig-02 OF sub-two-ambig-02-01".

?SECTION sub-two-ambig-02-02
 sub-two-ambig-02-02 SECTION.
   sub-two-ambig-02.

     DISPLAY "PARAGRAPH: sub-two-ambig-02 OF sub-two-ambig-02-02".

?SECTION sub-two-ambig-02-03
 sub-two-ambig-02-03 SECTION.
   sub-two-ambig-02.

     DISPLAY "PARAGRAPH: sub-two-ambig-02 OF sub-two-ambig-02-03".

?SECTION sub-two-ambig-03-01
 sub-two-ambig-03-01 SECTION.
   sub-two-ambig-03.

     DISPLAY "PARAGRAPH: sub-two-ambig-03 OF sub-two-ambig-03-01".

?SECTION sub-two-ambig-03-02
 sub-two-ambig-03-02 SECTION.
   sub-two-ambig-03.

     DISPLAY "PARAGRAPH: sub-two-ambig-03 OF sub-two-ambig-03-02".

?SECTION sub-two-ambig-03-03
 sub-two-ambig-03-03 SECTION.
   sub-two-ambig-03.

     DISPLAY "PARAGRAPH: sub-two-ambig-03 OF sub-two-ambig-03-03".

?SECTION sub-two-ambig-04-01
 sub-two-ambig-04-01 SECTION.
   sub-two-ambig-04.

     DISPLAY "PARAGRAPH: sub-two-ambig-04 OF sub-two-ambig-04-01".

?SECTION sub-two-ambig-04-02
 sub-two-ambig-04-02 SECTION.
   sub-two-ambig-04.

     DISPLAY "PARAGRAPH: sub-two-ambig-04 OF sub-two-ambig-04-02".

?SECTION sub-two-ambig-04-03
 sub-two-ambig-04-03 SECTION.
   sub-two-ambig-04.

     DISPLAY "PARAGRAPH: sub-two-ambig-04 OF sub-two-ambig-04-03".

?SECTION sub-two-ambig-04-04
 sub-two-ambig-04-04 SECTION.
   sub-two-ambig-04.

     DISPLAY "PARAGRAPH: sub-two-ambig-04 OF sub-two-ambig-04-04".

?SECTION sub-two-ambig-05-01
 sub-two-ambig-05-01 SECTION.
   sub-two-ambig-05.

     DISPLAY "PARAGRAPH: sub-two-ambig-05 OF sub-two-ambig-05-01".

?SECTION sub-two-ambig-05-02
 sub-two-ambig-05-02 SECTION.
   sub-two-ambig-05.

     DISPLAY "PARAGRAPH: sub-two-ambig-05 OF sub-two-ambig-05-02".

?SECTION sub-two-ambig-05-03
 sub-two-ambig-05-03 SECTION.
   sub-two-ambig-05.

     DISPLAY "PARAGRAPH: sub-two-ambig-05 OF sub-two-ambig-05-03".

?SECTION sub-two-ambig-05-04
 sub-two-ambig-05-04 SECTION.
   sub-two-ambig-05.

     DISPLAY "PARAGRAPH: sub-two-ambig-05 OF sub-two-ambig-05-04".

?SECTION sub-two-ambig-05-05
 sub-two-ambig-05-05 SECTION.
   sub-two-ambig-05.

     DISPLAY "PARAGRAPH: sub-two-ambig-05 OF sub-two-ambig-05-05".

?SECTION sub-two-ambig-06-01
 sub-two-ambig-06-01 SECTION.
   sub-two-ambig-06.

     DISPLAY "PARAGRAPH: sub-two-ambig-06 OF sub-two-ambig-06-01".

?SECTION sub-two-ambig-06-02
 sub-two-ambig-06-02 SECTION.
   sub-two-ambig-06.

     DISPLAY "PARAGRAPH: sub-two-ambig-06 OF sub-two-ambig-06-02".

?SECTION sub-two-ambig-06-03
 sub-two-ambig-06-03 SECTION.
   sub-two-ambig-06.

?SECTION sub-two-ambig-06-04
 sub-two-ambig-06-04 SECTION.
   sub-two-ambig-06.

     DISPLAY "PARAGRAPH: sub-two-ambig-06 OF sub-two-ambig-06-04".

?SECTION sub-two-ambig-06-05
 sub-two-ambig-06-05 SECTION.
   sub-two-ambig-06.

     DISPLAY "PARAGRAPH: sub-two-ambig-06 OF sub-two-ambig-06-05".

?SECTION sub-two-ambig-06-06
 sub-two-ambig-06-06 SECTION.
   sub-two-ambig-06.

     DISPLAY "PARAGRAPH: sub-two-ambig-06 OF sub-two-ambig-06-06".

 END PROGRAM sub-program-two.
*****************************************************************************
