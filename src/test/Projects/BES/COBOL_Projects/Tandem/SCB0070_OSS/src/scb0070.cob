*******************************************************************************
*
* Program: XCB70P0
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

 main-program-begin.

   DISPLAY "PARAGRAPH: main-program-begin".

   PERFORM main-program-foo THRU main-program-set-data.

   PERFORM main-program-exit.

 main-program-exit.

    DISPLAY "PARAGRAPH: main-program-exit".
    STOP RUN.

 main-program-foo.

    DISPLAY "PARAGRAPH: main-program-foo".

 main-program-bar.

    DISPLAY "PARAGRAPH: main-program-bar".

 main-program-sec SECTION.

 main-program-bar.

    DISPLAY "PARAGRAPH: main-program-bar OF main-program-sec".

 main-program-set-data.

    DISPLAY "PARAGRAPH: main-program-set-data OF main-program-sec".

    MOVE "A" TO main-alpha-a.

 main-program-display-data.

    DISPLAY "PARAGRAPH: main-program-display-data".

    DISPLAY main-alpha-a.

    PERFORM ambiguous-paragraph-01  OF  main-program-ambig-02-sec.

 ambig-section-name SECTION.

 main-program-tp-01.

    DISPLAY "PARAGRAPH: main-program-tp-01".

 main-program-ambig-01-sec SECTION.

 ambiguous-paragraph-01.

    DISPLAY "MAIN-PROGRAM PARAGRAPH: ambiguous-paragraph-01 OF main-program-ambig-01-sec".

 main-program-ambig-02-sec SECTION.

 ambiguous-paragraph-01.

    DISPLAY "MAIN-PROGRAM PARAGRAPH: ambiguous-paragraph-01 OF main-program-ambig-02-sec".

*****************************************************************************
?SECTION nested-sub-main-program-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-main-program.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-main-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-main-alpha-a.

 nested-sub-main-program-sec SECTION.

 nested-sub-main-program-begin.

   DISPLAY "PARAGRAPH: nested-sub-main-program-begin".

   PERFORM nested-sub-main-display-data.

   PERFORM nested-sub-main-tp-01.

   PERFORM nested-sub-main-set-data.

   PERFORM nested-sub-main-display-data.

   PERFORM nested-sub-main-tp-01.

   PERFORM ambiguous-paragraph-02.

   PERFORM nested-sub-main-exit.

 nested-sub-main-exit.

   DISPLAY "PARAGRAPH: nested-sub-main-exit".

   EXIT PROGRAM.

 nested-sub-main-set-data.

   DISPLAY "PARAGRAPH: nested-sub-main-set-data".

   MOVE "B" TO n-s-main-alpha-a.

 nested-sub-main-display-data.

   DISPLAY "PARAGRAPH: nested-sub-main-display-data".

   DISPLAY n-s-main-alpha-a.

   PERFORM ambiguous-paragraph-01.

 ambig-section-name SECTION.

 nested-sub-main-tp-01.

   DISPLAY "PARAGRAPH: nested-sub-main-tp-01".

 ambiguous-paragraph-01.

    DISPLAY "NESTED-SUB-MAIN-PROGRAM PARAGRAPH: ambiguous-paragraph-01".

 ambiguous-paragraph-02.

    DISPLAY "NESTED-SUB-MAIN-PROGRAM PARAGRAPH: ambiguous-paragraph-02".

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

 sub-program-one-sec SECTION.

 sub-program-one-begin.

   DISPLAY "PARAGRAPH: sub-program-one-begin".

   PERFORM sub-program-one-display-data.

   PERFORM sub-program-one-tp-01.

   CALL nested-sub-program-levelOne USING s-p-one-alpha-a.

   PERFORM sub-program-one-display-data.

   PERFORM sub-program-one-tp-01.

   PERFORM sub-program-one-set-data.

   PERFORM sub-program-one-display-data.

   PERFORM sub-program-one-tp-01.

   PERFORM ambiguous-paragraph-02.

   PERFORM sub-program-one-exit.

 sub-program-one-exit.

   DISPLAY "PARAGRAPH: sub-program-one-exit".

   EXIT PROGRAM.

 sub-program-one-set-data.

   DISPLAY "PARAGRAPH: sub-program-one-set-data".

   MOVE "C" TO s-p-one-alpha-a.

   PERFORM ambiguous-paragraph-03.

 sub-program-one-display-data.

   DISPLAY "PARAGRAPH: sub-program-one-display-data".

   DISPLAY s-p-one-alpha-a.

   PERFORM ambiguous-paragraph-01.

 sub-program-one-tp-01.

   DISPLAY "PARAGRAPH: sub-program-one-tp-01".

 ambiguous-paragraph-01.

    DISPLAY "SUB-PROGRAM-ONE PARAGRAPH: ambiguous-paragraph-01".

 ambiguous-paragraph-02.

    DISPLAY "SUB-PROGRAM-ONE PARAGRAPH: ambiguous-paragraph-02".

 ambiguous-paragraph-03.

    DISPLAY "SUB-PROGRAM-ONE PARAGRAPH: ambiguous-paragraph-03".

*****************************************************************************
?SECTION nested-sub-levelOne-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-program-levelOne.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-p-one-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-p-one-alpha-a.

 nested-sub-levelOne-sec SECTION.

 nested-sub-levelOne-begin.

   DISPLAY "PARAGRAPH: nested-sub-levelOne-begin".

   PERFORM nested-sub-levelOne-display.

   PERFORM nested-sub-levelOne-tp-01.

   CALL nested-sub-program-levelTwo USING n-s-p-one-alpha-a.

   PERFORM nested-sub-levelOne-display.

   PERFORM nested-sub-levelOne-tp-01.

   PERFORM nested-sub-levelOne-set-data.

   PERFORM nested-sub-levelOne-display.

   PERFORM nested-sub-levelOne-tp-01.

   PERFORM ambiguous-paragraph-02.

   PERFORM nested-sub-levelOne-exit.

 nested-sub-levelOne-exit.

   DISPLAY "PARAGRAPH: nested-sub-levelOne-exit".

   EXIT PROGRAM.

 nested-sub-levelOne-set-data.

   DISPLAY "PARAGRAPH: nested-sub-levelOne-set-data".

   MOVE "D" TO n-s-p-one-alpha-a.

   PERFORM ambiguous-paragraph-03.

 nested-sub-levelOne-display.

   DISPLAY "PARAGRAPH: nested-sub-levelOne-display".

   DISPLAY n-s-p-one-alpha-a.

 nested-sub-levelOne-tp-01.

   DISPLAY "PARAGRAPH: nested-sub-levelOne-tp-01".

 ambiguous-paragraph-02.

    DISPLAY "NESTED-SUB-PROGRAM-LEVELONE PARAGRAPH: ambiguous-paragraph-02".

 ambiguous-paragraph-03.

    DISPLAY "NESTED-SUB-PROGRAM-LEVELONE PARAGRAPH: ambiguous-paragraph-03".

*****************************************************************************
?SECTION nested-sub-levelTwo-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-program-levelTwo.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-p-two-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-p-two-alpha-a.

 nested-sub-levelTwo-sec SECTION.

 nested-sub-levelTwo-begin.

   DISPLAY "PARAGRAPH: nested-sub-levelTwo-begin".

   PERFORM nested-sub-levelTwo-display.

   PERFORM nested-sub-levelTwo-tp-01.

   PERFORM nested-sub-levelTwo-set-data.

   PERFORM nested-sub-levelTwo-display.

   PERFORM nested-sub-levelTwo-tp-01.

   PERFORM nested-sub-levelTwo-exit.

 nested-sub-levelTwo-exit.

   DISPLAY "PARAGRAPH: nested-sub-levelTwo-exit".

   EXIT PROGRAM.

 nested-sub-levelTwo-set-data.

   DISPLAY "PARAGRAPH: nested-sub-levelTwo-set-data".

   MOVE "E" TO n-s-p-two-alpha-a.

   PERFORM ambiguous-paragraph-03.

 nested-sub-levelTwo-display.

   DISPLAY "PARAGRAPH: nested-sub-levelTwo-display".

   DISPLAY n-s-p-two-alpha-a.

   PERFORM ambiguous-paragraph-01.

 nested-sub-levelTwo-tp-01.

   DISPLAY "PARAGRAPH: nested-sub-levelTwo-tp-01".

 ambiguous-paragraph-01.

    DISPLAY "NESTED-SUB-PROGRAM-LEVELTWO PARAGRAPH: ambiguous-paragraph-01".

 ambiguous-paragraph-03.

    DISPLAY "NESTED-SUB-PROGRAM-LEVELTWO PARAGRAPH: ambiguous-paragraph-03".

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

 sub-program-two-sec SECTION.

 sub-program-two-begin.

   DISPLAY "PARAGRAPH: sub-program-two-begin".

   PERFORM sub-program-two-display-data.

   PERFORM sub-program-two-tp-01.

   PERFORM sub-program-two-set-data.

   PERFORM sub-program-two-display-data.

   PERFORM sub-program-two-tp-01.

   PERFORM ambiguous-paragraph-02.

   PERFORM sub-program-two-exit.

 sub-program-two-exit.

   DISPLAY "PARAGRAPH: sub-program-two-exit".

   EXIT PROGRAM.

 sub-program-two-set-data.

   DISPLAY "PARAGRAPH: sub-program-two-set-data".

   MOVE "F" to s-p-two-alpha-a.

 sub-program-two-display-data.

   DISPLAY "PARAGRAPH: sub-program-two-display-data".

   DISPLAY s-p-two-alpha-a.

   PERFORM ambiguous-paragraph-01.

 sub-program-two-tp-01.

   DISPLAY "PARAGRAPH: sub-program-two-tp-01".

 ambiguous-paragraph-01.

    DISPLAY "SUB-PROGRAM-TWO PARAGRAPH: ambiguous-paragraph-01".

 ambiguous-paragraph-02.

    DISPLAY "SUB-PROGRAM-TWO PARAGRAPH: ambiguous-paragraph-02".

 END PROGRAM sub-program-two.
*****************************************************************************
