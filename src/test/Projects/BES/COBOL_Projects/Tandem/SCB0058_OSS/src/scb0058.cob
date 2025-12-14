*******************************************************************************
*
* Program: XCB58P0
*
* Purpose: This program has programs that contain paragraph names whose name
*          arevalid hexidecimal numbers.
*
?symbols
 identification division.
 program-id.  main-program.

 data division.
 working-storage section.

 01 MAIN-ALPHA-A              PIC A VALUE "A".

 procedure division.
 begin-main-program.

    DISPLAY "Main-Program".

    PERFORM BAD.
    PERFORM DEAD.
    PERFORM BAD-DEAD.
    PERFORM AAA THRU CCC.

    CALL nested-sub-main-program using MAIN-ALPHA-A.

    STOP RUN.

 AAA.

    DISPLAY "PARAGRAPH: AAA".

 BBB.

    DISPLAY "PARAGRAPH: BBB".

 CCC.

    DISPLAY "PARAGRAPH: CCC".

 BAD.

    DISPLAY "PARAGRAPH: BAD".

 DEAD.

    DISPLAY "PARAGRAPH: DEAD".

 BAD-DEAD.

    DISPLAY "PARAGRAPH: BAD-DEAD".

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

   PERFORM BEAD.
   PERFORM ADE.
   PERFORM ADE-ADE.

   CALL sub-program-one using N-S-MAIN-ALPHA-A.

   PERFORM nested-sub-main-exit.

 BEAD.

    DISPLAY "PARAGRAPH: BEAD".

 ADE.

    DISPLAY "PARAGRAPH: ADE".

 ADE-ADE.

    DISPLAY "PARAGRAPH: ADE-ADE".

 nested-sub-main-exit.

   DISPLAY "PARAGRAPH: nested-sub-main-exit".

   EXIT PROGRAM.

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

   PERFORM ABCDEF.
   PERFORM A0B1C2.
   PERFORM 3D4E5F.

   PERFORM sub-program-one-exit.

 sub-program-one-exit.

   DISPLAY "PARAGRAPH: sub-program-one-exit".

   EXIT PROGRAM.

 ABCDEF.

    DISPLAY "PARAGRAPH: ABCDEF".

 A0B1C2.

    DISPLAY "PARAGRAPH: A0B1C2".

 3D4E5F.

    DISPLAY "PARAGRAPH: 3D4E5F".

 END PROGRAM sub-program-one.
******************************************************************************
