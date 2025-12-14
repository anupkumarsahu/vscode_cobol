*******************************************************************************
*
* Program: XCB55P0
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

?SOURCE SCB0055A(main-begin-code)

?SECTION nested-sub-main-program-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-main-program.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-main-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-main-alpha-a.

?SOURCE SCB0055A(nested-sub-main-begin-code)

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

?SOURCE SCB0055A(sub-one-begin-code)

?SECTION nested-sub-levelOne-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-program-levelOne.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-p-one-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-p-one-alpha-a.

?SOURCE SCB0055A(nested-levelOne-begin-code)

?SECTION nested-sub-levelTwo-sec

 IDENTIFICATION DIVISION.
 PROGRAM-ID.  nested-sub-program-levelTwo.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.

   01 n-s-p-two-alpha-a  PIC A.

 PROCEDURE DIVISION USING n-s-p-two-alpha-a.

?SOURCE SCB0055A(nested-levelTwo-begin-code)

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

?SOURCE SCB0055A(sub-two-begin-code)
