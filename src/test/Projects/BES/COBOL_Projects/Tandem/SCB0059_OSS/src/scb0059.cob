******************************************************************************
* File:     SCB0059
* Synopsis: Copy library statement inclusion test
* Author:   Seth Hawthorne
*
* Details:  This test case tests different cases of including lines from
*                       copy libraries (CLIBCLB1 and CLIBCLB2). This includes
*           the following cases:
*              o Program line # same as line included from copylib
*              o Range of copylib lines included multiple times
*              o Program included same line # from two different copylibs
*
*           Note: Line numbers are important so observe relevant comments
*              Line numbers   1-100  common to CLIBCLB1 and CLIBCLB2
*              Line numbers 100-199  unique to CLIBCLB1
*              Line numbers 200-299  are common program and CLIBCLB1
*              Line numbers 300-...  are unique to the program
*
* Copyright 2006 (c) Hewlett Packard Inc. All Rights Reserved
*
******************************************************************************















































































































































































*##############################################################################
* Lines 200-... program lines
*##############################################################################
 IDENTIFICATION DIVISION.
 PROGRAM-ID. Copylib-statements.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

?SOURCE CLIBCLB1( DEFINE-DATA )

  01 alphanumeric-X160 PIC A(160) VALUE "123456789+123456789+123456789+123456789+123456789+
-                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwx
-                                       "yxz COBOL string literals are a maximum of 160 cha
-                                       "racters.!!".

 PROCEDURE DIVISION.
 main-start.

   Display "-----------------------------------------------------------------".
   Display "Native Inspect COBOL Test: Copylib-Statements".
   Display "-----------------------------------------------------------------".
   Perform Test-SingleInclusion.
   Perform Test-SameRanges
   Perform Test-MultipleInclusion.
   Perform Test-TwoClibs.
   Perform Test-Point.
   Perform Test-OutOfOrder.
   Display "End test".
   Stop Run.
 Test-Initialize.



******************************************************************************
* Test-SameRanges
*     Test inclusion of line ranges from two copy libraries which have the same
*     same numbers as this range!
*
******************************************************************************
* Display statements must begin at line 240
 Test-SameRanges.
   DISPLAY " ".
   DISPLAY "Test-SameRanges: Begin".
   DISPLAY "  program line: 240".
   DISPLAY "  program line: 241".
   DISPLAY "  program line: 242".
?SOURCE CLIBCLB1( RANGE-240-245 )
   DISPLAY "  program line: 244".
   DISPLAY "  program line: 245".
   DISPLAY "  program line: 246".
?SOURCE CLIBCLB2( RANGE-240-247 )
   DISPLAY "Test-SameRanges: End".









































*#############################################################################
* Lines 300-... Program unique
*#############################################################################

******************************************************************************
* Test-TwoClibs
*     Test inclusion of the same line range from two different copy libraries.
*
******************************************************************************
 Test-TwoClibs.
   DISPLAY " ".
   DISPLAY "Test-TwoClibs: Begin".
?SOURCE CLIBCLB1( RANGE-25-30 )
?SOURCE CLIBCLB2( RANGE-25-30 )
   DISPLAY "Test-TwoClibs: End".

******************************************************************************
* Test-SingleInclusion
*   Test single inclusion of CLIBCLB1 unique line range
******************************************************************************
 Test-SingleInclusion.
  DISPLAY " ".
  DISPLAY "Test-SingleInclusion: Begin".
?SOURCE CLIBCLB1( RANGE-110-112 )
  DISPLAY " ".
  DISPLAY "Test-SingleInclusion: End".


******************************************************************************
 Test-MultipleInclusion.
  DISPLAY " ".
  DISPLAY "Test-MultipleInclusion: Begin".
?SOURCE CLIBCLB1( RANGE-110-112 )
  DISPLAY " ".
  DISPLAY " Repeat display".
  DISPLAY " ".
?SOURCE CLIBCLB1( RANGE-110-112 )
  DISPLAY "Test-MultipleInclusion: End".


******************************************************************************
 Test-OutOfOrder.
  DISPLAY " ".
  DISPLAY "Test-OutOfOrder: Begin".
?SOURCE CLIBCLB1( RANGE-125 )
?SOURCE CLIBCLB1( RANGE-122 )
?SOURCE CLIBCLB1( RANGE-119 )
?SOURCE CLIBCLB1( RANGE-116 )

?SOURCE CLIBCLB1( RANGE-125 )
?SOURCE CLIBCLB1( RANGE-119 )
?SOURCE CLIBCLB1( RANGE-122 )
?SOURCE CLIBCLB1( RANGE-116 )
?SOURCE CLIBCLB1( RANGE-125 )
  DISPLAY "Test-OutOfOrder: End".


******************************************************************************
 Test-Point.

 END PROGRAM Copylib-statements.
