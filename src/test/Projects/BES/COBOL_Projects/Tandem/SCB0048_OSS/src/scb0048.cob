*******************************************************************************
*
* Program: XCB48P0
*
* Purpose: The same data item names are declared in a program and in a
*          sub program and nested sub programs.  Some of the data items
*          are declared GLOBAL.  This is useful for testing scoping
*          issues involving global data items.
*
?symbols
 identification division.
 program-id.  main-program.

 data division.
 working-storage section.

 01 param-item-00 native-4 value 2.

 procedure division.
 begin-main-program.

    DISPLAY "Main-Program".
    CALL nested-sub-main-program using param-item-00.
    CALL sub-program-one using param-item-00.
    CALL sub-program-two using param-item-00.
    STOP RUN.

*****************************************************************************

 identification division.
 program-id.  nested-sub-main-program.

 data division.
 working-storage section.

 linkage section.
 01 param-item-01 native-4.

 procedure division using param-item-01.
 begin-nested-sub-main-program.

   DISPLAY "Nested-Sub-Main-Program".

 end program nested-sub-main-program.
*****************************************************************************

 end program main-program.
******************************************************************************

 identification division.
 program-id.  sub-program-one.

 data division.
 working-storage section.

 01 Data-01 PIC X(20) value "Data-01 SP-1 GL     " GLOBAL.
 01 Data-02 PIC X(20) value "Data-02 SP-1 GL     " GLOBAL.
 01 Data-03 PIC X(20) value "Data-03 SP-1 NG     ".
 01 Data-04 PIC X(20) value "Data-04 SP-1 NG     ".
 01 Data-05 PIC X(20) value "Data-05 SP-1 NG     ".
 01 Data-06 PIC X(20) value "Data-06 SP-1 GL     " GLOBAL.
 01 Data-09 PIC X(20) value "Data-09 SP-1 NG     ".
 01 Data-10 PIC X(20) value "Data-10 SP-1 GL     " GLOBAL.
 01 Data-11 PIC X(20) value "Data-11 SP-1 GL     " GLOBAL.
 01 Data-12 PIC X(20) value "Data-12 SP-1 GL     " GLOBAL.

 linkage section.
 01 param-item-02 native-4.

 procedure division using param-item-02.
 begin-sub-program-one.

   DISPLAY "Sub-Program-One".

   DISPLAY Data-01.
   DISPLAY Data-02.
   DISPLAY Data-03.
   DISPLAY Data-04.
   DISPLAY Data-05.
   DISPLAY Data-06.
   DISPLAY Data-09.
   DISPLAY Data-10.
   DISPLAY Data-11.
   DISPLAY Data-12.

   CALL nested-sub-program-levelOne USING param-item-02.

*****************************************************************************

 identification division.
 program-id.  nested-sub-program-levelOne.

 data division.
 working-storage section.

 01 Data-01 PIC X(20) value "Data-01 NSP-L1 NG   ".
 01 Data-02 PIC X(20) value "Data-02 NSP-L1 GL   " GLOBAL.
 01 Data-03 PIC X(20) value "Data-03 NSP-L1 NG   ".
 01 Data-04 PIC X(20) value "Data-04 NSP-L1 GL   " GLOBAL.
 01 Data-07 PIC X(20) value "Data-07 NSP-L1 NG   ".
 01 Data-08 PIC X(20) value "Data-08 NSP-L1 GL   " GLOBAL.
 01 Data-11 PIC X(20) value "Data-11 NSP-L1 NG   ".
 01 Data-12 PIC X(20) value "Data-12 NSP-L1 GL   " GLOBAL.

 01 call-levelTwo PIC X value "F".
    88 call-levelTwo-sub-program value "T".

 linkage section.
 01 param-item-03 native-4.

 procedure division using param-item-03.
 begin-nested-sub-levelOne.

   DISPLAY "Nested-Sub-Program-LevelOne".

   DISPLAY Data-01.
   DISPLAY Data-02.
   DISPLAY Data-03.
   DISPLAY Data-04.
   DISPLAY Data-06.
   DISPLAY Data-07.
   DISPLAY Data-08.
   DISPLAY Data-10.
   DISPLAY Data-11.
   DISPLAY Data-12.

   CALL nested-sub-program-levelTwo using param-item-03.

*****************************************************************************

 identification division.
 program-id.  nested-sub-program-levelTwo.

 data division.
 working-storage section.

 01 Data-01 PIC X(20) value "Data-01 NSP-L2 NG   ".
 01 Data-02 PIC X(20) value "Data-02 NSP-L2 NG   ".
 01 Data-05 PIC X(20) value "Data-05 NSP-L2 NG   ".
 01 Data-06 PIC X(20) value "Data-06 NSP-L2 NG   ".
 01 Data-09 PIC X(20) value "Data-09 NSP-L2 GL   " GLOBAL.
 01 Data-12 PIC X(20) value "Data-12 NSP-L2 GL   " GLOBAL.

 linkage section.
 01 ls-inner-item native-4.

 procedure division using ls-inner-item.
 begin-nested-sub-levelTwo.

   DISPLAY "Nested-Sub-Program-LevelTwo".

   DISPLAY Data-01.
   DISPLAY Data-02.
   DISPLAY Data-04.
   DISPLAY Data-05.
   DISPLAY Data-06.
   DISPLAY Data-08.
   DISPLAY Data-09.
   DISPLAY Data-10.
   DISPLAY Data-11.
   DISPLAY Data-12.

 end program nested-sub-program-levelTwo.
*****************************************************************************

 end program nested-sub-program-levelOne.
*****************************************************************************

 end program sub-program-one.
*****************************************************************************

 identification division.
 program-id.  sub-program-two.

 data division.
 working-storage section.

 linkage section.
 01 param-item-05 native-4.

 procedure division using param-item-05.
 begin-sub-program-two.

   DISPLAY "Sub-Program-Two".

 end program sub-program-two.
*****************************************************************************
