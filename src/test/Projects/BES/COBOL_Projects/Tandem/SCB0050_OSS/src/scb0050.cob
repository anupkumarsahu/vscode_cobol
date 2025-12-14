*******************************************************************************
*
* Program: XCB50P0
*
* Purpose: The same data item names are declared in a program and in a
*          sub program.  This is useful for testing scoping/lookup
*          issues.
*
?optimize 0
?symbols
 identification division.
 program-id.  main-program.

 data division.
 working-storage section.

 01 Foo-01 PIC XX value "01".
 01 Foo-02 PIC 99 value 02.
 01 Foo-03 PIC XX value "03".
 01 Foo-04 PIC 99 value 04.

 01 Foo-Rec-01.
    05 Bar-01 PIC XX value "05".
    05 Bar-02 PIC 99 value 06.

 01 Foo-Rec-02.
    05 Bar.
        10 Muk-01 PIC XX value "07".
        10 Muk-02 PIC 99 value 08.

 01 Foo-Rec-03.
    05 Doo-01 native-2.
    05 Huh-01 native-4.

 01 Foo-Rec-04.
    05 Doo-01 native-2.
    05 Duh-01 native-8.

 01 Eek-01 PIC 9999 value 1234.

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

 01 Foo-01 PIC XX value "11".
 01 Foo-02 PIC 99 value 12.
 01 Foo-03 PIC 99 value 13.
 01 Foo-04 PIC XX value "14".

 01 Foo-Rec-01.
    05 Bar-01 PIC XX value "15".
    05 Bar-02 PIC 99 value 16.

 01 Foo-Rec-02.
    05 Bar.
        10 Muk-01 PIC XX value "17".
        10 Muk-02 PIC 99 value 18.

 01 Foo-Rec-03.
    05 Doo-01 native-2.
    05 Huh-01 native-4.

 01 Foo-Rec-04.
    05 Doo-01 native-2.
    05 Duh-01 native-8.

 01 Guk-01 PIC XXXX value "ABCD".

 linkage section.
 01 param-item-01 native-4.

 procedure division using param-item-01.
 begin-nested-sub-main-program.

   DISPLAY "Nested-Sub-Main-Program".
   DISPLAY foo-01.

 end program nested-sub-main-program.
*****************************************************************************

 end program main-program.
******************************************************************************

 identification division.
 program-id.  sub-program-one.

 data division.
 working-storage section.

 01 Foo-01 PIC XX value "21".
 01 Foo-02 PIC 99 value 22.
 01 Foo-03 PIC 99 value 23.
 01 Foo-04 PIC XX value "24".

 01 Foo-Rec-01.
    05 Bar-01 PIC XX value "25".
    05 Bar-02 PIC 99 value 26.

 01 Foo-Rec-02.
    05 Bar.
        10 Muk-01 PIC XX value "27".
        10 Muk-02 PIC 99 value 28.

 01 Foo-Rec-03.
    05 Doo-01 native-2.
    05 Huh-01 native-4.

 01 Foo-Rec-04.
    05 Doo-01 native-2.
    05 Duh-01 native-8.

 01 Guk-01 PIC XXXX value "5678".

 linkage section.
 01 param-item-02 native-4.

 procedure division using param-item-02.
 begin-sub-program-one.

   DISPLAY "Sub-Program-One".
   DISPLAY Foo-02.
   CALL sub-program-two USING param-item-02.
   CALL nested-sub-program-levelOne USING param-item-02.
   CALL nested-sub-program-levelOne USING param-item-02.

*****************************************************************************

 identification division.
 program-id.  nested-sub-program-levelOne.

 data division.
 working-storage section.

 01 Foo-01 PIC XX value "31".
 01 Foo-02 PIC 99 value 32.
 01 Foo-03 PIC 99 value 33.
 01 Foo-04 PIC XX value "34".

 01 Foo-Rec-01.
    05 Bar-01 PIC XX value "35".
    05 Bar-02 PIC 99 value 36.

 01 Foo-Rec-02.
    05 Bar.
        10 Muk-01 PIC XX value "37".
        10 Muk-02 PIC 99 value 38.

 01 Foo-Rec-03.
    05 Doo-01 native-2.
    05 Huh-01 native-4.

 01 Foo-Rec-04.
    05 Doo-01 native-2.
    05 Duh-01 native-8.

 01 Guk-01 PIC XXXX value "abcd".

 01 call-levelTwo PIC X value "F".
    88 call-levelTwo-sub-program value "T".

 linkage section.
 01 param-item-03 native-4.

 procedure division using param-item-03.
 begin-nested-sub-levelOne.

   DISPLAY "Nested-Sub-Program-LevelOne".
   DISPLAY foo-03.
   IF call-levelTwo-sub-program
      CALL nested-sub-program-levelTwo using param-item-03.

*****************************************************************************

 identification division.
 program-id.  nested-sub-program-levelTwo.

 data division.
 working-storage section.

 01 Foo-01 PIC XX value "41".
 01 Foo-02 PIC 99 value 42.
 01 Foo-03 PIC 99 value 43.
 01 Foo-04 PIC XX value "44".

 01 Foo-Rec-01.
    05 Bar-01 PIC XX value "45".
    05 Bar-02 PIC 99 value 46.

 01 Foo-Rec-02.
    05 Bar.
        10 Muk-01 PIC XX value "47".
        10 Muk-02 PIC 99 value 48.

 01 Foo-Rec-03.
    05 Doo-01 native-2.
    05 Huh-01 native-4.

 01 Foo-Rec-04.
    05 Doo-01 native-2.
    05 Duh-01 native-8.

 01 Guk-01 PIC XXXX value "WXYZ".

 linkage section.
 01 ls-inner-item native-4.

 procedure division using ls-inner-item.
 begin-nested-sub-levelTwo.

   DISPLAY "Nested-Sub-Program-LevelTwo".
   DISPLAY foo-04.

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

 01 Foo-01 PIC XX value "51".
 01 Foo-02 PIC 99 value 52.
 01 Foo-03 PIC 99 value 53.
 01 Foo-04 PIC XX value "54".

 01 Foo-Rec-01.
    05 Bar-01 PIC XX value "55".
    05 Bar-02 PIC 99 value 56.

 01 Foo-Rec-02.
    05 Bar.
        10 Muk-01 PIC XX value "57".
        10 Muk-02 PIC 99 value 58.

 01 Foo-Rec-03.
    05 Doo-01 native-2.
    05 Huh-01 native-4.

 01 Foo-Rec-04.
    05 Doo-01 native-2.
    05 Duh-01 native-8.

 01 Guk-01 PIC XXXX value "5678".

 linkage section.
 01 param-item-05 native-4.

 procedure division using param-item-05.
 begin-sub-program-two.

   DISPLAY "Sub-Program-Two".
   DISPLAY foo-04.
   perform lab-g-h-e-x.
   DISPLAY foo-04.
   DISPLAY foo-04.
   DISPLAY foo-04.

 lab-g-h-e-x.
     display "lab-g-h-e-x".
     display "lab-g-h-e-x".
     display "lab-g-h-e-x".
*****************************************************************************
 end program sub-program-two.
*****************************************************************************
