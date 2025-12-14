*******************************************************************************
*
* Program: XCB43P0
*
* Purpose: The same data item names are declared in a program and in a
*          "nested" sub program.  This is useful for testing scoping/lookup
*          issues.
*
?optimize 0
?symbols
 identification division.
 program-id.  main-program-unit.

 data division.
 working-storage section.

* Test 1: upper bound in same record
 01 Rec-A.
    05 Item-A pic 99 value 1.
    05 Data-A pic X occurs 1 to 10 times
       depending on Item-A value "A".

* Test 2: upper bound in outer item
 01 Rec-B.
    05 Data-B pic X occurs 1 to 10 times
       depending on Item-C value "B".
 01 Item-C pic 99 value 3.

* Test 3: upper bound in record item of other record
 01 Rec-D.
    05 Data-D pic X occurs 1 to 10 times
       depending on Item-E value "D".

 01 Rec-E.
    05 Other-Data pic xx.
    05 Item-E pic 99 value 5.

 01 Foo-01 PIC XX value "01".
 01 Foo-02 PIC 99 value 02.
 01 Foo-Rec-01.
    05 Bar-01 PIC XX value "03".
    05 Bar-02 PIC 99 value 04.

 01 Foo-Rec-02.
    05 Bar.
        10 Muk-01 PIC XX value "05".
        10 Muk-02 PIC 99 value 06.

* Test 4: upper bound in linkage section
 01 outer-item native-4 value 2.

 procedure division.
 begin.

    call sub-program-1 using outer-item.
    stop run.

******************************************************************************

    identification division.
    program-id.  sub-program-1.

    data division.
    working-storage section.

    01 Foo-01 PIC XX value "11".
    01 Foo-02 PIC 99 value 12.
    01 Foo-Rec-01.
       05 Bar-01 PIC XX value "13".
       05 Bar-02 PIC 99 value 14.

    01 Foo-Rec-02.
       05 Bar.
           10 Muk-01 PIC XX value "15".
           10 Muk-02 PIC 99 value 16.

    01 Muk-01 PIC XXXX value "1234".

    01 array-inner-member-group.
       02 array-ls-member pic x occurs 1 to 10 times
          depending on ls-inner-item value "A".

    linkage section.
    01 ls-inner-item native-4.

    procedure division using ls-inner-item.
    begin-inner.

      display foo-01.

    end program sub-program-1.
*****************************************************************************
 end program main-program-unit.
