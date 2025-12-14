*******************************************************************************
*
* Program: XCB45P0
*
* Purpose: The same data item names are declared in a sub program unit
*          and in a sub program unit that is nested within the it.  This
*          is useful for testing scoping/lookup issues.
*
?symbols
 identification division.
 program-id.  main-program-unit.

 data division.
 working-storage section.

 01 outer-item native-4 value 2.

 procedure division.
 begin.

    call sub-program-one using outer-item.
    stop run.

 end program main-program-unit.

******************************************************************************

  identification division.
  program-id.  sub-program-one.

  data division.
  working-storage section.
  01 array-inner-member-group.
     02 array-ls-member pic x occurs 1 to 10 times
        depending on ls-inner-item value "A".

  linkage section.
  01 ls-inner-item native-4.

  procedure division using ls-inner-item.
  begin-inner.

    display ls-inner-item.
    display "Sub-Program-One".
    call sub-program-two using ls-inner-item.

******************************************************************************

  identification division.
  program-id.  sub-program-two.

  data division.
  working-storage section.
  01 array-inner-member-group.
     02 array-ls-member pic x occurs 1 to 10 times
        depending on ls-inner-item value "A".

  linkage section.
  01 ls-inner-item native-4.

  procedure division using ls-inner-item.
  begin-inner.

    display ls-inner-item.
    display "Sub-Program-Two".

  end program sub-program-two.
*****************************************************************************
  end program sub-program-one.
