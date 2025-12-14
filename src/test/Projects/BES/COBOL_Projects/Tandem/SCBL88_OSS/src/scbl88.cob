* Paymentech mentioned a level-88 in Linkage Section
* could not be displayed by Native Inspect.
?symbols
 identification division.
 program-id.  main.
 data division.
 working-storage section.
 01 ws-rec.
     05 ws-item-1 native-4.
        88 ws-item-1-ok value 0 thru 10.
     05 ws-item-2 native-4.
        88 ws-item-2-ok value 10 thru 20.
 procedure division.
 begin.
    move 10 to ws-item-1 ws-item-2.
    call callee using ws-rec.
    display ws-item-1 " " ws-item-2.
 end program main.
 identification division.
 program-id. callee.
 data division.
 linkage section.
 01 ls-rec.
     05 ls-item-1 native-4.
        88 ls-item-1-ok value 0 thru 10.
     05 ls-item-2 native-4.
        88 ls-item-2-ok value 10 thru 20.
 procedure division using ls-rec.
 begin.
   if ls-item-1-ok
      display "item 1 ok"
   end-if
   if ls-item-2-ok
      display "item 2 ok"
   end-if
   move 20 to ls-item-1 ls-item-2.
 end program callee.
*end program main.
