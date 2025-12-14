?optimize 0,symbols
 identification division.
 program-id.  param-ptr.
 data division.
 working-storage section.
 01 di pointer.
 linkage section.
 01 pi native-2.
 01 pj native-2.
 01 px native-2.
 procedure division using pi pj.
 begin.
?innerlist
   set di to address of pi.
   set address of px to address of di.
   set address of pi to di.
   move pi to px.
?noinnerlist
   continue.
