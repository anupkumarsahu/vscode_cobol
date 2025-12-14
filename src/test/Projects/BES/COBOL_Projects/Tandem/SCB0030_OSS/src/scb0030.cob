* file given to me by Paul Robinson.
* he noticed that generated code did not update _perform_block
* after step in.  Remember, DU depends on _perform_block to find
* return address.
?symbols
?optimize 1
* ?innerlist
 identification division.
 program-id.  perf-opt-test.
 procedure division.
 begin.
   perform doit.
 doit.
   display "hello".
