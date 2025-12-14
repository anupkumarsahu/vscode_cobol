*******************************************************************************
*
* Program: XCB62P0
*
* Purpose: This program does a bunch of PERFORM paragraph statements,
*          including nested PERFORMs where a paragraph PERFORMs another
*          paragraph which PERFORMs another paragraph...
*
* NOTE:  This program is identical to XCB63P0 except that it is a single
*        monolithic source file while XCB63P0 is constructed using ?SOURCE
*        statements.
*
?symbols

?SECTION main-program-sec
 identification division.
 program-id.  main-program.

 data division.
 working-storage section.

 01 main-string PIC X(80) VALUE SPACES.

 01 RESULT  PIC 9(4).

 procedure division.

 main-program-begin.

   DISPLAY "main-program-begin".

   PERFORM main-paragraph-00.

   PERFORM main-paragraph-01.

   PERFORM main-paragraph-02.

   PERFORM main-paragraph-03 THRU main-paragraph-05.

   PERFORM main-program-exit.

?SECTION main-paragraph-00-sec
 main-paragraph-00.

   MOVE "  main-paragraph-00" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-01-sec
 main-paragraph-01.

   MOVE "  main-paragraph-01" TO main-string.
   DISPLAY main-string.

   PERFORM main-paragraph-01-01.

?SECTION main-paragraph-01-01-sec
 main-paragraph-01-01.

   MOVE "    main-paragraph-01-01" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-02-sec
 main-paragraph-02.

   MOVE "  main-paragraph-02" TO main-string.
   DISPLAY main-string.

   PERFORM main-paragraph-02-01.

   PERFORM main-paragraph-02-02.

?SECTION main-paragraph-02-01-sec
 main-paragraph-02-01.

   MOVE "    main-paragraph-02-01" TO main-string.
   DISPLAY main-string.

   DISPLAY "    main-paragraph-02-01: performing main-paragraph-02-01-01".
   PERFORM main-paragraph-02-01-01.
   DISPLAY "    main-paragraph-02-01:  back from main-paragraph-02-01-01".

   DISPLAY "    main-paragraph-02-01: performing main-paragraph-02-01-02".
   PERFORM main-paragraph-02-01-02.
   DISPLAY "    main-paragraph-02-01:  back from main-paragraph-02-01-02".

   DISPLAY "    main-paragraph-02-01: performing main-paragraph-02-01-03".
   PERFORM main-paragraph-02-01-03.
   DISPLAY "    main-paragraph-02-01:  back from main-paragraph-02-01-03".

?SECTION main-paragraph-02-01-01-sec
 main-paragraph-02-01-01.

   MOVE "      main-paragraph-02-01-01" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-02-01-02-sec
 main-paragraph-02-01-02.

   MOVE "      main-paragraph-02-01-02" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-02-01-03-sec
 main-paragraph-02-01-03.

   MOVE "      main-paragraph-02-01-03" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-02-02-sec
 main-paragraph-02-02.

   MOVE "    main-paragraph-02-02" TO main-string.
   DISPLAY main-string.

   PERFORM main-paragraph-02-02-01.

   PERFORM main-paragraph-02-02-02.

?SECTION main-paragraph-02-02-01-sec
 main-paragraph-02-02-01.

   MOVE "      main-paragraph-02-02-01" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-02-02-02-sec
 main-paragraph-02-02-02.

   MOVE "      main-paragraph-02-02-02" TO main-string.
   DISPLAY main-string.

   PERFORM main-paragraph-02-02-02-01.

   MOVE "10" TO RESULT.

?SECTION main-paragraph-02-02-02-01-sec
 main-paragraph-02-02-02-01.

   MOVE "        main-paragraph-02-02-02-01" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-03-sec
 main-paragraph-03.

   MOVE "  main-paragraph-03" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-04-sec
 main-paragraph-04.

   MOVE "  main-paragraph-04" TO main-string.
   DISPLAY main-string.

?SECTION main-paragraph-05-sec
 main-paragraph-05.

   MOVE "  main-paragraph-05" TO main-string.
   DISPLAY main-string.

?SECTION main-program-exit-sec
 main-program-exit.

   MOVE "main-program-exit" TO main-string.
   DISPLAY main-string.

   STOP RUN.

 END PROGRAM main-program.
