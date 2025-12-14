 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.

* signed short
  01  mi  NATIVE-2 VALUE 22.

  01  st.
      02 sti NATIVE-2.
      02 stcom NATIVE-2.

  01  st2.
      02 st2i NATIVE-2.
      02 stcom NATIVE-2.
      02 st2j NATIVE-2.

  01  st3.
      02 st3i NATIVE-2.
      02 st3j PIC XXXX value "emma".
      02 subst3.
         03 stcom NATIVE-2.
         03 sub3i NATIVE-2.
         03 sub3j NATIVE-2.

  01  st4.
      02 st4i NATIVE-2.
      02 subst4.
         03 sub4i NATIVE-2.
         03 sub4j NATIVE-2.
      02 st4j NATIVE-2.
      02 stcom NATIVE-2.

  01  st5.
      02 st5i NATIVE-2.
      02 subst5.
         03 sub5i NATIVE-2.
         03 sub5j NATIVE-2.
         03 subst52.
            04 sub52i NATIVE-2.
            04 sub52j NATIVE-2.

 EXTENDED-STORAGE SECTION.
 PROCEDURE DIVISION.
 rec-start.

   Display "Start rec".

   Move 17 to mi.

   Move 18 to sti   of st.
   Move 19 to stcom of st.

   Move 20 to st2i  of st2.
   Move 21 to stcom of st2.
   Move 22 to st2j  of st2.

   Move 30 to st3i  of st3.
   Move 31 to stcom of subst3.
   Move 32 to sub3i of subst3.
   Move 33 to sub3j of subst3.

   Move 40 to st4i  of st4.
   Move 41 to sub4i of subst4.
   Move 42 to sub4j of subst4.
   Move 43 to st4j  of st4.
   Move 44 to stcom of st4.

   Display "End main".

 END PROGRAM main.

