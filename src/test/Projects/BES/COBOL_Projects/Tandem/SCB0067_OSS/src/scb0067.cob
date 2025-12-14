 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01 condition-variable PIC X.
     88 condition VALUE "T".

  01 foo-var PIC XXXX.

 PROCEDURE DIVISION.
 main-start.

   DISPLAY "Start main".

** Simple IF cond THEN statement END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   END-IF

** Simple IF cond THEN statement.
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var.

** Simple IF cond THEN statement END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   END-IF

** Simple IF cond THEN statement.
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var.

** Complex IF cond THEN statement END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     PERFORM if-paragraph
   END-IF

** Complex IF cond THEN statement.
   MOVE "T" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     DISPLAY "Condition is TRUE".

** Complex IF cond THEN statement END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     PERFORM if-paragraph
   END-IF

** Complex IF cond THEN statement.
   MOVE "F" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     DISPLAY "Condition is TRUE".

** Simple IF cond THEN statement ELSE statement END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var
   END-IF

** Simple IF cond THEN statement ELSE statement.
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var.

** Simple IF cond THEN statement ELSE statement END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var
   END-IF

** Simple IF cond THEN statement ELSE statement.
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var.

** Complex IF cond THEN statement ELSE statement END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     PERFORM if-paragraph
   ELSE
     DISPLAY "Condition is FALSE"
     PERFORM else-paragraph
   END-IF

** Complex IF cond THEN statement ELSE statement.
   MOVE "T" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     DISPLAY "Condition is TRUE"
   ELSE
     PERFORM else-paragraph
     DISPLAY "Condition is FALSE".

** Complex IF cond THEN statement ELSE statement END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     PERFORM if-paragraph
   ELSE
     DISPLAY "Condition is FALSE"
     PERFORM else-paragraph
   END-IF

** Complex IF cond THEN statement ELSE statement.
   MOVE "F" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     DISPLAY "Condition is TRUE"
   ELSE
     PERFORM else-paragraph
     DISPLAY "Condition is FALSE".

** Simple IF cond THEN CONTINUE END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     CONTINUE
   END-IF

** Simple IF cond THEN CONTINUE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     CONTINUE.

** Simple IF cond THEN NEXT SENTENCE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     NEXT SENTENCE.

** Simple IF cond THEN CONTINUE END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     CONTINUE
   END-IF

** Simple IF cond THEN CONTINUE.
   MOVE "F" TO condition-variable.
   IF condition THEN
     CONTINUE.

** Simple IF cond THEN NEXT SENTENCE.
   MOVE "F" TO condition-variable.
   IF condition THEN
     NEXT SENTENCE.

** Complex IF cond THEN CONTINUE END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     MOVE "IF" TO foo-var
     CONTINUE
   END-IF

** Complex IF cond THEN CONTINUE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     CONTINUE.

** Simple IF cond THEN CONTINUE ELSE statement END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var
   END-IF

** Simple IF cond THEN CONTINUE ELSE statement.
   MOVE "T" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var.

** Simple IF cond THEN NEXT SENTENCE ELSE statement.
   MOVE "T" TO condition-variable.
   IF condition THEN
     NEXT SENTENCE
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var.

** Simple IF cond THEN CONTINUE ELSE statement END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var
   END-IF

** Simple IF cond THEN CONTINUE ELSE statement.
   MOVE "F" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var.

** Simple IF cond THEN NEXT SENTENCE ELSE statement.
   MOVE "F" TO condition-variable.
   IF condition THEN
     NEXT SENTENCE
   ELSE
     DISPLAY "Condition is FALSE"
     MOVE "ELSE" TO foo-var.

** Complex IF cond THEN CONTINUE ELSE statement END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     DISPLAY "Condition is FALSE"
     PERFORM else-paragraph
   END-IF

** Complex IF cond THEN CONTINUE ELSE statement.
   MOVE "T" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     PERFORM else-paragraph
     DISPLAY "Condition is FALSE".

** Complex IF cond THEN NEXT SENTENCE ELSE statement.
   MOVE "T" TO condition-variable.
   IF condition THEN
     NEXT SENTENCE
   ELSE
     PERFORM else-paragraph
     DISPLAY "Condition is FALSE".

** Complex IF cond THEN CONTINUE ELSE statement END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     PERFORM else-paragraph
     DISPLAY "Condition is FALSE"
   END-IF

** Complex IF cond THEN CONTINUE ELSE statement.
   MOVE "F" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     DISPLAY "Condition is FALSE"
     PERFORM else-paragraph.

** Complex IF cond THEN NEXT SENTENCE ELSE statement.
   MOVE "F" TO condition-variable.
   IF condition THEN
     NEXT SENTENCE
   ELSE
     DISPLAY "Condition is FALSE"
     PERFORM else-paragraph.

** Simple IF cond THEN statement ELSE CONTINUE END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     CONTINUE
   END-IF

** Simple IF cond THEN statement ELSE CONTINUE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     CONTINUE.

** Simple IF cond THEN statement ELSE NEXT SENTENCE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     NEXT SENTENCE.

** Simple IF cond THEN statement ELSE CONTINUE END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     CONTINUE
   END-IF

** Simple IF cond THEN statement ELSE CONTINUE.
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     CONTINUE.

** Simple IF cond THEN statement ELSE NEXT SENTENCE.
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     MOVE "IF" TO foo-var
   ELSE
     NEXT SENTENCE.

** Complex IF cond THEN statement ELSE CONTINUE END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     PERFORM if-paragraph
   ELSE
     CONTINUE
   END-IF

** Complex IF cond THEN statement ELSE CONTINUE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     DISPLAY "Condition is TRUE"
   ELSE
     CONTINUE.

** Complex IF cond THEN statement ELSE NEXT SENTENCE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     DISPLAY "Condition is TRUE"
   ELSE
     NEXT SENTENCE.

** Complex IF cond THEN statement ELSE CONTINUE END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     DISPLAY "Condition is TRUE"
   ELSE
     CONTINUE
   END-IF

** Complex IF cond THEN statement ELSE CONTINUE.
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     PERFORM if-paragraph
   ELSE
     CONTINUE.

** Complex IF cond THEN statement ELSE NEXT SENTENCE.
   MOVE "F" TO condition-variable.
   IF condition THEN
     DISPLAY "Condition is TRUE"
     PERFORM if-paragraph
   ELSE
     NEXT SENTENCE.

** Simple IF cond THEN CONTINUE ELSE CONTINUE END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     CONTINUE
   END-IF

** Simple IF cond THEN CONTINUE ELSE CONTINUE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     CONTINUE.

** Simple IF cond THEN CONTINUE ELSE NEXT SENTENCE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     NEXT SENTENCE
   ELSE
     NEXT SENTENCE.

** Simple IF cond THEN CONTINUE ELSE CONTINUE END-IF
   MOVE "F" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     CONTINUE
   END-IF

** Simple IF cond THEN CONTINUE ELSE CONTINUE.
   MOVE "F" TO condition-variable.
   IF condition THEN
     CONTINUE
   ELSE
     CONTINUE.

** Simple IF cond THEN CONTINUE ELSE NEXT SENTENCE.
   MOVE "F" TO condition-variable.
   IF condition THEN
     NEXT SENTENCE
   ELSE
     NEXT SENTENCE.

** Complex IF cond THEN CONTINUE ELSE CONTINUE END-IF
   MOVE "T" TO condition-variable.
   IF condition THEN
     MOVE "IF" TO foo-var
     CONTINUE
   ELSE
     MOVE "ELSE" TO foo-var
     CONTINUE
   END-IF

** Complex IF cond THEN CONTINUE ELSE CONTINUE.
   MOVE "T" TO condition-variable.
   IF condition THEN
     PERFORM if-paragraph
     CONTINUE
   ELSE
     PERFORM else-paragraph
     CONTINUE.

   DISPLAY "End main".
   STOP RUN.

 if-paragraph.
   MOVE "IF" TO foo-var.

 else-paragraph.
   MOVE "ELSE" TO foo-var.

 END PROGRAM main.

