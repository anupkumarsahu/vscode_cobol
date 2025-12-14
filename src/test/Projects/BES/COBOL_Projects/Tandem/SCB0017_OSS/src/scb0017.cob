 IDENTIFICATION DIVISION.
 PROGRAM-ID. main.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

  01 ss  NATIVE-2 VALUE 22.

 PROCEDURE DIVISION.
 main-start.
* This label will cause "ambiguous reference" in inspect.
* lab.
     display "main-start".
     perform lab-one.

* Inspect syntax can be BREAK sun.lab or BREAK lab of sun.
     perform lab of sun.
     perform lab of moon.

*    perform lab.  "ambiguous procedure reference"
     perform sun.
     perform moon.

 lab-one.
     display "lab-one".
     Move 17 to ss.

 sun section.
 lab.
     display "sunlight".
     Move 22 to ss.

 moon section.
 lab.
     display "moonlight".
     Move 44 to ss.

* lab and lab2 are not siblings
* labels can have a parent but no siblings.
 lab2.
     display "moonlight".
     Move 45 to ss.

* lab-two gets a parent symbol (stars) even though it is not ambiguous
 stars section.
 lab-two.
     display "starlight".
     Move 66 to ss.
