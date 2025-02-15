IDENTIFICATION DIVISION.
PROGRAM-ID. CAESAR-CIPHER.

DATA DIVISION.       
LINKAGE SECTION.
* For LN-FUNC
77 VAR1 PIC 9(2).
77 VAR2 PIC 9(2).
77 RESULT PIC ZZ9.
* For LN-OBJ-FUNC
01 VAR-OBJ.
   02 OBJ-VAR1 PIC 9(2).
   02 OBJ-VAR2 PIC 9(2).
   
   
PROCEDURE DIVISION.
*    This never gets displayed
    DISPLAY "Starting multi".       
*    This is a function entry point
    ENTRY "LN-FUNC" USING VAR1, VAR2, RESULT.
    COMPUTE RESULT = VAR1 + VAR2.
*    This is the return statement
     GOBACK.
*    Another function entry point
    ENTRY "LN-OBJ-FUNC" USING VAR-OBJ, RESULT.
    COMPUTE RESULT = OBJ-VAR1 + OBJ-VAR2.
*    Alternative return statement
     EXIT PROGRAM.
END PROGRAM CAESAR-CIPHER.
