* COPILOT helped me write this code with automatic code completion
IDENTIFICATION DIVISION.
PROGRAM-ID. CAESAR-CIPHER.

DATA DIVISION.
* CHATGPT helped me figure out which storage section to use and what variables belonged here
WORKING-STORAGE SECTION.
* Defines persistent variables here
    01 ORIGINAL-STR PIC X(100) VALUE "OPERATION DAYBREAK".
    01 ENCRYPTED-STR PIC X(100).
    01 DECRYPTED-STR PIC X(100).
    01 MAX-SHIFT-VALUE PIC 99 VALUE 2.
    01 DUMMY PIC X(100).
    01 EFFECTIVE-SHIFT PIC 99.
    01 SHIFTAMOUNT PIC 99.
    01 STR PIC X(100).
    01 RESULT PIC 999.
    01 I PIC 99 VALUE 1.

PROCEDURE DIVISION.
*    This never gets displayed
    DISPLAY "Starting multi".      

*    This is a function entry point
    ENTRY "ENCRYPT" USING STR, SHIFTAMOUNT, ENCRYPTED-STR.
    COMPUTE EFFECTIVE-SHIFT = SHIFTAMOUNT MOD 26.

    PERFORM VARYING I FROM 1 BY 1 UNTIL I = LEN(STR)
        IF STR(I:1) >= "A" AND STR(I:1) <= "Z" THEN
            COMPUTE RESULT = ICHAR(STR(I:1)) + EFFECTIVE-SHIFT
            IF RESULT > ICHAR("Z") THEN
                COMPUTE RESULT = RESULT - 26
            END-IF
            MOVE CHAR(RESULT) TO ENCRYPTED-STR(I:1)
        ELSE
            MOVE STR(I:1) TO ENCRYPTED-STR(I:1)
        END-IF
    END-PERFORM
*    This is the return statement
    GOBACK.
   
   
     
*    This is a function entry point
    ENTRY "DECRYPT" USING STR, SHIFTAMOUNT, DECRYPTED-STR.
    COMPUTE EFFECTIVE-SHIFT = MAX-SHIFT-VALUE - SHIFTAMOUNT MOD 26.

    PERFORM VARYING I FROM 1 BY 1 UNTIL I = LEN(STR)
        IF STR(I:1) >= "A" AND STR(I:1) <= "Z" THEN
            COMPUTE RESULT = ICHAR(STR(I:1)) - EFFECTIVE-SHIFT
            IF RESULT < ICHAR("A") THEN
                COMPUTE RESULT = RESULT + 26
            END-IF
            MOVE CHAR(RESULT) TO DECRYPTED-STR(I:1)
        ELSE
            MOVE STR(I:1) TO DECRYPTED-STR(I:1)
        END-IF
    END-PERFORM
*    This is the return statement
    GOBACK.     
     
     
     
*    Another function entry point
    ENTRY "SOLVE" USING STR, MAX-SHIFT-VALUE, DUMMY.
    DISPLAY "Trying all shifts:"
    PERFORM VARYING I FROM MAX-SHIFT-VALUE BY -1 UNTIL I = 0
        DISPLAY "CAESAR " I ":"
        CALL "ENCRYPT" USING STR, I, DUMMY
        DISPLAY DUMMY
    END-PERFORM
*    Alternative return statement
    EXIT PROGRAM.
     
END PROGRAM CAESAR-CIPHER.
