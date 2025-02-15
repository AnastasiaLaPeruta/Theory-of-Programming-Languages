        IDENTIFICATION DIVISION.
        PROGRAM-ID. CAESAR-CIPHER.
        
        ENVIRONMENT DIVISION.
        
        DATA DIVISION.
        
        WORKING-STORAGE SECTION.
        01 newChar PIC x(1).
        01 testString PIC x(99) VALUE "HAL".
        01 Result PIC x(99). 
        01 newString PIC x(99).
        01 abc PIC x(99) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
        01 cba PIC x(99) VALUE "ZYXWVUTSRQPONMLKJIHGFEDCBA".
        01 stringLength PIC 99.
        01 stringShift PIC 99 VALUE 2.
        01 CharCount PIC 99.
        01 searchChar PIC x(1).
        01 shiftPos PIC 99.
        01 maxShift PIC 99 VALUE 26.
        01 adjustedMaxShift PIC 99.
        01 i PIC 99 VALUE 1.
        
        PROCEDURE DIVISION.    
                MOVE FUNCTION UPPER-CASE(testString) TO Result.
                MOVE FUNCTION TRIM(Result) TO Result.
                
  				MOVE 0 TO stringLength.
    			INSPECT FUNCTION REVERSE(Result) TALLYING stringLength FOR LEADING SPACES.
    			COMPUTE stringLength = Length of Result - stringLength.
    			
    			DISPLAY " ".
    			DISPLAY "Universal Test String: HAL".
    			DISPLAY "Encrypt and Decrypt shift amount: 2".
    			DISPLAY "Solve max shift: 26".
    			DISPLAY " ".
    			DISPLAY "Encrypt".
    			PERFORM Encrypt stringLength TIMES
    			MOVE 1 TO i.
    			DISPLAY " ".
    			DISPLAY "Decrypt".
    			PERFORM Decrypt stringLength TIMES
    			DISPLAY " ".
    			MOVE maxShift TO adjustedMaxShift.
    			ADD adjustedMaxShift, 1 GIVING adjustedMaxShift
    			DISPLAY "Solve".
    			PERFORM Solve adjustedMaxShift TIMES
    			DISPLAY " ".
    			STOP RUN.
    	
    	Encrypt.
    			MOVE " " TO searchChar.
    			MOVE Result(i:i) TO searchChar.
    			MOVE 1 TO CharCount.
    			
    			INSPECT abc TALLYING CharCount for CHARACTERS
    				BEFORE INITIAL searchChar.
    				
    			IF CharCount < 27 
    					  COMPUTE shiftPos = (CharCount + 1) + stringShift
                        IF shiftPos > 26
                                COMPUTE shiftPos = shiftPos - 26
                        END-IF
    					  MOVE abc(shiftPos:1) TO searchChar

    			END-IF.
    			MOVE searchChar TO newString(i:i).
    			ADD i, 1 GIVING i.
    			DISPLAY searchChar WITH NO ADVANCING.
    	
    	Decrypt.
    			MOVE " " TO searchChar.
    			MOVE newString(i:i) TO searchChar.
    			MOVE 1 TO CharCount.
    			
    			INSPECT abc TALLYING CharCount for CHARACTERS
    				BEFORE INITIAL searchChar.
    				
    			IF CharCount < 27 
    					    COMPUTE shiftPos = (CharCount + 1) - stringShift
                        IF shiftPos < 1
                                COMPUTE shiftPos = shiftPos + 26
                        END-IF
    					    MOVE abc(shiftPos:1) TO searchChar


    			END-IF.
    			ADD i, 1 GIVING i.
    			DISPLAY searchChar WITH NO ADVANCING.
    			
    	Solve-Decrypt.
    			MOVE " " TO searchChar.
    			MOVE Result(i:i) TO searchChar.
    			MOVE 1 TO CharCount.
    			
    			INSPECT cba TALLYING CharCount for CHARACTERS
    				BEFORE INITIAL searchChar.
    				
    			IF CharCount < 27 
    					ADD maxShift, CharCount GIVING shiftPos
    					IF FUNCTION MOD(shiftPos, 26) IS NOT ZERO 		    				
    							MOVE FUNCTION MOD(shiftPos, 26) to shiftPos
    							MOVE cba(shiftPos:1) TO searchChar

    					ELSE 
    						MOVE "A" TO searchChar
    					END-IF
    			END-IF.
    			ADD i, 1 GIVING i.
    			DISPLAY searchChar WITH NO ADVANCING.
    	
    	Solve.
    			MOVE 1 TO i.
    			PERFORM Solve-Decrypt stringLength TIMES
    			SUBTRACT 1 FROM maxShift GIVING maxShift.
    			DISPLAY " ".
    			DISPLAY " ".
    			