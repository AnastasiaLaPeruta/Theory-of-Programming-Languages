IDENTIFICATION DIVISION. 
        PROGRAM-ID. CAESAR-CIPHER.
        
        ENVIRONMENT DIVISION.
        
        DATA DIVISION.
        
        WORKING-STORAGE SECTION.
        01 testString PIC x(99) VALUE "HAL".
        01 Result PIC x(99). 
        01 newString PIC x(99).
        01 atoz PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
        01 stringLength PIC 99.
        01 stringShift PIC 99 VALUE 2.
        01 CharCount PIC 99.
        01 searchChar PIC x(1).
        01 shiftPos PIC 99.
        01 maxShift PIC 99 VALUE 26.
        01 adjustedMaxShift PIC 99.
        01 i PIC 99 VALUE 1.
        01 displayShift PIC 99.
        01 sIndex PIC 99 VALUE 1.
        01 currentShift PIC S99.
        01 n PIC 99.
        01 loopShift PIC S99.
        01 oldPos       PIC 99.



        
        PROCEDURE DIVISION. 
        
        MOVE FUNCTION UPPER-CASE(testString) TO Result.
        MOVE FUNCTION TRIM(Result) TO Result.
                
  			MOVE 0 TO stringLength.
    		INSPECT FUNCTION REVERSE(Result) TALLYING stringLength FOR LEADING SPACES.
    		COMPUTE stringLength = Length of Result - stringLength.
    			
    		DISPLAY " ".
    		DISPLAY "Test String: HAL".
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
    			DISPLAY " ".
    			DISPLAY " ".
    			DISPLAY "Solve".
    			DISPLAY " ".
    			PERFORM Solve
    			DISPLAY " ".
    			STOP RUN.
    	
    	Encrypt.
    			MOVE " " TO searchChar.
    			MOVE Result(i:i) TO searchChar.
    			MOVE 0 TO CharCount.
    			
    			INSPECT atoz TALLYING CharCount FOR CHARACTERS
          BEFORE INITIAL searchChar.
          IF CharCount < 27 
                COMPUTE shiftPos = (CharCount + 1) + stringShift
                IF shiftPos > 26
                        COMPUTE shiftPos = shiftPos - 26
                END-IF
                MOVE atoz(shiftPos:1) TO searchChar
          END-IF.

    			MOVE searchChar TO newString(i:i).
    			ADD i, 1 GIVING i.
    			DISPLAY searchChar WITH NO ADVANCING.
    	
    	
    	Decrypt.
    			MOVE " " TO searchChar.
    			MOVE newString(i:i) TO searchChar.
    			MOVE 0 TO CharCount.
    			
    			INSPECT atoz TALLYING CharCount for CHARACTERS
    				BEFORE INITIAL searchChar.
    				
    			IF CharCount < 27 
    					    COMPUTE shiftPos = (CharCount + 1) - stringShift
                        IF shiftPos < 1
                                COMPUTE shiftPos = shiftPos + 26
                        END-IF
    					    MOVE atoz(shiftPos:1) TO searchChar


    			END-IF.
    			ADD i, 1 GIVING i.
    			DISPLAY searchChar WITH NO ADVANCING.
    			
    			
    			
    			
    	Solve-Decrypt.
      MOVE FUNCTION UPPER-CASE(Result(sIndex:1)) TO searchChar.
      MOVE 0 TO CharCount.
      INSPECT atoz TALLYING CharCount FOR CHARACTERS BEFORE INITIAL searchChar.
      IF CharCount >= 0
           COMPUTE oldPos = CharCount + 1
           COMPUTE shiftPos = FUNCTION MOD((oldPos - 1) + (currentShift - 26), 26) + 1
           MOVE atoz(shiftPos:1) TO searchChar
      END-IF.
      DISPLAY searchChar WITH NO ADVANCING.







    Solve.
      PERFORM VARYING loopShift FROM 26 BY -1 UNTIL loopShift < 0
           MOVE loopShift TO currentShift
           COMPUTE displayShift = currentShift
           DISPLAY "Caesar " displayShift ": " WITH NO ADVANCING
           MOVE 1 TO sIndex
           PERFORM Solve-Decrypt VARYING sIndex FROM 1 BY 1 UNTIL sIndex > stringLength
           DISPLAY " "
      END-PERFORM.



IDENTIFICATION DIVISION. 
        PROGRAM-ID. CAESAR-CIPHER.
        
        ENVIRONMENT DIVISION.
        
        DATA DIVISION.
        
        WORKING-STORAGE SECTION.
        01 testString PIC x(99) VALUE "HAL".
        01 Result PIC x(99). 
        01 newString PIC x(99).
        01 atoz PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
        01 stringLength PIC 99.
        01 stringShift PIC 99 VALUE 2.
        01 CharCount PIC 99.
        01 searchChar PIC x(1).
        01 shiftPos PIC 99.
        01 maxShift PIC 99 VALUE 26.
        01 adjustedMaxShift PIC 99.
        01 i PIC 99 VALUE 1.
        01 displayShift PIC 99.
        01 sIndex PIC 99 VALUE 1.
        01 currentShift PIC S99.
        01 n PIC 99.
        01 loopShift PIC S99.
        01 oldPos PIC 99.

        
        PROCEDURE DIVISION. 
        
        MOVE FUNCTION UPPER-CASE(testString) TO Result.
        MOVE FUNCTION TRIM(Result) TO Result.
                
  			MOVE 0 TO stringLength.
    		INSPECT FUNCTION REVERSE(Result) TALLYING stringLength FOR LEADING SPACES.
    		COMPUTE stringLength = Length of Result - stringLength.
    			
    		DISPLAY " ".
    		DISPLAY "Test String: HAL".
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
    			    DISPLAY " ".
    			    DISPLAY " ".
    			    DISPLAY "Solve".
    			    DISPLAY " ".
    			    PERFORM Solve
    			        DISPLAY " ".
    			        STOP RUN.
    	
    	  Encrypt.
    		MOVE " " TO searchChar.
    		MOVE Result(i:i) TO searchChar.
    		MOVE 0 TO CharCount.
    			
    		INSPECT atoz TALLYING CharCount FOR CHARACTERS
        BEFORE INITIAL searchChar.
        IF CharCount < 27 
              COMPUTE shiftPos = (CharCount + 1) + stringShift
              IF shiftPos > 26
                    COMPUTE shiftPos = shiftPos - 26
              END-IF
              MOVE atoz(shiftPos:1) TO searchChar
        END-IF.

    		MOVE searchChar TO newString(i:i).
    		ADD i, 1 GIVING i.
    	  DISPLAY searchChar WITH NO ADVANCING.
    	
    	
    	  Decrypt.
    		MOVE " " TO searchChar.
    		MOVE newString(i:i) TO searchChar.
    		MOVE 0 TO CharCount.
    			
    		INSPECT atoz TALLYING CharCount for CHARACTERS
    		BEFORE INITIAL searchChar.
    				
    		IF CharCount < 27 
    			    COMPUTE shiftPos = (CharCount + 1) - stringShift
              IF shiftPos < 1
                    COMPUTE shiftPos = shiftPos + 26
              END-IF
    					    MOVE atoz(shiftPos:1) TO searchChar
    		END-IF.
    		ADD i, 1 GIVING i.
    		DISPLAY searchChar WITH NO ADVANCING.
    			
    			
        Solve-Decrypt.
        MOVE FUNCTION UPPER-CASE(Result(sIndex:1)) TO searchChar.
        MOVE 0 TO CharCount.
        INSPECT atoz TALLYING CharCount FOR CHARACTERS BEFORE INITIAL searchChar.
        IF CharCount >= 0
              COMPUTE oldPos = CharCount + 1
              COMPUTE shiftPos = FUNCTION MOD((oldPos - 1) + (currentShift - 26), 26) + 1
              MOVE atoz(shiftPos:1) TO searchChar
        END-IF.
        DISPLAY searchChar WITH NO ADVANCING.


        Solve.
        PERFORM VARYING loopShift FROM 26 BY -1 UNTIL loopShift < 0
             MOVE loopShift TO currentShift
             COMPUTE displayShift = currentShift
             DISPLAY "Caesar " displayShift ": " WITH NO ADVANCING
             MOVE 1 TO sIndex
             PERFORM Solve-Decrypt VARYING sIndex FROM 1 BY 1 UNTIL sIndex > stringLength
                  DISPLAY " "
             END-PERFORM.



