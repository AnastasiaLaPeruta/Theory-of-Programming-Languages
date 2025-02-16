FUNCTION encrypt () AS STRING
    Dim searchChar As String = " "
    searchChar = Mid(result, i, i)
    Dim charCount As Integer = 0

    ' suggested by ChatGPT
    charCount = INSTR(atoz, searchChar) - 1

    IF charCount < 27 THEN
        Dim shiftPos As Integer = (charCount + 1) + stringShift
        IF shiftPos > 26 THEN
            shiftPos = shiftPos - 26
        END IF
        searchChar = Mid(atoz, shiftPos, 1)
    END IF

    ' declare type before using
    Dim newString As String = ""

    Mid(newString, i, i) = searchChar
    i = i + 1
    PRINT searchChar
END FUNCTION

    


FUNCTION decrypt ()
    searchChar = " "
    searchChar = Mid(newString, i, i)
    charCount = 0

    ' suggested by ChatGPT
    CharCount = INSTR(atoz, searchChar) - 1

    IF charCount < 27 THEN
        shiftPos = (charCount + 1) - stringShift
        IF shiftPos < 1 THEN
            shiftPos = shiftPos + 26
        END IF
        searchChar = Mid(atoz, shiftPos, 1)
    END IF

    i = i + 1
    PRINT searchChar
  
END FUNCTION




FUNCTION solveDecrypt()
    searchChar =  UCASE$(Mid(result, sIndex, 1))
    charCount = 0

    ' suggested by ChatGPT
    CharCount = INSTR(atoz, searchChar) - 1
    
    IF charCount >= 0 THEN
        oldPos = CharCount + 1
        ' appropriate mod function suggested by ChatGPT
        shiftPos = (((oldPos - 1) + (currentShift - 26)) MOD 26) + 1 
        searchChar = Mid(atoz, shiftPos, 1)
    END IF

    PRINT searchChar

END FUNCTION

FUNCTION solve()
    FOR loopShift = 26 TO 0 STEP -1
        currentShift = loopShift
        PRINT "Caesar "; displayShift; ": "
        sIndex = 1
        FOR sIndex = 1 TO sIndex > stringLength
            solveDecrypt()
        NEXT
        PRINT " "
    NEXT 
END FUNCTION



SUB Main()
    Dim testString As String = "HAL"
    Dim atoz As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    Dim stringShift As Integer = 2
    ' fixed length string of one char suggested by ChatGPT
    Dim searchChar As String * 1
    Dim maxShift As Integer = 26
    Dim i As Integer = 1
    Dim sIndex As Integer = 1

    Dim result As String = UCASE$(testString)
    result = TRIM$(result)

    Dim stringLength As Integer = 0
    stringLength = LEN(TRIM$(Result))

    PRINT " "
    PRINT "Test String: HAL"
    PRINT " "
    PRINT "Encrypt"
    FOR i = 1 TO stringLength
        encrypt()
    NEXT
    Dim i As Integer = 1
    PRINT " "
    PRINT "Decrypt"
    FOR i = 1 TO stringLength
        decrypt()
    NEXT
    PRINT " "
    Dim adjustedMaxShift As Integer = maxShift
    adjustedMaxShift += 1
    PRINT " "
    PRINT " "
    PRINT "Solve"
    PRINT " "
    solve()
    PRINT " "
END SUB
