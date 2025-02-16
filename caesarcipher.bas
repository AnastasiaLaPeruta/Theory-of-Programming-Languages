FUNCTION encrypt (str AS STRING, shiftAmount AS INTEGER, encryptedStr AS STRING) AS STRING
    searchChar = " "
    searchChar
    
    
    effectiveShift = shiftAmount MOD 26
    FOR i AS INTEGER = 1 TO LEN(str)
        ' syntax assisted by ChatGPT 
        ' extracts 1 character substring
        IF MID$(str, i, 1) >= "A" AND MID$(str, i, 1) <= "Z" THEN
            result = ASC(MID$(str, i, 1)) + effectiveShift
            IF result > ASC("Z") THEN
            result = result - 26
            END IF
            ' modifies characters of encryptedStr
            MID$(encryptedStr, i, 1) = Chr(result)
        ELSE 
            MID$(encryptedStr, i, 1) = MID$(str, i, 1)
        END IF
    RETURN (encryptedStr)
END FUNCTION


FUNCTION decrypt (str AS STRING, shiftAmount AS INTEGER, decryptedStr AS STRING) AS STRING
    effectiveShift = maxShiftValue - shiftAmount MOD 26
    FOR i AS INTEGER = 1 TO LEN(str)
    IF MID$(str, i, 1) >= "A" AND MID$(str, i, 1) <= "Z" THEN
        result = ASC(MID$(str, i, 1)) - effectiveShift
        IF result < ASC("A") THEN
            result = result + 26
        END IF
        ' result replaces character in string
        MID$(decryptedStr, i, 1) = Chr(result)
    ELSE
        MID$(decryptedStr, i, 1) = MID$(str, i, 1)
    END IF
    RETURN (decryptedStr)
END FUNCTION


FUNCTION solve(str AS STRING, maxShiftValue AS INTEGER, dummy AS STRING) AS STRING
    PRINT "Trying all shifts:"
    FOR i AS INTEGER = maxShiftValue TO 0 BY -1
        PRINT "CAESAR " + i + ":"
        dummy = encrypt(str, i, dummy)
        PRINT dummy
    NEXT
    RETURN dummy
END FUNCTION

SUB Main()
   'sample string
    str = "HAL"
   'maximum shift value
    maxShiftValue = 2
   'dummy variable
    dummy = ""
    PRINT "Original String: " + str
END SUB
