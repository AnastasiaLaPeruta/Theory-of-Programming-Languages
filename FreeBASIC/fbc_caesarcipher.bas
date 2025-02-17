' caesarcipher.bas
' A Caesar cipher encryption/decryption program in FreeBASIC

#include "crt.bi"  ' For Sleep (pause) at the end

'--- Global Variables (shared across modules)
Dim Shared testString As String    ' Original input string
Dim Shared procString As String    ' Processed (uppercase, trimmed) input
Dim Shared atoz As String          ' Alphabet string
Dim Shared ztoa As String          ' Reverse alphabet string (unused in encryption)
Dim Shared stringShift As Integer  ' Shift for encryption
Dim Shared maxShift As Integer     ' Maximum label value for solving

' Initialize global variables.
Sub InitializeGlobals()
    testString   = "HAL"                             ' Original input
    atoz         = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"       ' Alphabet string
    ztoa         = "ZYXWVUTSRQPONMLKJIHGFEDCBA"        ' Reverse alphabet string
    stringShift  = 2                                 ' Shift for encryption
    maxShift     = 26                                ' We'll label our shifts from 26 down to 0
End Sub

'----------------------------------------------------------
' FUNCTION Encrypt
'
' Encrypts a string using a Caesar cipher.
' Parameters:
'   s     - the input string (assumed uppercase)
'   shift - the shift amount (e.g. 2)
' Returns:
'   The encrypted string.
'----------------------------------------------------------
Function Encrypt(s As String, shift As Integer) As String
    Dim result As String = ""
    Dim i As Integer
    Dim charPos As Integer  ' renamed from pos to avoid conflicts
    Dim index As Integer
    Dim ch As String
    For i = 1 To Len(s)
        ch = Mid(s, i, 1)
        ch = UCase$(ch)
        charPos = Instr(1, atoz, ch)  ' returns 0 if not found
        If charPos <> 0 Then
            index = charPos + shift
            If index > 26 Then index = index - 26
            result = result & Mid(atoz, index, 1)
        Else
            result = result & ch
        End If
    Next i
    Return result
End Function

'----------------------------------------------------------
' FUNCTION Decrypt
'
' Decrypts a string that was encrypted with a Caesar cipher.
' Parameters:
'   s     - the encrypted string
'   shift - the shift amount used during encryption
' Returns:
'   The decrypted string.
'----------------------------------------------------------
Function Decrypt(s As String, shift As Integer) As String
    Dim result As String = ""
    Dim i As Integer
    Dim charPos As Integer
    Dim index As Integer
    Dim ch As String
    For i = 1 To Len(s)
        ch = Mid(s, i, 1)
        charPos = Instr(1, atoz, ch)
        If charPos <> 0 Then
            index = charPos - shift
            If index < 1 Then index = index + 26
            result = result & Mid(atoz, index, 1)
        Else
            result = result & ch
        End If
    Next i
    Return result
End Function

'----------------------------------------------------------
' SUB Solve
'
' Prints all possible Caesar cipher decryptions using a computed shift:
'   currentShift = (stringShift + (26 - L)) mod 26
' for L from maxLabel down to 0.
'----------------------------------------------------------
Sub Solve(s As String, maxLabel As Integer)
    Dim L As Integer
    Dim currentShift As Integer
    For L = maxLabel To 0 Step -1
         currentShift = (stringShift + (26 - L)) mod 26
         Print "Caesar "; L; ": "; Decrypt(s, currentShift)
    Next L
End Sub

'----------------------------------------------------------
' SUB Main
'
' Main program: initializes variables, encrypts and decrypts the input,
' and prints all possible Caesar cipher solutions.
'----------------------------------------------------------
Sub Main()
    Dim encrypted As String, decrypted As String

    procString = UCase$(testString)
    procString = Trim$(procString)
    Print "Test String: "; testString
    encrypted = Encrypt(procString, stringShift)
    Print "Encrypted: "; encrypted
    decrypted = Decrypt(encrypted, stringShift)
    Print "Decrypted: "; decrypted
    Print
    Print "Trying all shifts:"
    Solve(encrypted, maxShift)
    
    Print
    Print "Press any key to exit..."
    Sleep  ' Waits for a key press
End Sub

'--- Program Entry Point ---
InitializeGlobals()
Main()
