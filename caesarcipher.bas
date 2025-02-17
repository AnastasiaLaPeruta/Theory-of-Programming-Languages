Option Explicit

' Global variable declarations
Dim testString As String         ' Original input string
Dim Result As String             ' Processed (uppercase, trimmed) input
Dim newString As String          ' Encrypted string
Dim atoz As String               ' Alphabet string
Dim ztoa As String               ' Reverse alphabet string
Dim stringShift As Integer       ' Shift for encryption
Dim maxShift As Integer          ' Maximum shift value

' Initialize global variables
testString = "HAL"                                 ' Original input
atoz = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"               ' Alphabet string
ztoa = "ZYXWVUTSRQPONMLKJIHGFEDCBA"                ' Reverse alphabet string
stringShift = 2                                    ' Shift for encryption
maxShift = 26                                      ' Maximum shift value

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
    Dim result As String
    result = ""
    Dim i As Integer, pos As Integer, index As Integer
    Dim ch As String
    For i = 1 To Len(s)
        ch = Mid(s, i, 1)
        ch = UCase(ch)
        pos = InStr(1, atoz, ch)
        If pos <> 0 Then
            index = pos + shift
            If index > 26 Then
                index = index - 26
            End If
            result = result + Mid(atoz, index, 1)
        Else
            result = result + ch
        End If
    Next i
    Encrypt = result
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
    Dim result As String
    result = ""
    Dim i As Integer, pos As Integer, index As Integer
    Dim ch As String
    For i = 1 To Len(s)
        ch = Mid(s, i, 1)
        pos = InStr(1, atoz, ch)
        If pos <> 0 Then
            index = pos - shift
            If index < 1 Then
                index = index + 26
            End If
            result = result + Mid(atoz, index, 1)
        Else
            result = result + ch
        End If
    Next i
    Decrypt = result
End Function

'----------------------------------------------------------
' SUB Solve
'
' Prints all possible Caesar cipher decryptions by trying all shifts.
' Parameters:
'   s        - the encrypted string
'   maxShift - the maximum shift value to try (typically 26)
'----------------------------------------------------------
Sub Solve(s As String, maxShift As Integer)
    Dim shift As Integer
    For shift = maxShift To 0 Step -1
        Print "Caesar "; shift; ": "; Decrypt(s, shift)
    Next shift
End Sub

'----------------------------------------------------------
' SUB Main
'
' Main program: initializes variables, encrypts and decrypts the input,
' and prints all possible Caesar cipher solutions.
'----------------------------------------------------------
Sub Main()
    Dim encrypted As String, decrypted As String

    Result = UCase(testString)
    Result = Trim(Result)
    Print "Test String: "; testString
    encrypted = Encrypt(Result, stringShift)
    Print "Encrypted: "; encrypted
    decrypted = Decrypt(encrypted, stringShift)
    Print "Decrypted: "; decrypted
    Print
    Print "Solve:"
    Solve(encrypted, maxShift)
    Sleep  ' Pause before ending
End Sub

Main
