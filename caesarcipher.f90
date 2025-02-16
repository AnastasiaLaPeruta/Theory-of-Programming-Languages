! This is the main program, One Compiler AI helped call my functions better than I had set up initially and ChatGPT helped work out displaying output
program caesarcipher
    implicit none
    character(len=100) :: encryptedStr, decryptedStr
    character(len=100) :: originalStr
    integer :: maxShiftValue
    character(len=100) :: dummy  

    interface
        function encrypt(str, shiftAmount) result(encryptedStr) 
            ! One Compiler AI prompt helped me figure out how to shift correctly
            character(len=*), intent(in) :: str
            integer, intent(in) :: shiftAmount
            character(len=len(str)) :: encryptedStr
        end function encrypt

        function decrypt(str, shiftAmount) result(decryptedStr) 
            ! One Compiler AI helped rework my method of negating shiftAmount
            character(len=*), intent(in) :: str
            integer, intent(in) :: shiftAmount
            character(len=len(str)) :: decryptedStr
        end function decrypt

        function solve(str, maxShiftValue) result(dummy)  
            ! One Compiler AI helped reconstruct my function that was originally causing errors
            character(len=*), intent(in) :: str
            integer, intent(in) :: maxShiftValue
            character(len=len(str)) :: dummy
        end function solve
    end interface

    ! Assigns a string and max shift value
    originalStr = "OPERATION DAYBREAK"
    maxShiftValue = 2  ! Shifting by 2

    ! Call the functions and store the results
    encryptedStr = encrypt(originalStr, maxShiftValue)
    print *, 'Encrypted: ', trim(encryptedStr)  

    decryptedStr = decrypt(encryptedStr, maxShiftValue)
    print *, 'Decrypted: ', trim(decryptedStr)

    ! Call the solve function
    dummy = solve(originalStr, 26)
end program caesarcipher

! Function to encrypt a string (only A-Z, wraps properly)
function encrypt(str, shiftAmount) result(encryptedStr)
    implicit none
    character(len=*), intent(in) :: str
    integer, intent(in) :: shiftAmount
    character(len=len(str)) :: encryptedStr
    integer :: i, charCode, effectiveShift

    effectiveShift = mod(shiftAmount, 26)  ! Wrap shifts properly

    do i = 1, len(str)
        if (str(i:i) >= "A" .and. str(i:i) <= "Z") then
            charCode = ichar(str(i:i)) + effectiveShift
            if (charCode > ichar("Z")) then
                charCode = charCode - 26  ! Wrap around A-Z
            end if
            encryptedStr(i:i) = char(charCode)
        else
            encryptedStr(i:i) = str(i:i)  ! Keep spaces unchanged
        end if
    end do
end function encrypt

! Function to decrypt a string (reverses shift properly)
function decrypt(str, shiftAmount) result(decryptedStr)
    implicit none
    character(len=*), intent(in) :: str
    integer, intent(in) :: shiftAmount
    character(len=len(str)) :: decryptedStr
    integer :: i, charCode, effectiveShift

    effectiveShift = mod(shiftAmount, 26)  ! Wrap shifts properly

    do i = 1, len(str)
        if (str(i:i) >= "A" .and. str(i:i) <= "Z") then
            charCode = ichar(str(i:i)) - effectiveShift
            if (charCode < ichar("A")) then
                charCode = charCode + 26  ! Wrap around A-Z
            end if
            decryptedStr(i:i) = char(charCode)
        else
            decryptedStr(i:i) = str(i:i)  ! Keep spaces unchanged
        end if
    end do
end function decrypt

! Function to try all possible shifts (correct order & formatting)
function solve(str, maxShiftValue) result(dummy)
    implicit none
    character(len=*), intent(in) :: str
    integer, intent(in) :: maxShiftValue
    character(len=len(str)) :: dummy
    integer :: i
    character(len=100) :: shiftedStr

    interface
        function encrypt(str, shiftAmount) result(encryptedStr)
            character(len=*), intent(in) :: str
            integer, intent(in) :: shiftAmount
            character(len=len(str)) :: encryptedStr
        end function encrypt
    end interface

    print *, 'Trying all shifts:'
    do i = maxShiftValue, 0, -1  ! Prints from 26 down to 0
        shiftedStr = encrypt(str, i)
        print '(A, I2, A, A)', 'Caesar ', i, ': ', trim(shiftedStr)
    end do

    dummy = "Done"
end function solve