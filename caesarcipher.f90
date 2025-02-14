! Have to declare this before the main program.
function encrypt (str, shiftAmount) result(encryptedStr) ! One Compiler AI prompt helped me figure out how to shift correctly
    implicit none
    character(len=*), intent(in) :: str
    integer, intent(in) :: shiftAmount
    character(len=len(str)) :: encryptedStr
    integer :: i, asciiValue, shiftedValue

    do i = 1, len(str)
        asciiValue = iachar(str(i:i))  ! Get ASCII value of the character
        shiftedValue = asciiValue + shiftAmount

        ! Check for wrapping around alphabet
        if (shiftedValue > iachar('Z')) then
            shiftedValue = shiftedValue - 26  ! Assuming uppercase letters only
        end if

        encryptedStr(i:i) = achar(shiftedValue)  ! Convert back to character
    end do
end function encrypt

function decrypt (str, shiftAmount) result(decryptedStr) ! One Compiler AI helped rework my method of negating shiftAmount
    implicit none
    character(len=*), intent(in) :: str
    integer, intent(in) :: shiftAmount
    character(len=len(str)) :: decryptedStr

    decryptedStr = encrypt(str, -shiftAmount)  ! Reverse the shift
end function decrypt

function solve (str, maxShiftValue) result(dummy)  ! One Compiler AI helped reconstruct my function that was originally causing errors
    implicit none
    character(len=*), intent(in) :: str
    integer, intent(in) :: maxShiftValue
    integer :: i
    character(len=len(str)) :: dummy  ! Declare a dummy return variable

    ! Prints the original string
    print *, 'Original: ', str

    ! Loops through until 0
    do i = maxShiftValue, 0, -1
        print *, 'Caesar ', i, ': ', encrypt(str, i)
    end do
end function solve

! This is the main program.
program caesarcipher
    implicit none
    ! let's try to call a function.
    string :: encrypt ("HAL", 22) ! We have to declare the return type of the function.
    
    string :: decrypt
    
    string :: solve


end program caesarcipher