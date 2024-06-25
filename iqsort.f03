! Program: Iterative Quicksort
! Creator: Jessica Nguyen
! Date: 2024-01-29
! Purpose: Uses an iterative quicksort algorithm to sort an array of integers

program iqsort
    ! add in the modules 
    use :: stackADT
    use :: intIO

    ! declare variables
    integer, dimension(:), allocatable :: numArray
    integer :: numSize
    real :: start, end

    ! read the inputted file to obtain its unsorted integer array and array size
    call readUnsorted(numArray, numSize)
    
    ! quicksort the array iteratively (and time it as well)
    call cpu_time(start)
    call iterativeQsort(numArray, numSize)
    call cpu_time(end)
    write(*,'(/,A,f5.3,A)') "Iterative Quicksort took ", end-start, " seconds."

    ! output the sorted array into a new file
    call writeSorted(numArray, numSize)

contains

! this subroutine will quicksort an array of integers iteratively
subroutine iterativeQsort(sortArray, arraySize)
    ! declare parameter variables
    integer, dimension(:), allocatable, intent(inout) :: sortArray
    integer, intent(in) :: arraySize

    ! declare local variables
    integer :: i, j, l, r, x, w              
    logical :: result
    type(stackSettings) :: leftStack, rightStack

    ! initialize local variables
    call isEmpty(leftStack, result)
    if (result .EQV. .FALSE.) then
        call clear(leftStack)
    end if
    call isEmpty(rightStack, result)
    if (result .EQV. .FALSE.) then
        call clear(rightStack)
    end if
    call push(leftStack, 1)
    call push(rightStack, arraySize)

    do 
        ! if both stacks are empty, exit the while loop
        if ((leftStack%currentIndex .EQ. 1) .AND. (rightStack%currentIndex .EQ. 1)) exit

        ! take the top request from the stack
        call pop(leftStack, l)
        call pop(rightStack, r)

        ! partition the array from sortArray(l) to sortArray(r)
        do 
            if (l .GE. r) exit
            
            ! make temporary variables for important values (x is the middle)
            i = l
            j = r
            x = sortArray((l+r)/2)

            do 
                if (i .GT. j) exit
               
                ! uses the pivot (middle number) as comparison,
                ! finds the closest value to the left of the pivot
                do
                    if (sortArray(i) .GE. x) exit
                    i = i + 1
                end do

                ! uses the pivot (middle number) as comparison,
                ! finds the closest value to the right of the pivot
                do
                    if (x .GE. sortArray(j)) exit
                    j = j - 1
                end do
       
                ! switches the two values near the pivot
                if (i .LE. j) then
                    w = sortArray(i)
                    sortArray(i) = sortArray(j)
                    sortArray(j) = w
                    
                    ! make i and j equal to eachother
                    i = i + 1
                    j = j - 1
                end if
            end do
            
            ! if the array has more space left
            if ((j-l) .LT. (r-i)) then
                ! stack request to sort right partition
                if (i .LT. r) then
                    call push(leftStack, i)
                    call push(rightStack, r)
                end if

                ! l or r will delimit the left partition
                r = j
            else 
                if (l .LT. j) then
                    call push(leftStack, l)
                    call push(rightStack, j)
                end if

                l = i
            end if
        end do
    end do
end subroutine iterativeQsort

end program iqsort