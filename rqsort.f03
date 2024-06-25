! Program: Recursive Quicksort
! Creator: Jessica Nguyen
! Date: 2024-01-29
! Purpose: Uses a recursive quicksort algorithm to sort an array of integers

program rqsort
    ! add in the module
    use :: intIO

    ! declare variables
    integer, dimension(:), allocatable :: numArray
    integer :: numSize
    real :: start, end

    ! read the inputted file to obtain its unsorted integer array and array size
    call readUnsorted(numArray, numSize)

    ! quicksort the array recursively (and time it as well)
    call cpu_time(start)
    call recursiveQsort(numArray, 1, numSize)
    call cpu_time(end)
    write(*,'(/,A,f5.3,A)') "Recursive Quicksort took ", end-start, " seconds."

    ! output the sorted array into a new file
    call writeSorted(numArray, numSize)

contains

! this subroutine will quicksort an array of integers recursively
recursive subroutine recursiveQsort(sortArray, low, high)
    ! declare parameter variables
    integer, dimension(:), allocatable, intent(inout) :: sortArray
    integer, intent(in) :: low, high

    ! declare local variables
    integer :: pivot, tmp
    integer :: i, j

    ! initialize local variables
    pivot = sortArray(high)
    i = low - 1

    ! check if the lowest index value is less than the highest index value
    if (low .LT. high) then
        ! go through this range in the array
        do j = low, high
            ! if the current value is less than the pivot (the highest index value), 
            ! switch the current value with the previous value
            if (sortArray(j) .LT. pivot) then
                i = i + 1
                tmp = sortArray(i)
                sortArray(i) = sortArray(j)
                sortArray(j) = tmp
            end if
        end do

        ! switch the pivot (the highest index value) with the center value (i+1)
        tmp = sortArray(i+1)
        sortArray(i+1) = sortArray(high)
        sortArray(high) = tmp

        ! split up the array range into two smaller parts and put a recursive call on each
        call recursiveQsort(sortArray, low, i)
        call recursiveQsort(sortArray, (i + 2), high)
    end if
end subroutine recursiveQsort

end program rqsort