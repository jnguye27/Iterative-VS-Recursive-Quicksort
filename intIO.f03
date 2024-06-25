! Program: File Input and Output
! Creator: Jessica Nguyen
! Date: 2024-01-29
! Purpose: Obtains user input to read an unsorted integer list from a file
!          Writes a sorted integer list to a new (or old) file

module intIO
    ! declare implicit type
    implicit none

contains

    ! this subroutine will prompt for a file containing numbers to be sorted
    ! returns the collected integer array and array size
    subroutine readUnsorted (unsortedArray, arraySize)
        ! declare implicit type
        implicit none

        ! declare parameter variables
        integer, dimension(:), allocatable, intent(inout) :: unsortedArray
        integer, intent(inout) :: arraySize

        ! declare local variables
        integer, dimension(:), allocatable :: tmpArray
        character (len=1000) :: fileName
        logical :: fileExist
        integer :: EOF, number, maxSize

        ! allocate and initialize the array
        maxSize = 1000
        allocate(unsortedArray(maxSize))
        unsortedArray = 0

        ! prompt the user for any file containing numbers to be sorted
        write (*,'(A)') "Enter a file containing numbers to be sorted:"
        read (*,*) fileName

        ! loop this process until it's valid
        do
            ! check if the file exists
            inquire (file=fileName, exist=fileExist)

            ! if it does exist then read the file into an array, if it doesn't then provide an error and retry
            if (fileExist .EQV. .TRUE.) then
                ! open the file
                open (unit=10, file=fileName, status='old', action='read')

                ! read it onto an array and obtain the array size
                arraySize = 0
                do 
                    ! read in the number 
                    read (10, *, iostat=EOF) number

                    ! if this is the last line, exit
                    if (EOF .NE. 0) exit

                    ! if we're at the end of the array, re-allocate it to a bigger size
                    if (arraySize .EQ. maxSize) then
                        allocate(tmpArray(maxSize * 2))
                        tmpArray(1:maxSize) = unsortedArray(1:maxSize)
                        deallocate(unsortedArray)
                        maxSize = maxSize * 2
                        allocate(unsortedArray(maxSize))
                        unsortedArray = tmpArray
                        deallocate(tmpArray)
                    end if

                    ! add the file number to the array
                    arraySize = arraySize + 1
                    unsortedArray(arraySize) = number
                end do

                ! close the file and exit
                close(10)
                exit
            else
                ! provide an error message and allow them to re-enter another file
                write (*,'(/,A)') "This file does not exist."
                write (*,'(A)') "Enter a different file containing numbers to be sorted:"
                read (*,*) fileName
            end if
        end do
    end subroutine readUnsorted

    ! this subprogram will write the sorted numbers to a file named sortedNUM.txt
    subroutine writeSorted (sortedArray, arraySize)
        ! declare implicit type
        implicit none

        ! declare parameter variables
        integer, dimension(:), allocatable, intent(inout) :: sortedArray
        integer, intent(in) :: arraySize

        ! declare local variables
        logical :: fileExist
        integer :: i, response

        ! check if the file exists
        inquire (file='sortedNUM.txt', exist=fileExist)

        ! if it does then ask to replace the file, if it doesn't then make a new one
        if (fileExist .EQV. .TRUE.) then
            ! prompt if they would like their file to be overwritten
            write (*,'(/,A)') "Would you like to overwrite 'sortedNUM.txt' (1 = Yes, 2 = No)?"
            read (*,*) response
            do
                ! 1 = overwrite the current file, 2 = don't overwrite the current file, any other number = error
                if (response .EQ. 1) then
                    ! open the file
                    open (unit=20, file='sortedNUM.txt', status='replace', action='write')

                    ! write it onto a file (one per line)
                    do i = 1, arraySize-1
                        write (20,'(I0)') sortedArray(i)
                    end do
                    write (20,'(I0,$)') sortedArray(arraySize)

                    ! tell the user where to find it
                    write (*,'(/,A,/)') "The sorted numbers were overwritten to 'sortedNUM.txt'."

                    ! close the file and exit
                    close(20)
                    exit
                else if (response .EQ. 2) then
                    write (*,'(/,A,/)') "The sorted numbers were not overwritten to 'sortedNUM.txt'."
                    exit
                else
                    write (*,'(/,A)') "Ths input is invalid."
                    write (*,'(A)') "Would you like to overwrite 'sortedNUM.txt' (1 = Yes, 2 = No)?"
                    read (*,*) response
                end if
            end do
        else
            ! open the file
            open (unit=20, file='sortedNUM.txt', status='new', action='write')
  
            ! write it onto a file (one per line)
            do i = 1, arraySize-1
                write (20,'(I0)') sortedArray(i)
            end do
            write (20,'(I0,$)') sortedArray(arraySize)

            ! close the file
            close(20)

            ! tell the user where to find it
            write (*,'(/,A,/)') "The sorted numbers were saved to 'sortedNUM.txt'."
        end if

        ! deallocate the array
        deallocate(sortedArray)
    end subroutine writeSorted

end module intIO