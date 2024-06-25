! Program: Stack Functions
! Creator: Jessica Nguyen
! Date: 2024-01-29
! Purpose: Holds a stack structure and has stack functions like pop, push, clear, and isempty

module stackADT
    ! declare implicit type
    implicit none

    ! create a stack structure
    type :: stackSettings
        integer, dimension(100) :: stack
        integer :: currentIndex = 1
        integer :: stackSize = 100
    end type

contains

    ! this subroutine removes the top number from the stack
    subroutine pop(stack, poppedNum)
        ! declare implicit type
        implicit none

        ! declare parameter variables
        type(stackSettings), intent(inout) :: stack
        integer, intent(inout) :: poppedNum

        ! get the popped number, clear the value from the stack, and decrease the index
        if (stack%currentIndex .GT. 1) then
            poppedNum = stack%stack(stack%currentIndex)
            stack%stack(stack%currentIndex) = 0
            stack%currentIndex = stack%currentIndex - 1
        end if
    end subroutine pop

    ! this subroutine adds a new number to the top of the stack
    subroutine push(stack, pushedNum)
        ! declare implicit type
        implicit none

        ! declare parameter variables
        type(stackSettings), intent(inout) :: stack
        integer, intent(in) :: pushedNum

        ! if the index isn't at max size, push a number to the top of the stack
        if (stack%currentIndex .LT. stack%stackSize) then
            stack%currentIndex = stack%currentIndex + 1
            stack%stack(stack%currentIndex) = pushedNum
        end if
    end subroutine push

    ! this subroutine clears/empties the entire stack
    subroutine clear(stack)
        ! declare implicit type
        implicit none

        ! declare parameter variable
        type(stackSettings), intent(inout) :: stack

        ! clear the entire stack
        stack%stack = 0
    end subroutine clear

    ! this subroutine checks if the stack is empty or not
    subroutine isEmpty(stack, answer)
        ! declare implicit type
        implicit none

        ! declare parameter variables
        type(stackSettings), intent(inout) :: stack
        logical, intent(inout) :: answer

        ! is the stack currently empty (index=1)? 
        ! returns a logical value
        answer = (stack%currentIndex .EQ. 1)
    end subroutine isEmpty

end module stackADT