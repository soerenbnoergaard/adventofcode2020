module mymodule
    type pocketdimension
        integer,dimension(-100:100, -100:100, -100:100) :: values
    end type

    contains
end module

program main
    use mymodule
    implicit none

    type(pocketdimension) :: pd
    integer :: n

    call load(pd)

    do n=1,6
        call iterate(pd)
    end do

    call num_active_cubes(pd, n)
    print *, "Num. active after boot:", n
    ! call show(pd)
end

subroutine num_active_neighbours(pd, x, y, z, n)
    use mymodule
    implicit none
    type(pocketdimension), intent(in) :: pd
    integer, intent(in) :: x, y, z
    integer, intent(out) :: n

    integer :: i, j, k

    n = 0
    do k = -1,1
        do j = -1,1
            do i = -1,1
                if (k == 0 .and. j == 0 .and. i == 0) then
                    ! Skip the current cell
                else if (pd%values(x+i, y+j, z+k) == 1) then
                    n = n + 1
                end if
            end do
        end do
    end do
    return
end

subroutine num_active_cubes(pd, n)
    use mymodule
    implicit none
    type(pocketdimension), intent(in) :: pd
    integer, intent(out) :: n

    n = sum(pd%values)
end

subroutine iterate(pd)
    use mymodule
    implicit none
    type(pocketdimension), intent(inout) :: pd
    type(pocketdimension) :: tmp

    integer :: x, y, z
    integer :: n
    integer :: here

    tmp = pd

    do z = lbound(pd%values,3)+1, ubound(pd%values,3)-1
        do y = lbound(pd%values,2)+1, ubound(pd%values,2)-1
            do x = lbound(pd%values,1)+1, ubound(pd%values,1)-1

                here = pd%values(x, y, z)
                call num_active_neighbours(pd, x, y, z, n)

                if (here == 1) then
                    if (n == 2 .or. n == 3) then
                        ! Remain active
                    else
                        tmp%values(x, y, z) = 0
                    end if

                else if (here == 0) then
                    if (n == 3) then
                        tmp%values(x, y, z) = 1
                    else
                        ! Remain inactive
                    end if
                end if
            end do
        end do
    end do

    pd = tmp
end


subroutine show(pd)
    use mymodule
    implicit none
    type(pocketdimension), intent(in) :: pd
    integer :: x, y, z
    integer :: here

    do z = lbound(pd%values,3), ubound(pd%values,3)
        print *, "z = ", z
        do y = lbound(pd%values,2), ubound(pd%values,2)
            do x = lbound(pd%values,1), ubound(pd%values,1)
                here = pd%values(x, y, z)
                if (here == 1) then
                    write (*, "(A)", advance="no") "#"
                else if  (here == 2) then
                    write (*, "(A)", advance="no") "O"
                else
                    write (*, "(A)", advance="no") "."
                end if
                ! write (*, "(I1)", advance="no") pd%values(x, y, z)
                ! print *, 
            end do
            print *, ""
        end do
    end do
end

subroutine load(output)
    use mymodule
    implicit none

    ! character(len = *), parameter :: filename = "../test_input1.txt"
    ! integer, parameter :: num_rows = 3, num_cols = 3

    character(len = *), parameter :: filename = "../puzzle_input.txt"
    integer, parameter :: num_rows = 8, num_cols = 8

    type(pocketdimension), intent(out) :: output
    integer ::  x, y, z, row, col
    character :: c
    character(len = 80) :: line

    output%values = 0

    open(1, file = filename, status = "old")

    z = 0

    do row = 1,num_rows
        read (1,*) line
        do col = 1,num_cols
            c = line(col:col)

            x = col - 1
            y = row - 1
            z = 0

            if (c .eq. "#") then
                output%values(x, y, z) = 1
            else if (c .eq. ".") then
                output%values(x, y, z) = 0
            end if


        end do
    end do
    

    close(1)

    return
end
