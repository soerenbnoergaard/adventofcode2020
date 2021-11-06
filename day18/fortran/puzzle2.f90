module mymodule
end module

program main
    use mymodule
    implicit none

    call load()
end

subroutine load()
    implicit none

    ! character(len = *), parameter :: filename = "../test_input4.txt"
    ! integer(kind=8), parameter :: num_lines = 5

    character(len = *), parameter :: filename = "../puzzle_input.txt"
    integer(kind=8), parameter :: num_lines = 373

    integer(kind=8) :: i, n, nsum
    character(len = 256) :: line

    nsum = 0

    open(1, file = filename, status = "old")

    do i = 1,num_lines
        read (1,"(A256)") line
        call evaluate(line, n)
        ! print *, TRIM(line), " = ", n
        nsum = nsum + n
    end do
    
    close(1)

    print *, "Sum of homework:", nsum

    return
end

subroutine str2int(str, int, stat)
    implicit none
    character(len = *), intent(in) :: str
    integer(kind=8), intent(out) :: int
    integer(kind=8), intent(out) :: stat

    read(str,*,iostat=stat) int
end

logical function gpt(o1, o2)
    ! Returns true if o1 has greater precedance than o2

    implicit none
    character, intent(in) :: o1, o2

    if ((o1 == '+') .and. (o2 == '*')) then
        gpt = .true.
    else if ((o1 == '*') .and. (o2 == '+')) then
        gpt = .false.
    else
        gpt = .true.
    end if
end function

subroutine evaluate(line, res)
    implicit none

    character(len = *), intent(in) :: line
    integer(kind=8), intent(out) :: res

    logical :: gpt
    integer :: i, j, k
    character :: token
    integer(kind=8) :: token_int, stat

    ! Shunting-yard algorithm
    character, dimension(0:100) :: output_queue
    character, dimension(0:100) :: operator_stack
    integer(kind=8) :: i_output
    integer(kind=8) :: i_operator
    character :: o1, o2

    ! Reset stack/queue index (-1 = empty)
    i_output = -1
    i_operator = -1

    ! PARSE INPUT TOKENS
    do i = 1,len(line)
        token = line(i:i)
        call str2int(token, token_int, stat)

        if (token == ' ') then
            ! WHITESPACE

        else if (stat == 0) then
            ! NUMBER

            ! Append to output queue
            i_output = i_output + 1
            output_queue(i_output) = token

        else if (token == '(') then
            ! LEFT PARENTHESIS

            ! Push to operator stack
            i_operator = i_operator + 1
            operator_stack(i_operator) = token

        else if (token == ')') then
            ! RIGHT PARENTHESIS

            do while (operator_stack(i_operator) /= '(')
                if (i_operator < 0) then
                    print *, "Operator stack is empty"
                    call EXIT(1)
                end if

                ! Pop the operator into the output queue
                o1 = operator_stack(i_operator)
                i_operator = i_operator - 1

                i_output = i_output + 1
                output_queue(i_output) = o1
            end do

            ! Pop the left parethesis from the operator stack and discard it
            if ((i_operator >= 0) .and. (operator_stack(i_operator) /= '(')) then
                print *, "Missing start parenthesis"
                call EXIT(1)
            end if
            i_operator = i_operator - 1

        else
            ! OPERATOR

            o1 = token
            do while ((i_operator >= 0) .and. (operator_stack(i_operator) /= '(') .and. (gpt(operator_stack(i_operator), o1)))
                ! Pop from operator stack into output queue
                o2 = operator_stack(i_operator)
                i_operator = i_operator - 1

                i_output = i_output + 1
                output_queue(i_output) = o2
            end do

            ! Push o1 to the operator stack
            i_operator = i_operator + 1
            operator_stack(i_operator) = o1
        end if
    end do

    ! NO MORE INPUT TOKENS
    do while (i_operator >= 0)
        o1 = operator_stack(i_operator)

        if (o1 == '(') then
            print *, "Mismatched parentheses"
            call EXIT(1)
        end if

        ! Pop the operator into the output queue
        i_output = i_output + 1
        output_queue(i_output) = o1

        i_operator = i_operator - 1
    end do

    ! SEND LISTING TO THE RPN CALCULATOR TO GET THE RESULT
    call rpn_calculator(output_queue(0:i_output), i_output, res)
end


subroutine rpn_calculator(listing, length, res)
    implicit none
    character, dimension(0:*), intent(in) :: listing
    integer(kind=8), intent(out) :: length
    integer(kind=8), intent(out) :: res

    character :: token
    integer(kind=8) :: token_int, stat, i

    ! RPN calculator
    integer(kind=8), dimension(0:10) :: stack
    integer(kind=8) :: sp
    integer(kind=8) :: x, y

    sp = -1

    do i = 0, length
        token = listing(i)
        call str2int(token, token_int, stat)

        if (stat == 0) then
            ! NUMBER
            sp = sp + 1
            stack(sp) = token_int

        else
            ! OPERATOR
            x = stack(sp)
            sp = sp - 1

            y = stack(sp)
            sp = sp - 1

            if (token == '+') then
                x = x + y
            else if (token == '*') then
                x = x * y
            end if

            sp = sp + 1
            stack(sp) = x
        end if
    end do

    ! print *, "Listing: ", listing(0:length)
    ! print *, "Stack:   ", stack(0:sp)
    res = stack(sp)
end
