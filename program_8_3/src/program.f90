program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    integer                 :: In = 0, Out = 0, N = 0, i = 0!, tmp = 0!, j = 0!, Max = 0, MaxE = 0, MinE = 0
    integer, allocatable    :: X(:, :), X2(:, :)
    character(10)           :: format

    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate(X(N, N)) 
        read (In, *) (X(i, :),  i = 1, N)
    close (In)
    
    open (file=output_file, encoding=E_, newunit=Out)
        write (format, '(a, i0, a)') "(", N, "i3)"
        write (Out, format) (X(i, :), i = 1, N)
        
        X2 = MyFoo(X)

        write (Out, *)
        write (Out, format) (X2(i, :), i = 1, N)
    close (Out)

contains
    pure function MyFoo(A) result(B)
        integer, allocatable    :: B(:, :)
        integer, intent(in)     :: A(:, :)
        integer                 :: i, N, tmp

        N = Ubound(A, 1)
        allocate(B(N, N))
        
        B = A

        do i = 1, N
            tmp = B(i, i)
            B(i, i) = B(i, N - i + 1)
            B(i, N - i + 1) = tmp
        enddo

    end function MyFoo

end program program_6_1_v
