program program_4_1_v
    implicit none
    integer, parameter      :: R_ = 4
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    integer                 :: In = 0, Out = 0, N = 0, i = 0
    real(R_)                :: x1 = 0, x2 = 0, h = 0
    real(R_), allocatable   :: X(:), F(:)

    open (file=input_file, encoding=E_, newunit=In)
        read (In, '(f6.5)') x1, x2, h
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, '(3(a, T4, "= ", f6.5/))') "x1", x1, "x2", x2, "h", h
    close (Out)

    N = Int((x2 - x1) / h + 0.5) + 1

    allocate (X(N), F(N))
    forall (i = 1:N) &
        X(i) = x1 + h*(i-1)
    F = X * X * TAN(X) + SIN(X) / X
    
    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write (Out, '(a, T8, "| ", a)') "X", "Y"
        write (Out, '(f6.5, T8, "| ", f10.9)') (X(i), F(i), i = 1,N)
    close (Out)
end program program_4_1_v
