program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    !character(:), allocatable :: form
    integer                 :: In = 0, Out = 0, N = 0, i = 0, C = 0
    integer, allocatable    :: A(:, :), X(:), Y(:)!, C(:)
    character(10)           :: format
    
    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate(A(N, N)) 
        read (In, *) (A(i, :), i = 1, N)
        
        allocate(X(N))
        read (In, *) X

        allocate(Y(N))
        read (In, *) Y
    close (In)
    
    open (file=output_file, encoding=E_, newunit=Out)    
        write (format, '(a, i0, a)') "(", N, "i0)"
        write (Out, format) (A(i, :), i = 1, N)
        
        C = Dot_product(X, Matmul(A, Y))
        
        write (Out, *)
        write (Out, '(a, T4, "= ", i0)') "C", C
        !C = []
        !write (Out, *)
        !write (Out, format) A
        !write (Out, format) B

        !C = [ Dot_product(X, Matmul(A, B))]
        
        !write (Out, *)
        !write (Out, *) C
    close (Out)
        
end program program_6_1_v
