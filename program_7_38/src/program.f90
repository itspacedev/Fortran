program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    !character(:), allocatable :: form
    integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
    integer, allocatable    :: A(:, :), B(:, :), C(:, :)
    
    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate( A(N, 1) ) 
        read (In, *) A(:, 1)

        read( In, *) M
        allocate( B(1, M) )
        read (In, *) B(1, :)
    close (In)
    
    open (file=output_file, encoding=E_, newunit=Out)    
        !write (Out, '(a, T4, "= ", i0)') "N", N
        write (Out, *)
        write (Out, '(5(i0, " "))') A(1, :)
        !write (Out, *)
        !write (Out, '(a, T4, "= ", i0)') "M", M
        !write (Out, *)
        !write (Out, '(3(i0, " "))') B
        write (Out, *)

        allocate( C(M, N) )
        C = Matmul(A, B)
        !forall(i = 1:M) &
        !    C(i, :) = A * B(i)

        write (Out, *)
        write (Out, '(5(i0, " "))') (C(i, :), i = 1, M)
             
    close (Out)
        
end program program_6_1_v
