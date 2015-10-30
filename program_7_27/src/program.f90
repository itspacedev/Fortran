program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    integer                 :: In = 0, Out = 0, N = 0, i = 0!, j = 0!, Max = 0, MaxE = 0, MinE = 0
    integer, allocatable    :: X(:, :), tmp( :)
    character(10)           :: format

    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate(X(N, N)) 
        read (In, *) (X(i, :),  i = 1, N)
    close (In)
    
    open (file=output_file, encoding=E_, newunit=Out)    
        allocate(tmp( N))
        
        do concurrent (i=1:N:2)
            tmp = X(i, :)
            X(i, :) = X(i+1, :)
            X(i+1, :) = tmp
        enddo

        write (format, '(a, i0, a)') "(", N, "i3)"
        write (Out, format) (X(i, :), i = 1, N)
        

    close (Out)
        
end program program_6_1_v
