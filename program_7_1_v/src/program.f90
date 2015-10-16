program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    character(:), allocatable :: form
    integer                 :: In = 0, Out = 0, N = 0, i = 0, MinInd = 0, tmp
    integer, allocatable    :: X(:)
    
    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate( X(N) ) 
        read (In, *) X
    close (In)
    
    open (file=output_file, encoding=E_, newunit=Out)    
        write (Out, '(a, T4, "= ", i0)') "N", N 
        form =  '(7(i0, " "))'
        write (Out, form) X
        do i=1, N-1
            !write (Out, "(i0)") X(i)
            MinInd = MinLoc(X(i:N), 1) + i-1
            if (i /= MinInd) then
                tmp = X(i)
                X(i) = X(MinInd)
                X(MinInd) = tmp
            end if
        
        end do     
    
        write (Out, form) X
    close (Out)
        
end program program_6_1_v
