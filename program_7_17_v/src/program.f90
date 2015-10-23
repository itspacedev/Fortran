program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    integer                 :: In = 0, Out = 0, N = 0, i = 0, j = 0!, Max = 0, MaxE = 0, MinE = 0
    integer                 :: max_val = 0, max_neg = 0, max_pos = 0, N_max = 0
    integer, allocatable    :: X(:, :), Ind(:, :), Ind_max(:, :)
    character(10)           :: format
    logical, allocatable    :: Mask_max(:, :)

    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate(X(N, N)) 
        read (In, *) (X(i, :),  i = 1, N)
    close (In)
    
    open (file=output_file, encoding=E_, newunit=Out)    
        !write (Out, '(a, T4, "= ", i0)') "N", N
    
        write (format, '(a, i0, a)') "(", N, "i3)"
        write (Out, format) (X(i, :), i = 1, N)
        
        max_neg = Abs(MinVal(X, X < 0))
        max_pos = Abs(MaxVal(X, X > 0))

        max_val = max_neg
        if (max_pos > max_val) then
            max_val = max_pos
        endif

        if (max_val > 0) then
            max_val = - max_val
        endif
        
        !write (Out, *) "max_val = ", max_val
    
        allocate ( Ind(N*N, 2) )
        Ind(:, 1) = [((i, i = 1, N), j = 1, N)]
        Ind(:, 2) = [((j, i = 1, N), j = 1, N)]


        Mask_max = ( X == max_val .OR. X == Abs(max_val) )
        N_max = Count(Mask_max)

        Ind_max = Reshape( Pack(Ind, Spread( Reshape(Mask_max, [N*N]), 2, 2)), [N_max, 2] )
        
        write (Out, *)
        write (Out, *) "Abs(max_val) = ", Abs(max_val)
        write (Out, '(2i2)') (Ind_max(i, :), i = 1, N_max) 

    close (Out)
        
end program program_6_1_v
