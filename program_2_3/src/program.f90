program program_2_3
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    integer                 :: In=0, Out=0, N=0, C=0
    real(R_), allocatable   :: X(:)
    !integer, allocatable    :: X(:)
    logical, allocatable    :: Pos(:)

    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate  (X(N))
        !read (In, '(f3.0)', advance='no') X
        read (In, '(f5.8)') X
    close (In)
    
    !allocate (Pos(N), source=.false.)
    Pos = X > 0
    C = Count(Pos)
    
    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, *) X
        write (Out, *)
        write (Out, '(a, T7, "= ", i0)') 'Count', C
    close (Out)
    
end program program_2_3
