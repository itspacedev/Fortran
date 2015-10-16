program program_5_3
    implicit none
    integer, parameter          :: R_ = 4
    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    !character(:), allocatable   :: form
    integer                     :: In = 0, Out = 0, N = 0, i = 0, C = 0
    real(R_)                    :: A = 0
    logical, allocatable        :: Pos(:)
    real(R_), allocatable       :: X(:)

    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate (X(N))
        read (In, '(f5.3)') X
    close (In)
    
    Pos = X < 0
    A = Sum(X, Pos)
    C = Count(Pos)    

    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, '(f10.5)') (X(i), i = 1,N)
        write (Out, '(a, T7, "= ", f10.3)') "A", A
        write (Out, '(a, T7, "= ", i0)') "C", C
    close (Out)
end program program_5_3
