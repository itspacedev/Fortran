program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    !character(:), allocatable :: form
    integer                 :: In = 0, Out = 0, N = 0, i = 0, j = 0, Max = 0
    integer, allocatable    :: X(:, :)
    
    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) N
        allocate(X(N, N)) 
        read (In, *) (X(i, :),  i = 1, N)
    close (In)
    
    open (file=output_file, encoding=E_, newunit=Out)    
        write (Out, '(a, T4, "= ", i0)') "N", N
        write (Out, '(5(i0, " "))') (X(i, :), i = 1, N)
        
        do i = 1, N
            do j = 1, N
                if (Abs(X(i, j)) > Max) then
                    Max = Abs( X(i, j) )
                end if
            end do
        end do
        
        write (Out, *)
        write (Out, '(a, T7, "= ", i0)') "Max", Max
    close (Out)
        
end program program_6_1_v
