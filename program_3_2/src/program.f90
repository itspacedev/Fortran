program program_3_2
    implicit none
    integer, parameter          :: R_=4
    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    character(:), allocatable   :: form
    integer                     :: In=0, Out=0, m=1, K=0, i=0
    integer, allocatable        :: MAS(:) 
    
    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) K
    close (In)
 
    open (file=output_file, encoding=E_, newunit=Out)
        form = "(a, T7, '= ', i0)"
        
        write (Out, form) "K", K

        eval: block
            if (K > 0) then
                allocate (MAS(K))
                forall (i = 1:K) &
                   MAS(i) = i
                m = PRODUCT(MAS)
            else if (K == 0) then
                m = 1
            else
                write (Out, *) "incorrect K"
                exit eval
            end if
            write (Out, form) "m", m
        end block eval 
    close (Out)
    
end program program_3_2
