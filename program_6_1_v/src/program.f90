program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 16
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    integer                 :: In = 0, Out = 0, f = 1, n = 0
    real(R_)                :: x = 0, ex = 1, item = 0, new_item = 0
    
    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) x    
    close (In)
    
    n = 1
    item = x / f
    new_item = item
    ex = ex + new_item
    n = n + 1    
    
    open (file=output_file, encoding=E_, newunit=Out)    

    do
        f = f * n
        item = new_item
        new_item = (item * x) / f
        n = n + 1
        
        write (Out, *) "item=", item
        write (Out, *) "new_item=", new_item 
        if (new_item == item) &
            exit
    end do     

    !open (file=output_file, encoding=E_, newunit=Out)
        write (Out, *) "n=", n
        write (Out, '(a, T7, "= ", f30.10)') "ex", ex
        write (Out, *) "error=", Exp(x) - ex
    close (Out)
end program program_6_1_v
