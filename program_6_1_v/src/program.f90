program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    integer                 :: In = 0, Out = 0, n = 0, f = 1
    real(R_)                :: x = 0, ex = 1, ex2 = 0, item = 0, new_item = 0
    
    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) x    
    close (In)
    
    n = 1
    f = 1
    item = x
    new_item = item / f 
    ex = ex + new_item
    ex2 = ex

    open (file=output_file, encoding=E_, newunit=Out)

        do
            n = n + 1
        
            item = new_item
            new_item = (item * x) / n
            write (Out, '(a, T7, " ", f8.4)') "item", new_item
        
            ex2 = ex + new_item

            if (ex2 == ex) then
                exit
            else
                ex = ex2
            end if

            write (Out, '(a, T7, " ", f8.4)') "Exp", ex
        end do 

        write (Out, *) "n=", n
        write (Out, '(a, T7, "= ", f8.4)') "Exp", ex
        write (Out, *) "error=", Exp(x) - ex
    close (Out)
end program program_6_1_v
