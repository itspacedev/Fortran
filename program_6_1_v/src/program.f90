program program_6_1_v
    implicit none
    integer, parameter      :: R_ = 8
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    integer                 :: In = 0, Out = 0, n = 0, f = 1
    real(R_)                :: x = 0, ex = 1, item = 0, new_item = 0
    
    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) x    
    close (In)
    
    n = 1
    f = 1
    item = x
    new_item = item / f 
    ex = ex + new_item
    
    open (file=output_file, encoding=E_, newunit=Out)    
    
    write (Out, *) "item_1= ", item
    write (Out, *) "ex1= ", ex 

    do
        n = n + 1
        
        item = new_item
        new_item = (item * x)
        write (Out, *) "item*x= ", new_item
        new_item = new_item / n
        write (Out, *) "item/n= ", new_item
        
        !f = f * n
        !new_item = new_item / f
        
        write (Out, *) "n= ", n
        write (Out, *) "item= ", item
        write (Out, *) "new_item= ", new_item 
        !if (new_item == item) then
        if (new_item == 0_R_) then
            exit
        else
            ex = ex + new_item
        end if
        write (Out, *) "ex=", ex
    end do     

    !open (file=output_file, encoding=E_, newunit=Out)
        write (Out, *) "n=", n
        write (Out, '(a, T7, "= ", f30.10)') "ex", ex
        write (Out, *) "error=", Exp(x) - ex
    close (Out)
end program program_6_1_v
