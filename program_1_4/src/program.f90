program program_1_4
    implicit none
    integer, parameter          :: R_ = 8;
    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
    character(:), allocatable      :: form
    integer                     :: In = 0, Out = 0
    real(R_)                    :: a = 0, ln_a = 0, item = 0, item_2, p = 0.693147

    open (file=input_file, encoding=E_, newunit=In)
        read (In, *) a
    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        form = "(a, T7, '= ', f0.2)"
        
        write (Out, form) "a", a
        write (Out, form)
    
        item    = a / (4 + a)
        item_2  = item * item
        ln_a    = item

        write (Out, form) "item", item

        item    = (item * item_2) / 3
        ln_a    = ln_a + item
        
        write (Out, form) "item", item
        
        item    = ((item * item_2) * 3) / 5
        ln_a    = p + 2 * (ln_a + item)        
        
        write (Out, form) "item", item
        write (Out, form) "ln", ln_a
        write (Out, form) "Error", log(2+a) - ln_a  
    close (Out)
end program program_1_4
