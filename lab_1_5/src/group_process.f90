module group_process
    use environment
    use group_io

    implicit none

contains

    ! Get list by gender
    pure recursive subroutine Get_list_by_gender(Stud, List, Gender)
        type(student), intent(in)       :: Stud
        type(student), pointer          :: List
        character(kind=CH_), intent(in) :: Gender

        if(Stud%Sex == Gender) then
            allocate(List, source=Stud)
            List%next => Null()

            if(Associated(Stud%next)) &
                call Get_list_by_gender(Stud%next, List%next, Gender)

        elseif(Associated(Stud%next)) then
            call Get_list_by_gender(Stud%next, List, Gender)
        endif
    endsubroutine Get_list_by_gender
    
    ! Get Avg Age
    pure recursive function Get_Avg_Age(Stud, N, Cyear, Sum_Age) result(Avg_Age)
        type(student), intent(in)       :: Stud
        integer, intent(in)             :: N, Cyear, Sum_Age
        integer                         :: Summa, Avg_Age

        if(Associated(Stud%next)) then
            Summa   = Sum_Age + (Cyear - Stud%Year)
            Avg_Age = Get_Avg_Age(Stud%next, N+1, Cyear, Summa)
        else
            Avg_Age = Ceiling(Real((Sum_Age + (Cyear - Stud%Year)) / N, R_))
        endif
    endfunction Get_Avg_Age
    
 
    ! Get Postfix
    pure function Get_Postfix(Avg_Age) result(Postfix)
        integer, intent(in)          :: Avg_Age 
        character(:), allocatable    :: Postfix

        select case (Mod(Avg_Age, 10))
            case(1)
                Postfix = "год"
            case(2:4)
                Postfix = "года"
            case default
                Postfix = "лет"
        endselect
    endfunction Get_Postfix

 
    ! Write Avg Age
    subroutine Write_Avg_Age(output_file, str, Avg_Age, Position)
        character(*), intent(in)    :: output_file, str, Position
        integer, intent(in)         :: Avg_Age
        
        character(:), allocatable   :: format
        integer                     :: Out, IO

        open (file=output_file, encoding=E_, newunit=Out, position=Position)
            format = '(/a, 1x, a, 1x, i0, 1x, a)'
            write (Out, format, iostat=IO) "Средний возраст", str, Avg_Age, Get_Postfix(Avg_Age)
            call Handle_IO_status(IO, "Writing avg age")
        close (Out)
    endsubroutine Write_Avg_Age
end module group_process 
