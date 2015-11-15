module group_io
    use environment

    implicit none
    integer, parameter :: SURNAMES_LEN  = 15
    integer, parameter :: INITIALS_LEN  = 5

    type student
        character(SURNAMES_LEN, kind=CH_)   :: Surname  = ""
        character(INITIALS_LEN, kind=CH_)   :: Initials = ""
        character(kind=CH_)                 :: Sex      = ""
        integer(I_)                         :: Year     = 0
        type(student), pointer              :: next     => Null()
    end type student

contains
    ! Read class list
    function Read_class_list(input_file) result(Class_List)
        type(student), pointer      :: Class_List
        character(*), intent(in)    :: input_file
        integer                     :: In

        open (file=input_file, encoding=E_, newunit=In)
            Class_List => Read_Student(In)
        close (In)
    endfunction Read_class_list
    
    ! Read student
    recursive function Read_Student(In) result(Stud)
        type(student), pointer      :: Stud
        integer, intent(in)         :: In
        integer                     :: IO
        character(:), allocatable   :: format

        allocate(Stud)
        format = '(3(a, 1x), i4)'
        read(In, format, iostat=IO) Stud%Surname, Stud%Initials, Stud%Sex, Stud%Year
        call Handle_IO_status(IO, "Reading line from file")
        if (IO == 0) then
            Stud%next => Read_Student(In)
        else
            deallocate(Stud)
            nullify(Stud)
        endif
    endfunction Read_Student

    ! Write class list
    subroutine Write_class_list(output_file, Class_List, List_name, Position)
        character(*), intent(in)    :: output_file, List_name, Position
        type(student), intent(in)   :: Class_List
        integer                     :: Out

        open(file=output_file, encoding=E_, newunit=Out, position=Position)
            write(Out, '(/a)') List_name
            call Write_student(Out, Class_List)
        close(Out)
    endsubroutine Write_class_list
    
    ! Write student
    recursive subroutine Write_student(Out, Stud)
        integer, intent(in)         :: Out
        type(student), intent(in)   :: Stud
        integer                     :: IO
        character(:), allocatable   :: format

        format = '(3(a, 1x), i4)'
        write(Out, format, iostat=IO) Stud%Surname, Stud%Initials, Stud%Sex, Stud%Year
        call Handle_IO_status(IO, "Writing student")
        if(Associated(Stud%next)) call Write_student(Out, Stud%next)
    endsubroutine Write_student
endmodule group_io
