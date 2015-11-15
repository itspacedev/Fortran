module group_io
    use environment

    implicit none
    integer, parameter :: STUD_AMOUNT   = 8
    integer, parameter :: SURNAMES_LEN  = 15
    integer, parameter :: INITIALS_LEN  = 5

    type student
        character(SURNAMES_LEN, kind=CH_)   :: Surname  = ""
        character(INITIALS_LEN, kind=CH_)   :: Initials = ""
        character(kind=CH_)                 :: Sex      = ""
        integer(I_)                         :: Year     = 0
    end type student

contains
    
    ! Procedure create data file
    subroutine Create_data_file(input_file, data_file)
        character(*), intent(in)    ::  input_file, data_file
        type(student)               ::  stud
        integer                     :: In, Out, IO, i, recl
        character(:), allocatable   :: format

        open (file=input_file, encoding=E_, newunit=In)
            recl = (SURNAMES_LEN + INITIALS_LEN + 1) * CH_ + I_
            open (file=data_file, form='unformatted', newunit=Out, access='direct', recl=recl)
                format = '(3(a, 1x), i4)'
                do i=1,STUD_AMOUNT
                    read (In, format, iostat=IO) stud
                    call Handle_IO_status(IO, "Reading class list, line: " // i)

                    write (Out, iostat=IO, rec=i) stud
                    call Handle_IO_status(IO, "Creating unformatted file with class list, record: " // i)
                enddo
            close (Out)
        close (In)
    endsubroutine Create_data_file
    
    ! Function read data file
    function Read_class_list(data_file) result(Group)
        type(student)               Group(STUD_AMOUNT)
        character(*), intent(in)    :: data_file
        integer In, IO, recl

        recl = ((SURNAMES_LEN + INITIALS_LEN + 1) * CH_ + I_) * STUD_AMOUNT
        open (file=data_file, form='unformatted', newunit=In, access='direct', recl=recl)
            read (In, iostat=IO, rec=1) Group
            call Handle_IO_status(IO, "Reading data_file")
        close (In)
    endfunction Read_class_list

    ! Procedure write list
    subroutine Write_class_list(output_file, Group, List_name, Position)
        character(*), intent(in)    :: output_file, List_name, Position
        type(student), intent(in)   :: Group(:)

        integer                     :: Out, IO
        character(:), allocatable   :: format

        open (file=output_file, encoding=E_, position=Position, newunit=Out)
            write (Out, '(/a)') List_name
            format = '(3(a, 1x), i4)'
            write (Out, format, iostat=IO) Group
            call Handle_IO_status(IO, "Writing " // List_name)
        close (Out)
    endsubroutine Write_class_list
    
    ! Function Get avg age
    pure function Get_Gender_Avg_Age(Gender_Group, Cyear) result(Avg_Age)
        type(student), intent(in)  :: Gender_Group(:)
        integer, intent(in)        :: Cyear

        integer :: Gender_Age(Size(Gender_Group))
        integer :: Avg_Age

        Gender_Age  = Cyear -  Gender_Group%Year
        Avg_Age     = Ceiling(Real(Sum(Gender_Age) / Size(Gender_Group), R_))
    endfunction Get_Gender_Avg_Age
    
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
endmodule group_io
