program lab_1_2
    use environment

    implicit none

    integer, parameter                  ::  STUD_AMOUNT = 8, SURNAMES_LEN = 15, INITIALS_LEN = 5
    !character(kind=CH_)                 ::  MALE = Char(Int(z'041C'), CH_)
    character(:), allocatable           ::  input_file, output_file

    character(kind=CH_)                 ::  Surnames(STUD_AMOUNT, SURNAMES_LEN) = "", &
                                            Initials(STUD_AMOUNT, INITIALS_LEN) = "", &
                                            Genders(STUD_AMOUNT) = ""

    !character(kind=CH_), allocatable    :: Boys_Surnames(:, :), Boys_Initials(:, :)
    integer                  :: Years(STUD_AMOUNT)

    input_file  = "../data/input.txt"
    output_file = "output.txt"
    
    ! Read data
    call Read_class_list(input_file, Surnames, Initials, Genders, Years)
    
    ! Write data
    call Write_class_list(output_file, Surnames, Initials, Genders, Years, "Исходный список", "rewind")

contains

    ! Procedre Read
    subroutine Read_class_list(input_file, Surnames, Initials, Genders, Years)
        character(*)        input_file
        character(kind=CH_) Surnames(:, :), Initials(:, :), Genders(:)
        integer             Years(:)
        intent(in)          input_file
        intent(out)         Surnames, Initials, Genders, Years

        integer In, IO, i
        character(:), allocatable   :: format
        
        open (file=input_file, encoding=E_, newunit=In)
            format = '(' // SURNAMES_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, i4)'
            read (In, format, iostat=IO) (Surnames(i, :), Initials(i, :), Genders(i), Years(i), i=1,STUD_AMOUNT)
            call Handle_IO_status(IO, "Read students list")
        close (In)

    end subroutine Read_class_list
    
    ! Procedure Write
    subroutine Write_class_list(Output_File, Surnames, Initials, Genders, Years, List_name, Position)
        character(*)        Output_File, List_name, Position
        character(kind=CH_) Surnames(:, :), Initials(:, :), Genders(:)
        integer             Years(:)
        intent(in)          Output_File, List_name, Position, Surnames, Initials, Genders, Years

        integer Out, IO, i
        character(:), allocatable   :: format
        
        open (file=Output_File, encoding=E_, newunit=Out, position=Position)
            write (Out, '(/a)') List_name
            format = '(' // SURNAMES_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, a, 1x, i0)'
            write (Out, format, iostat=IO) &
                (Surnames(i, :), Initials(i, :), Genders(i), Years(i), i=1, STUD_AMOUNT)
            call Handle_IO_status(IO, "Write students list: " // List_name)
        close (Out)
    end subroutine Write_class_list
end program lab_1_2
