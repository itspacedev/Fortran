program lab_1_2
    use environment

    implicit none

    integer, parameter                  ::  STUD_AMOUNT = 8, SURNAMES_LEN = 15, INITIALS_LEN = 5
    character(kind=CH_)                 ::  MALE = Char(Int(z'041C'), CH_)
    character(:), allocatable           ::  input_file, output_file

    character(kind=CH_)                 ::  Surnames(STUD_AMOUNT, SURNAMES_LEN) = "", &
                                            Initials(STUD_AMOUNT, INITIALS_LEN) = "", &
                                            Genders(STUD_AMOUNT) = ""

    character(kind=CH_), allocatable    :: Boys_Surnames(:, :), Boys_Initials(:, :)
    integer, allocatable                :: Boys_Years(:)
    integer                             :: Years(STUD_AMOUNT), i

    input_file  = "../data/input.txt"
    output_file = "output.txt"
    
    ! Read data
    call Read_class_list(input_file, Surnames, Initials, Genders, Years)
    
    ! Write data
    call Write_class_list(output_file, Surnames, Initials, Genders, Years, "Исходный список", "rewind")
    
    ! Get Boys
    call Get_list_by_gender(Surnames, Initials, Genders, Years, &
        Boys_Surnames, Boys_Initials, Boys_Years, MALE)
    
    ! Write boys list
    call Write_class_list(output_file, Boys_Surnames, Boys_Initials, [(MALE, i=1, Size(Boys_Years))], &
        Boys_Years, "Список юношей", "append")

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
                (Surnames(i, :), Initials(i, :), Genders(i), Years(i), i=1, Size(Genders))
            call Handle_IO_status(IO, "Write students list: " // List_name)
        close (Out)
    end subroutine Write_class_list

    ! Get list by gender
    subroutine Get_list_by_gender(Surnames, Initials, Genders, Years, &
        Gender_Surnames, Gender_Initials, Gender_Years, Gender)
        
        character(kind=CH_) Surnames(:, :), Initials(:, :), Genders(:)
        integer Years(:)

        character(kind=CH_) Gender_Surnames(:, :), Gender_Initials(:, :)
        integer Gender_Years(:)

        character(kind=CH_) Gender
        
        intent(in)  Surnames, Initials, Genders, Years, Gender
        intent(out) Gender_Surnames, Gender_Initials, Gender_Years
        allocatable Gender_Surnames, Gender_Initials, Gender_Years

        logical, allocatable    :: Is_A_Gender(:)
        integer, allocatable    :: Gender_Pos(:)
        integer                 :: Gender_Amount, i
        integer, parameter      :: INDEXES(*) = [(i, i=1, STUD_AMOUNT)]
        
        Is_A_Gender = Genders == Gender
        Gender_Amount = Count(Is_A_Gender)

        Gender_Pos = Pack(INDEXES, Is_A_Gender)
        allocate(Gender_Surnames(Gender_Amount, SURNAMES_LEN), Gender_Initials(Gender_Amount, INITIALS_LEN), &
            Gender_Years(Gender_Amount))

        do concurrent (i = 1:Gender_Amount) 
            Gender_Surnames(i, :) = Surnames(Gender_Pos(i), :)
            Gender_Initials(i, :) = Initials(Gender_Pos(i), :)
            Gender_Years(i) = Years(Gender_Pos(i))
        enddo
    end subroutine Get_list_by_gender
end program lab_1_2
