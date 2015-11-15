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
    integer                             :: Years(STUD_AMOUNT), i, Boys_avg_age, CYEAR = 2015

    input_file  = "../data/input.txt"
    output_file = "output.txt"
    
    ! Read data
    call Read_class_list(input_file, Surnames, Initials, Genders, Years)
    
    ! Write data
    call Write_class_list(output_file, Surnames, Initials, Genders, Years, "Исходный список", "rewind")
    
    ! Get Boys
    call Get_list_by_gender(Surnames, Initials, Genders, Years, &
        Boys_Surnames, Boys_Initials, Boys_Years, MALE)

    ! Get Boys avg Age
    !call Get_avg_age(Boys_Years, Boys_avg_age)
    Boys_avg_age = Get_avg_age(Boys_Years)


    ! Write boys list
    call Write_class_list(output_file, Boys_Surnames, Boys_Initials, [(MALE, i=1, Size(Boys_Years))], &
        Boys_Years, "Список юношей", "append")
    
    ! Write boys avg age
    call Write_avg_age(output_file, "юношей", Boys_avg_age, "append")

contains
    
    ! Procedure write avg age
    subroutine Write_avg_age(output_file, str, avg_age, Position)
        character(*)    output_file, str, Position
        integer         avg_age
        intent(in)      output_file, str, avg_age, Position
    
        integer Out, IO
        character(:), allocatable :: format

        open(file=output_file, encoding=E_, newunit=Out, position=Position)
            format =  '(/a, 1x, a, 1x, i0, 1x, a)'
            write(Out, format, iostat=IO) "Средний возраст", str, avg_age, Get_postfix(avg_age)
            call Handle_IO_status(IO, "Write avg age")
        close (Out)
    endsubroutine Write_avg_age

    ! Function get age postfix
    pure function Get_postfix(Avg_age) result(Age_postfix)
        integer                      Avg_age
        character(:), allocatable :: Age_postfix

        intent(in) Avg_age

        select case (Mod(Avg_age, 10))
            case (1)
                Age_postfix = 'год'
            case (2:4)
                Age_postfix = 'года'
            case default
                Age_postfix = 'лет'
        endselect
    endfunction Get_postfix

    ! Procedure to get ages
    pure function Get_avg_age(Gender_Years) result(Gender_avg_age)
        integer         Gender_Years(:), Gender_avg_age
        
        intent(in)      Gender_Years
        !intent(out)     Gender_avg_age

        !integer(Size(Gender_Years)) :: Gender_Age
        integer :: Gender_Age(Size(Gender_Years))

        Gender_Age      = CYEAR - Gender_Years
        Gender_avg_age  = Ceiling(Real(Sum(Gender_Age) / Size(Gender_Age), R_))
    !endsubroutine Get_avg_age
    endfunction Get_avg_age

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
