program lab_1
    use Environment
    
    implicit none

    integer, parameter              :: STUD_AMOUNT = 6, SURNAMES_LEN = 15, INITIALS_LEN = 5, YEARS_LEN = 4
    character(kind=CH_), parameter  :: MALE = Char(Int(z'041C'), CH_)
    character(:), allocatable       :: input_file, output_file, format, format2
    
    character(SURNAMES_LEN, kind=CH_) :: Surnames(STUD_AMOUNT) = CH__""
    character(INITIALS_LEN, kind=CH_) :: Initials(STUD_AMOUNT) = CH__""

    character(SURNAMES_LEN, kind=CH_), allocatable :: Boys_Surnames(:)
    character(INITIALS_LEN, kind=CH_), allocatable ::  Boys_Initials(:)
    integer, allocatable              :: Boys_Years(:), Boys_Age(:), Postfix_Arr(:)
    
    character(kind=CH_)               :: Gender(STUD_AMOUNT) = CH__""!, postfix_end_char = CH__""
    logical, allocatable              :: Is_A_Boy(:)
    integer                           :: Years(STUD_AMOUNT) = 0
    integer :: In, Out, IO, i, Boys_Amount = 0, Boys_Avg = 0
    integer :: CYEAR = 2015
    
    character(:), allocatable :: postfix
    !character(10, kind=CH_) :: postfix

    input_file  = "../data/input.txt"
    output_file = "output.txt"
    
    open (file=input_file, encoding=E_, newunit=In)
        format = '(3(a, 1x), i4)'
        read (In, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Years(i), i = 1, STUD_AMOUNT)
    close (In)

    Out = OUTPUT_UNIT
    open (Out, encoding=E_)
        select case (io)
            case(0)
            case(IOSTAT_END)
                write (Out, *) "End of file has been reached while reading class"
            case(1:)
                write (Out, *) "Error while reading class list: ", io
            case default
                write (Out, *) "Unknown error: ", io
        end select

    open (file=output_file, encoding=E_, newunit=Out)
        !write (Out, *) "Source: " // 12
        write (Out, '(a)') "Исходный список:"
        write (Out, format, iostat=IO) (Surnames(i), Initials(i), Gender(i), Years(i), i = 1, STUD_AMOUNT)
    close (Out)

    Is_A_Boy      = Gender == MALE ! Char(Int(z'041C', CH_))
    Boys_Amount   = Count(Is_A_Boy)

    Boys_Surnames = Pack(Surnames, Is_A_Boy)
    Boys_Initials = Pack(Initials, Is_A_Boy)
    Boys_Years    = Pack(Years, Is_A_Boy)
    
    Boys_Age = CYEAR - Boys_Years
    Boys_Avg = Ceiling(Real(Sum(Boys_Age) / Boys_Amount, R_))

    open (file=output_file, encoding=E_, newunit=Out, position='append')
        write(Out, *) (Postfix_Arr(i), i =1,Size(Postfix_Arr))
        
        write(Out, '(/a)') "Список юношей:"
        format2 = '(3(a, 1x), i4, 1x,  i0, " лет")'

        write(Out, format2, iostat=IO) &
        (Boys_Surnames(i), Boys_Initials(i), "М", Boys_Years(i), Boys_Age(i), i = 1, Boys_Amount)

        select case (Mod(Boys_Avg, 10))
            case (1)
                postfix = 'god'
            case (2:4)
                postfix = 'goda'
            case default
                postfix = 'let'
        endselect
        
        write(Out, '(/a, i0, 1x, a)') "Средний возраст юношей: ", Boys_Avg, postfix
   close (Out) 
end program lab_1
