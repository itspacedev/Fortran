program lab_1_2
    use environment
    use group_io
    implicit none

    character(:), allocatable           :: input_file, output_file, data_file
    character(kind=CH_)                 :: MALE = Char(Int(z'041C'), CH_)
    type(student)                       :: Group(STUD_AMOUNT)
    type(student), allocatable          :: Boys(:)
    integer                             :: Boys_Avg_Age, CYEAR = 2015

    input_file  = "../data/input.txt"
    output_file = "output.txt"
    data_file   = "class.dat"
    
    ! Create Data File
    call Create_data_file(input_file, data_file)
    
    ! Read Data File
    Group = Read_class_list(data_file)
    
    ! Write Group
    call Write_class_list(output_file, Group, "Исходный список", "rewind")
    
    ! Get Boys
    Boys = Pack(Group, Group%Sex == MALE)
   
    ! Write Boys list
    call Write_class_list(output_file, Boys, "Список юношей", "append")

    ! Get Boys avg age
    Boys_Avg_Age = Get_Gender_Avg_Age(Boys, CYEAR)
    
    ! Write Result
    call Write_Avg_Age(output_file, "юношей", Boys_Avg_Age, "append")    
    
   end program lab_1_2
