program lab_1_2
    use environment
    use group_io
    use group_process
    
    implicit none

    character(:), allocatable           :: input_file, output_file
    character(kind=CH_)                 :: MALE = Char(Int(z'041C'), CH_)
    type(student), pointer              :: Group_List => Null(), Boys_List => Null()
    integer                             :: CYEAR = 2015

    input_file  = "../data/input.txt"
    output_file = "output.txt"

    ! Read students
    Group_List => Read_class_list(input_file)
    
    if(Associated(Group_List)) then
        call Write_class_list(output_file, Group_List, "Исходный список", "rewind")
        
        call Get_list_by_gender(Group_List, Boys_List, MALE)
        
        if(Associated(Boys_List)) then
            ! write boys
            call Write_class_list(output_file, Boys_List, "Список юношей", "append")
            
            ! Write avg age
            call Write_Avg_Age(output_file, "юношей", Get_Avg_Age(Boys_List, 1, CYEAR, 0), "append")
        endif
    endif
end program lab_1_2
