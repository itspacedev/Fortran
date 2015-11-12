module Environment
    use ISO_Fortran_Env

    implicit none
    integer, parameter      :: I_ = INT32
    integer, parameter      :: R_ = REAL32
    integer, parameter      :: C_ = R_
    integer, parameter      :: CH_ = Selected_Char_Kind("ISO_10646")
    character(*), parameter :: E_ = "UTF-8"

    interface operator (//)
        module procedure Int_plus_string
        module procedure string_plus_int
    end interface

contains
    pure function Int_plus_string(int, str) result(res)
        integer, intent(in)         :: int
        character(*), intent(in)    :: str
        character(Len(str) + Max(Floor(Log10(Real(int, real64))) + 1, 1))   :: res
        
        write (res, '(i0, a)') int, str
   end function Int_plus_string

   pure function string_plus_int(strVal, intVal) result(res)
        integer, intent(in)         :: intVal
        character(*), intent(in)    :: strVal
        character(Len(strVal) + Max(Floor(Log10(Real(intVal, real64))) + 1, 1))   :: res

        write (res, '(a, i0)') strVal, intVal
   end function string_plus_int

end module Environment
