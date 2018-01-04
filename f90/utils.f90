! **********************************************************************
! All subroutines licensed under the terms of LGPL or GPLv2 or above,
! as specified in the subroutine headers.
! **********************************************************************

module utils

implicit none

!*********************************************************
! Double precision constants
real(8), parameter    :: PI  = 3.14159265357898
real(8), parameter    :: D2R = PI/180.d0
real(8), parameter    :: R2D = 180.d0/PI
real(8), parameter    :: EPS = 1.0e-8
real(8), parameter    :: EarthRad = 6378.137; ! km
real(8), parameter    :: EarthEcc2 = 0.00669437999014 ! eccentricity squared


! **********************************************************************
!  subroutines to compute 2x2 and 3x3 matrix inverses and conjugates

public                     ::  matconjg, rmatinv2, matinv2, matinv3

! **********************************************************************
!  ascii character functions

public                     ::  toupper, isdigit, isempty

! **********************************************************************
!  utility subroutines

public                     ::  deg2dms, dms2deg, sortidx, datestr


! **********************************************************************
!  ascii character codes (default integers)

integer, public, parameter :: ascii_nul = 0                          ! null

integer, public, parameter :: ascii_soh = 1                          ! start of heading
integer, public, parameter :: ascii_stx = 2                          ! start of text
integer, public, parameter :: ascii_etx = 3                          ! end of text
integer, public, parameter :: ascii_eot = 4                          ! end of transmission
integer, public, parameter :: ascii_enq = 5                          ! enquiry
integer, public, parameter :: ascii_ack = 6                          ! acknowledge
integer, public, parameter :: ascii_bel = 7                          ! bell
integer, public, parameter :: ascii_bs = 8                           ! backspace
integer, public, parameter :: ascii_ht = 9                           ! horizontal tab
integer, public, parameter :: ascii_lf = 10                          ! line feed
integer, public, parameter :: ascii_vt = 11                          ! vertical tab
integer, public, parameter :: ascii_ff = 12                          ! form feed
integer, public, parameter :: ascii_cr = 13                          ! carriage return
integer, public, parameter :: ascii_s0 = 14                          ! shift out
integer, public, parameter :: ascii_si = 15                          ! shift in
integer, public, parameter :: ascii_dle = 16                         ! data link escape
integer, public, parameter :: ascii_dc1 = 17                         ! device control 1
integer, public, parameter :: ascii_dc2 = 18                         ! device control 2
integer, public, parameter :: ascii_dc3 = 19                         ! device control 3
integer, public, parameter :: ascii_dc4 = 20                         ! device control 4
integer, public, parameter :: ascii_nak = 21                         ! negative acknowledge
integer, public, parameter :: ascii_syn = 22                         ! synchronous idle
integer, public, parameter :: ascii_etb = 23                         ! end of transmission block
integer, public, parameter :: ascii_can = 24                         ! cancel
integer, public, parameter :: ascii_em = 25                          ! end of medium
integer, public, parameter :: ascii_sub = 26                         ! substitute
integer, public, parameter :: ascii_esc = 27                         ! escape
integer, public, parameter :: ascii_fs = 28                          ! file separator
integer, public, parameter :: ascii_gs = 29                          ! group separator
integer, public, parameter :: ascii_rs = 30                          ! record separator
integer, public, parameter :: ascii_us = 31                          ! unit separator

integer, public, parameter :: ascii_sp = 32                          ! space
integer, public, parameter :: ascii_ep = 33                          ! ! exclamation point
integer, public, parameter :: ascii_qtm = 34                         ! " quotation marks
integer, public, parameter :: ascii_ns = 35                          ! # number sign
integer, public, parameter :: ascii_cs = 36                          ! $ currency symbol
integer, public, parameter :: ascii_pct = 37                         ! % percent
integer, public, parameter :: ascii_amp = 38                         ! & ampersand
integer, public, parameter :: ascii_apo = 39                         ! ' apostrophe
integer, public, parameter :: ascii_lp = 40                          ! ( left parenthesis
integer, public, parameter :: ascii_rp = 41                          ! ) right parenthesis
integer, public, parameter :: ascii_ast = 42                         ! * asterisk
integer, public, parameter :: ascii_pls = 43                         ! + plus
integer, public, parameter :: ascii_com = 44                         ! , comma
integer, public, parameter :: ascii_mns = 45                         ! - minus
integer, public, parameter :: ascii_prd = 46                         ! . period
integer, public, parameter :: ascii_sl = 47                          ! / slash

integer, public, parameter :: ascii_0 = 48                           ! 0 zero
integer, public, parameter :: ascii_1 = 49                           ! 1 one
integer, public, parameter :: ascii_2 = 50                           ! 2 two
integer, public, parameter :: ascii_3 = 51                           ! 3 three
integer, public, parameter :: ascii_4 = 52                           ! 4 four
integer, public, parameter :: ascii_5 = 53                           ! 5 five
integer, public, parameter :: ascii_6 = 54                           ! 6 six
integer, public, parameter :: ascii_7 = 55                           ! 7 seven
integer, public, parameter :: ascii_8 = 56                           ! 8 eight
integer, public, parameter :: ascii_9 = 57                           ! 9 nine

integer, public, parameter :: ascii_cl = 58                          ! : colon
integer, public, parameter :: ascii_scl = 59                         ! ; semicolon
integer, public, parameter :: ascii_lt = 60                          ! < less than
integer, public, parameter :: ascii_eq = 61                          ! = equals
integer, public, parameter :: ascii_gt = 62                          ! > greater than
integer, public, parameter :: ascii_qm = 63                          ! ? question mark
integer, public, parameter :: ascii_ats = 64                         ! @ at sign

integer, public, parameter :: ascii_uca = 65                         ! upper case A
integer, public, parameter :: ascii_ucb = 66                         ! upper case B
integer, public, parameter :: ascii_ucc = 67                         ! upper case C
integer, public, parameter :: ascii_ucd = 68                         ! upper case D
integer, public, parameter :: ascii_uce = 69                         ! upper case E
integer, public, parameter :: ascii_ucf = 70                         ! upper case F
integer, public, parameter :: ascii_ucg = 71                         ! upper case G
integer, public, parameter :: ascii_uch = 72                         ! upper case H
integer, public, parameter :: ascii_uci = 73                         ! upper case I
integer, public, parameter :: ascii_ucj = 74                         ! upper case J
integer, public, parameter :: ascii_uck = 75                         ! upper case K
integer, public, parameter :: ascii_ucl = 76                         ! upper case L
integer, public, parameter :: ascii_ucm = 77                         ! upper case M
integer, public, parameter :: ascii_ucn = 78                         ! upper case N
integer, public, parameter :: ascii_uco = 79                         ! upper case O
integer, public, parameter :: ascii_ucp = 80                         ! upper case P
integer, public, parameter :: ascii_ucq = 81                         ! upper case Q
integer, public, parameter :: ascii_ucr = 82                         ! upper case R
integer, public, parameter :: ascii_ucs = 83                         ! upper case S
integer, public, parameter :: ascii_uct = 84                         ! upper case T
integer, public, parameter :: ascii_ucu = 85                         ! upper case U
integer, public, parameter :: ascii_ucv = 86                         ! upper case V
integer, public, parameter :: ascii_ucw = 87                         ! upper case W
integer, public, parameter :: ascii_ucx = 88                         ! upper case X
integer, public, parameter :: ascii_ucy = 89                         ! upper case Y
integer, public, parameter :: ascii_ucz = 90                         ! upper case Z

integer, public, parameter :: ascii_lb = 91                          ! [ left bracket
integer, public, parameter :: ascii_bsl = 92                         ! \ backslash
integer, public, parameter :: ascii_rb = 93                          ! ] right bracket
integer, public, parameter :: ascii_crt = 94                         ! ^ caret
integer, public, parameter :: ascii_und = 95                         ! _ underscore
integer, public, parameter :: ascii_gra = 96                         ! ` grave accent

integer, public, parameter :: ascii_lca = 97                         ! lower case a
integer, public, parameter :: ascii_lcb = 98                         ! lower case b
integer, public, parameter :: ascii_lcc = 99                         ! lower case c
integer, public, parameter :: ascii_lcd = 100                        ! lower case d
integer, public, parameter :: ascii_lce = 101                        ! lower case e
integer, public, parameter :: ascii_lcf = 102                        ! lower case f
integer, public, parameter :: ascii_lcg = 103                        ! lower case g
integer, public, parameter :: ascii_lch = 104                        ! lower case h
integer, public, parameter :: ascii_lci = 105                        ! lower case i
integer, public, parameter :: ascii_lcj = 106                        ! lower case j
integer, public, parameter :: ascii_lck = 107                        ! lower case k
integer, public, parameter :: ascii_lcl = 108                        ! lower case l
integer, public, parameter :: ascii_lcm = 109                        ! lower case m
integer, public, parameter :: ascii_lcn = 110                        ! lower case n
integer, public, parameter :: ascii_lco = 111                        ! lower case o
integer, public, parameter :: ascii_lcp = 112                        ! lower case p
integer, public, parameter :: ascii_lcq = 113                        ! lower case q
integer, public, parameter :: ascii_lcr = 114                        ! lower case r
integer, public, parameter :: ascii_lcs = 115                        ! lower case s
integer, public, parameter :: ascii_lct = 116                        ! lower case t
integer, public, parameter :: ascii_lcu = 117                        ! lower case u
integer, public, parameter :: ascii_lcv = 118                        ! lower case v
integer, public, parameter :: ascii_lcw = 119                        ! lower case w
integer, public, parameter :: ascii_lcx = 120                        ! lower case x
integer, public, parameter :: ascii_lcy = 121                        ! lower case y
integer, public, parameter :: ascii_lcz = 122                        ! lower case z

integer, public, parameter :: ascii_lbr = 123                        ! { left brace
integer, public, parameter :: ascii_vl = 124                         ! | vertical line
integer, public, parameter :: ascii_rbr = 125                        ! } right brace
integer, public, parameter :: ascii_tld = 126                        ! ~ tilde

integer, public, parameter :: ascii_del = 127                        ! delete


contains

! **********************************************************************
! datestr(): converts date/time string from one format to another;
! limited functionality
! (C) Anna Kelbert, 2013

character(len=80) function datestr(time1,format1,format2) result (time2)

  character(len=*), intent(in) :: time1,format1,format2
  ! local
  character(len=2)             :: month, day, hour, minute, second, year2
  character(len=4)             :: year
  integer                      :: k(2), n, value

  select case (trim(format1))
  case ('YYYY-MM-DDThh:mm:ss','YYYY-MM-DD','XML') ! XML date and time format
    year = time1(1:4)
    month = time1(6:7)
    day = time1(9:10)
    if (len_trim(time1) >= 19) then
        hour = time1(12:13)
        minute = time1(15:16)
        second = time1(18:19)
    else
        hour = '00'
        minute = '00'
        second = '00'
    end if
  case ('YYYYMMDD hh:mm:ss','YYYYMMDD') ! MT1 system field data file format
    year = time1(1:4)
    month = time1(5:6)
    day = time1(7:8)
    if (len_trim(time1) >= 9) then
        hour = time1(10:11)
        minute = time1(13:14)
        second = time1(16:17)
    else
        hour = '00'
        minute = '00'
        second = '00'
    end if
  case ('YYYY') ! Year only
    year = time1(1:4)
    month = '01'
    day = '01'
    hour = '00'
    minute = '00'
    second = '00'
  case ('MM/DD/YY') ! USA EDI format
    ! we want to also accommodate sloppy M/D/YY format
    ! but keeping the simple syntax as default
    n = len_trim(time1)
    if (n<8) then
        k(1) = index(time1,'/')
        k(2) = k(1)+index(time1(k(1)+1:n),'/')
        read(time1(1:k(1)-1),*) value
        write(month,'(i0.2)') value
        read(time1(k(1)+1:k(2)-1),*) value
        write(day,'(i0.2)') value
        read(time1(k(2)+1:n),*) value
        write(year2,'(i0.2)') value
    else
        month = time1(1:2)
        day = time1(4:5)
        year2 = time1(7:8)
    end if
    ! allow for years 2000's, 2010's and 2020's;
    ! assume 20th century for everything else
    if (iachar(year2(1:1)) <= ascii_2) then
        year = '20'//year2
    else
        year = '19'//year2
    end if
    hour = '00'
    minute = '00'
    second = '00'
  case ('DD/MM/YY') ! European EDI format
    ! we want to also accommodate sloppy D/M/YY format
    ! but keeping the simple syntax as default
    n = len_trim(time1)
    if (n<8) then
        k(1) = index(time1,'/')
        k(2) = k(1)+index(time1(k(1)+1:n),'/')
        read(time1(1:k(1)-1),*) value
        write(day,'(i0.2)') value
        read(time1(k(1)+1:k(2)-1),*) value
        write(month,'(i0.2)') value
        read(time1(k(2)+1:n),*) value
        write(year2,'(i0.2)') value
    else
        day = time1(1:2)
        month = time1(4:5)
        year2 = time1(7:8)
    end if
    ! allow for years 2000's, 2010's and 2020's;
    ! assume 20th century for everything else
    if (iachar(year2(1:1)) <= ascii_2) then
        year = '20'//year2
    else
        year = '19'//year2
    end if
    hour = '00'
    minute = '00'
    second = '00'
  case ('DD/MM/YYYY') ! Lana added for AdelaideUni Surveys 
    year = time1(7:10)
    month = time1(4:5)
    day = time1(1:2)
    second = '00'   
    if (len_trim(time1) == 16) then ! hh:mm
        hour = time1(12:13)
        minute = time1(15:16)
    elseif (len_trim(time1) == 15) then ! h:mm
        hour = '0'//time1(12:12)
        minute = time1(14:15)
    else
        hour = '00'
        minute = '00'
    end if
  case ('DD/MM/YYYY hh:mm') ! Lana added for AdelaideUni Surveys 
    year = time1(7:10)
    month = time1(4:5)
    day = time1(1:2)
    second = '00'   
    if (len_trim(time1) == 16) then ! hh:mm
        hour = time1(12:13)
        minute = time1(15:16)
    elseif (len_trim(time1) == 15) then ! h:mm
        hour = '0'//time1(12:12)
        minute = time1(14:15)
    else
        hour = '00'
        minute = '00'
    end if
  case default
    write(0,*) 'Warning: unknown input time format ',trim(format1),' for ',trim(time1)
    time2 = time1
    return
  end select

  select case (trim(format2))
  case ('YYYY-MM-DDThh:mm:ss','XML') ! XML date and time format
    time2 = year//'-'//month//'-'//day//'T'//hour//':'//minute//':'//second
  case ('YYYY-MM-DD') ! XML date format
    time2 = year//'-'//month//'-'//day
  case ('YYYY') ! Year only
    time2 = year
  case ('MM/DD/YY')
    time2 = month//'/'//day//'/'//year(3:4)
  case ('DD/MM/YY')
    time2 = day//'/'//month//'/'//year(3:4)
  case default
    write(0,*) 'Warning: unknown output time format ',format2,' for ',time1
    time2 = time1
    return
  end select

end function datestr


! **********************************************************************
! isempty(): checks for an empty string; outputs true or false; helps avoid
! segmentation fault and not use adjustl on empty strings of zero size
! (C) Anna Kelbert, 2013

logical function isempty(str)

  character(len=*), intent(in) :: str

  if (len_trim(str)==0) then
    isempty = .true.
  else
    isempty = .false.
  end if

end function isempty

! **********************************************************************
! identity(): outputs an identity matrix of the same size as the input
! (C) Anna Kelbert, 2009

subroutine identity(a)

  real(8), intent(inout), dimension(:,:) :: a
  integer :: i, n

  if (size(a,1) /= size(a,2)) then
  	write(0,*) 'Error in identity: not a square matrix'
  	return
  end if

  n = size(a,1)

  a = 0.0d0

  do i=1,n
  	a(i,i) = 1.0d0
  end do

end subroutine identity

! **********************************************************************
! matconjg(): computes a general conjugate transpose of a complex matrix
! (C) Anna Kelbert, 2017

pure function matconjg(A) result(B)
    !! Performs a direct calculation of the conjugate transpose of an NxM matrix.
    complex(8), intent(in)   :: A(:,:)   !! Matrix
    complex(8), allocatable  :: B(:,:)   !! Conjugate transpose matrix
    integer :: istat, i, j, nc1, nc2

    ! Calculate the hermitian conjugate of the matrix
    nc1 = size(A,1)
    nc2 = size(A,2)
    allocate(B(nc2,nc1), STAT=istat)
    B = transpose(A)
    do i=1,nc2
       do j=1,nc1
          B(i,j) = dconjg(B(i,j))
       end do
    end do

  end function

! **********************************************************************
! rmatinv2(): computes the inverse of a 2x2 real matrix

  function rmatinv2(A) result(B)
    !! Performs a direct calculation of the inverse of a 2×2 matrix.
    real(8), intent(in) :: A(2,2)   !! Matrix
    real(8)             :: B(2,2)   !! Inverse matrix
    real(8)             :: det,detinv

    det = (A(1,1)*A(2,2) - A(1,2)*A(2,1))

    if (abs(det) < epsilon(det)) then
        write(0,*) 'Error in rmatinv2: zero determinant'
    end if

    ! Calculate the inverse determinant of the matrix
    detinv = 1/det

    ! Calculate the inverse of the matrix
    B(1,1) = +detinv * A(2,2)
    B(2,1) = -detinv * A(2,1)
    B(1,2) = -detinv * A(1,2)
    B(2,2) = +detinv * A(1,1)
  end function

! **********************************************************************
! matinv2(): computes the inverse of a 2x2 complex matrix

  function matinv2(A) result(B)
    !! Performs a direct calculation of the inverse of a 2×2 matrix.
    complex(8), intent(in) :: A(2,2)   !! Matrix
    complex(8)             :: B(2,2)   !! Inverse matrix
    complex(8)             :: det,detinv

    det = (A(1,1)*A(2,2) - A(1,2)*A(2,1))

    if (abs(det) < epsilon(dreal(det))) then
        write(0,*) 'Error in matinv2: zero determinant'
    end if

    ! Calculate the inverse determinant of the matrix
    detinv = 1/det

    ! Calculate the inverse of the matrix
    B(1,1) = +detinv * A(2,2)
    B(2,1) = -detinv * A(2,1)
    B(1,2) = -detinv * A(1,2)
    B(2,2) = +detinv * A(1,1)
  end function

! **********************************************************************
! matinv3(): computes the inverse of a 3x3 complex matrix

  function matinv3(A) result(B)
    !! Performs a direct calculation of the inverse of a 3×3 matrix.
    complex(8), intent(in) :: A(3,3)   !! Matrix
    complex(8)             :: B(3,3)   !! Inverse matrix
    complex(8)             :: det,detinv

    det = (A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
              - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
              + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

    if (abs(det) < epsilon(dreal(det))) then
        write(0,*) 'Error in matinv3: zero determinant'
    end if

    ! Calculate the inverse determinant of the matrix
    detinv = 1/det

    ! Calculate the inverse of the matrix
    B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
    B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
    B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
    B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
    B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
    B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
    B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
    B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
    B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
  end function

! **********************************************************************
! matinv4(): computes the inverse of a 4x4 complex matrix

  function matinv4(A) result(B)
    !! Performs a direct calculation of the inverse of a 4×4 matrix.
    complex(8), intent(in) :: A(4,4)   !! Matrix
    complex(8)             :: B(4,4)   !! Inverse matrix
    complex(8)             :: det,detinv

    det = (A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
       - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
       + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
       - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

    if (abs(det) < epsilon(dreal(det))) then
        write(0,*) 'Error in matinv4: zero determinant'
    end if

    ! Calculate the inverse determinant of the matrix
    detinv = 1/det

    ! Calculate the inverse of the matrix
    B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
    B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
    B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
    B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
    B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
    B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
    B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
    B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
    B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
    B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
    B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
    B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
    B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
    B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
    B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
    B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
  end function

! **********************************************************************
function to_upper(strIn) result(strOut)
! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)

     implicit none

     character(len=*), intent(in) :: strIn
     character(len=len(strIn)) :: strOut
     integer :: i,j

     do i = 1, len(strIn)
          j = iachar(strIn(i:i))
          if (j>= iachar("a") .and. j<=iachar("z") ) then
               strOut(i:i) = achar(iachar(strIn(i:i))-32)
          else
               strOut(i:i) = strIn(i:i)
          end if
     end do

end function to_upper

! **********************************************************************
!  toupper(): if lowercase, change to uppercase
! Extracted from Fortran 95 module character_functions
! (C) 2003 Purple Sage Computing Solutions, Inc.
! licensed under the terms of the GPLv2 or above.

elemental character( len= 1) function toupper( ch)

   character( len= 1), intent( in) :: ch

   integer :: ich

! ----------------------------------------------------------------------

   ich = iachar( ch)                                                 ! get ascii character code

   ch_range: select case( ich)                                       ! which character

   case( ascii_lca: ascii_lcz) ch_range                              ! lower case

      toupper = achar( ich - 32 )                                    ! change to upper case

   case default ch_range                                             ! otherwise

      toupper = achar( ich)                                          ! ignore

   end select ch_range                                               ! which character

! ----------------------------------------------------------------------

end function toupper


! **********************************************************************
!  isdigit(): true if input ascii character is numeric
! Extracted from Fortran 95 module character_functions
! (C) 2003 Purple Sage Computing Solutions, Inc.
! licensed under the terms of the GPLv2 or above.

elemental logical function isdigit( ch)

   character( len= 1), intent( in) :: ch

   integer :: ich

! ----------------------------------------------------------------------

   ich = iachar( ch)                                                 ! get ascii character code

   ch_range: select case( ich)                                       ! which character

   case( ascii_0: ascii_9) ch_range                                  ! digit

      isdigit = .true.                                               ! found it

   case default ch_range                                             ! otherwise

      isdigit = .false.                                              ! found it not

   end select ch_range                                               ! which character

! ----------------------------------------------------------------------

end function isdigit

logical function isnan(value)
real(8) value
real a
a = real(value)
if (.not.(a<a+1)) then
isnan = .true.
else
isnan = .false.
end if
return
end function

logical function isinf(value)
real(8) value
real a
a = real(value)
if ((a*0).ne.0) then
isinf = .true.
else
isinf = .false.
end if
return
end function

! **********************************************************************
! fix_spaces(): replace all new line, carriage return and tab characters
! with spaces - typically to be postprocessed with trim(adjustl(str2))
! (C) Anna Kelbert, 2013
!
! Note that the declaration
! character(len=len(str1)) function fix_spaces(str1) result (str2)
! causes Intel compiler to choke; thus replaced with len=800 [AK, 2015]

character(len=800) function fix_spaces(str1) result (str2)

     character(len=*), intent(in) :: str1
     ! local
     integer i

     str2 = str1
     do i=1,len(str2)
        select case (ichar(str2(i:i)))
            case (ascii_lf)
                ! new line
                str2(i:i) = achar(ascii_sp)
            case (ascii_cr)
                ! carriage return
                str2(i:i) = achar(ascii_sp)
            case (ascii_ht)
                ! horizontal tab
                str2(i:i) = achar(ascii_sp)
            case (ascii_vt)
                ! vertical tab
                str2(i:i) = achar(ascii_sp)
            case (ascii_sp)
                ! space
            case default
                ! otherwise, do nothing
        end select
     end do

end function fix_spaces

! **********************************************************************
! A quick and dirty string parsing routine that divides a string
! up into segments using a one-char delimiter.
! For efficiency, uses a hardcoded max number of strings = 100.
! Written by: Anna Kelbert, 11 March 2013
! Last edited on 16 Aug 2013 to remove trailing tabs & new lines.
! This subroutine is distributed under the terms of
! the GNU Lesser General Public License.

subroutine parse_str(str,delim,strarray,num)
     character(len=*), intent(in) :: str
     character(len=1), intent(in) :: delim
     character(len=*), pointer    :: strarray(:)
     integer, intent(out), optional :: num
     ! local
     character(len=len(str)) :: strtail,temp
     integer                 :: i1,i2,istat,L,N,i,j(100)

     L = len(str)
     strtail = str
     i1 = 1
     N = 1
     j(N) = i1 ! start index for delim-separated string
     do ! marking and counting
        i2 = i1 + index(strtail,delim) - 1
        !write(*,*) 'DEBUG: ',i1,delim,i2,strtail
        if (i2>i1) then
            temp = str(i1:i2-1)
            strtail = trim(str(i2+1:L))
            i1 = i2 + 1
            N = N + 1
            j(N) = i1
        else ! final text portion
            temp = trim(strtail)
            exit
        end if
     end do

     if (associated(strarray)) then
        deallocate(strarray, stat=istat)
     end if
     allocate(strarray(N), stat=istat)
     do i = 1,N-1
        strarray(i) = trim(adjustl(fix_spaces(str(j(i):j(i+1)-2))))
        !write(*,*) 'DEBUG: ',j(i),j(i+1),L,strarray(i)
     end do
     strarray(N) = trim(adjustl(fix_spaces(str(j(N):L))))

     if (present(num)) then
        num = N
     end if

end subroutine parse_str


! **********************************************************************
! Convert latitude or longitude from a decimal to the
! sexagesimal system (degrees, minutes & seconds);
! the result is a character string length 16.
! Written by: Anna Kelbert, 3 Nov 2007
! Modified  : 30 Jun 2011 to add leading zeros to mins
! Modified  : 3 March 2013 to improve accuracy
! See also  : dms2deg
! This subroutine is distributed under the terms of
! the GNU Lesser General Public License.

  function deg2dms(loc) result (cloc)

  	real(8), intent(in)     :: loc

  	real(8)                 :: aloc
  	character(len=16)       :: cloc
  	integer                 :: d,m
  	real                    :: s
  	character(len=4)        :: cdeg
  	character(len=2)        :: cmin
  	character(len=5)        :: csec
	logical                 :: negative

! ----------------------------------------------------------------------

  	negative = .false.
    if (loc < 0) negative = .true.
	aloc = abs(loc)

	d = floor(aloc)
	if (negative) then
		write(cdeg,'(i4)') -d
	else
		write(cdeg,'(i4)') d
	end if

	m = floor((aloc-d)*60)
	write(cmin,'(i2.2)') m

	s = ((aloc-dble(d))*60.0d0-dble(m))*60.0d0
    write(csec,'(i2.2,a1,i2.2)') floor(s),'.',floor(100*(s-floor(s)))

	cloc = cdeg//':'//cmin//':'//csec

! ----------------------------------------------------------------------

  end function deg2dms

! **********************************************************************
! Convert latitude or longitude from sexagesimal system
! (degrees, minutes & seconds) to degrees.
! Input is a character string length 16.
! Output is a real value.
! Written by: Anna Kelbert, 9 Jan 2013
! Modified  : 3 March 2013 to fix negative longitudes
! See also  : deg2dms
! This subroutine is distributed under the terms of
! the GNU Lesser General Public License.

  function dms2deg(cloc) result (loc)

    character(len=*), intent(in)  :: cloc

    real(8)                 :: aloc
    real(8)                 :: loc
    integer                 :: i1,i2
    integer                 :: d,m
    real                    :: s
    character(len=4)        :: cdeg
    character(len=2)        :: cmin
    character(len=5)        :: csec

! ----------------------------------------------------------------------

    i1 = index(cloc,':')
    i2 = i1+index(cloc(i1+1:16),':')
    cdeg = cloc(1:i1-1)
    cmin = cloc(i1+1:i2-1)
    csec = trim(cloc(i2+1:16))

    read(cdeg,*) d
    read(cmin,*) m
    read(csec,*) s

    if (d<0) then
        loc = dble(d) - (60.0d0*dble(m) + dble(s))/3600.0d0
    else
        loc = dble(d) + (60.0d0*dble(m) + dble(s))/3600.0d0
    end if

! ----------------------------------------------------------------------

  end function dms2deg

! **********************************************************************
! Tools to convert lat/lon to X/Y and back
! Written by: Anna Kelbert, 3 March 2013
! See also  : ll2xy, xy2ll
! Matlab    :
!        function res = kmPerDeg(lat0)
!            a = latlontools.EarthRad;
!            e2 = latlontools.EarthEcc2;
!            if nargin > 0
!                res = pi*a*(1-e2)/(180*(1-e2*sin(lat0*pi/180)^2)^(3/2));
!            else
!                res = 1.1119e+02; % for latitude ~ 48 degrees
!            end
!        end
! This subroutine is distributed under the terms of
! the GNU Lesser General Public License.
  function kmPerDeg(lat0) result (res)

    real(8), intent(in), optional :: lat0
    ! local
    real(8) a,e2,res

    a = EarthRad
    e2 = EarthEcc2
    if (present(lat0)) then
        res = PI*a*(1.0d0-e2)/(180.0d0*(1.0d0-e2*sin(lat0*D2R)**2)**(3/2))
    else
        res = 1.1119e+02 ! for latitude ~ 48 degrees
    end if

  end function kmPerDeg

! **********************************************************************
! Tools to convert lat/lon to X/Y and back
! Ideally you would know what sort of projection was used to compute locations in km.
! The obvious crude (but fine for small areas) approximation is
! x =(lat-lat0)*a/360 and
! y = cos(lat)*(lon-lon0)*a/360,
! where a is the circumference of the earth .... 1 degree ~ 111 km.
! Written by: Gary Egbert & Anna Kelbert, 3 March 2013
! See also  : kmPerDeg, xy2ll
! Matlab    :
!        function [xy] = ll2xy(ll,lat0,lon0)
!            xy(1,:) = latlontools.kmPerDeg(lat0)*(ll(1,:)-lat0);
!            xy(2,:) = latlontools.kmPerDeg(lat0)*(ll(2,:)-lon0).*cos(ll(1,:)*pi/180);
!        end
! This subroutine is distributed under the terms of
! the GNU Lesser General Public License.
  subroutine ll2xy(ll,xy,lat0,lon0)

    real(8), dimension(:,:), intent(in)  :: ll ! (2,N)
    real(8), dimension(:,:), intent(out) :: xy ! (2,N)
    real(8), intent(in)                  :: lat0, lon0

    xy(1,:) = 1.0e3*kmPerDeg(lat0)*(ll(1,:)-lat0)
    xy(2,:) = 1.0e3*kmPerDeg(lat0)*(ll(2,:)-lon0)*cos(ll(1,:)*D2R)

  end subroutine ll2xy

! **********************************************************************
! Tools to convert lat/lon to X/Y and back
! Ideally you would know what sort of projection was used to compute locations in km.
! The obvious crude (but fine for small areas) approximation is
! x =(lat-lat0)*a/360 and
! y = cos(lat)*(lon-lon0)*a/360,
! where a is the circumference of the earth .... 1 degree ~ 111 km.
! Written by: Gary Egbert & Anna Kelbert, 3 March 2013
! See also  : ll2xy, kmPerDeg
! Matlab    :
!        function [ll] = xy2ll(xy,lat0,lon0)
!            ll(1,:) = xy(1,:)/latlontools.kmPerDeg(lat0)+lat0;
!            ll(2,:) = (xy(2,:)/latlontools.kmPerDeg(lat0))./cos(ll(1,:)*pi/180)+lon0;
!        end
! This subroutine is distributed under the terms of
! the GNU Lesser General Public License.
  subroutine xy2ll(xy,ll,lat0,lon0)

    real(8), dimension(:,:), intent(in)  :: xy ! (2,N)
    real(8), dimension(:,:), intent(out) :: ll ! (2,N)
    real(8), intent(in)                  :: lat0, lon0

    ll(1,:) = xy(1,:)/(1.0e3*kmPerDeg(lat0))+lat0
    ll(2,:) = ( xy(2,:)/(1.0e3*kmPerDeg(lat0)) )/cos(ll(1,:)*D2R)+lon0

  end subroutine xy2ll


! **********************************************************************
! (C) 2002-2005 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This subroutine is distributed under the terms of the GNU Lesser
! General Public License.

!BOP
! !ROUTINE: sortidx
! !INTERFACE:
subroutine sortidx(n,a,idx)
! !INPUT/OUTPUT PARAMETERS:
!   n   : number of elements in array (in,integer)
!   idx : permutation index (out,integer(n))
!   a   : real array (in,real(n))
! !DESCRIPTION:
!   Finds the permutation index {\tt idx} which sorts the real array {\tt a}
!   into ascending order. No sorting of the array {\tt a} itself is performed.
!   Uses the heapsort algorthim.
!
! !REVISION HISTORY:
!   Created October 2002 (JKD)
!   Included tolerance eps, April 2006 (JKD)
!EOP
!BOC
implicit none
! arguments
integer, intent(in) :: n
real(8), intent(in) :: a(n)
integer, intent(out) :: idx(n)
! local variables
integer i,j,k,l,m
! tolerance for deciding if one number is smaller than another
real(8), parameter :: eps=1.d-14
if (n.le.0) then
  write(*,*)
  write(*,'("Error(sortidx): n <= 0 : ",I8)') n
  write(*,*)
  stop
end if
do i=1,n
  idx(i)=i
end do
if (n.eq.1) return
l=n/2+1
k=n
10 continue
if (l.gt.1) then
  l=l-1
  m=idx(l)
else
  m=idx(k)
  idx(k)=idx(1)
  k=k-1
  if (k.eq.1) then
    idx(1)=m
    return
  end if
end if
i=l
j=l+l
20 continue
if (j.le.k) then
  if (j.lt.k) then
    if (a(idx(j)).lt.a(idx(j+1))+eps) j=j+1
  end if
  if (a(m).lt.a(idx(j))+eps) then
    idx(i)=idx(j)
    i=j
    j=j+j
  else
    j=k+1
  end if
  goto 20
end if
idx(i)=m
goto 10
end subroutine
!EOC

! **********************************************************************
! (C) 2006 Xavier Garcia from edi2edi

Pure Integer Function fin(string)
! this routine locates the end of a character string without spaces
character(LEN=*), INTENT (IN) :: string
fin=index(string,' ')-1
if (fin == -1 .AND. len(string) /= 0) fin=len(string)
return
end function fin

!...............................................................................

Pure Integer Function txtfin(string)
! this routine locates the end of a character string with spaces, e.g., a windows path
character(LEN=*), INTENT (IN) :: string
integer :: n
n=len(string)
do while(string(n:n) == ' ' .AND. n>0)
   n=n-1
end do
txtfin=n
return
end function txtfin

!...............................................................................

subroutine tin(text,dfault)
implicit none
character(LEN=*), INTENT(INOUT):: dfault
character(LEN=*), INTENT(IN):: text
character(LEN=1024) line
character(LEN=120):: AUX
integer :: n
line=''
n=len_trim(dfault)
write(AUX,'(A)') dfault
AUX=adjustl(AUX)
aux=aux(1:n)//']: '
write(*,'(a,a,a)',advance='no') text//' [Default: ',trim(aux),' '
write(*,'($)')
read(*,'(a)') line
if(line /= "") read(line,'(A)') dfault
end subroutine tin

!...............................................................................

subroutine yin(text,dfault)
implicit none
character(LEN=*), INTENT(INOUT):: dfault
character(LEN=*), INTENT(IN):: text
character(LEN=1) line
character(LEN=120):: AUX
integer :: n
dfault = to_upper(dfault)
if (dfault /= 'Y' .AND. dfault /= 'N') then
   write(*,'(A)') 'Error using yin.'
   stop
end if
line=''
n=len_trim(dfault)
do while (.TRUE.)
   write(AUX,'(A)') dfault
   AUX=adjustl(AUX)
   aux=aux(1:n)//']: '
   write(*,'(a,a,a)',advance='no') text//' [Default: ',trim(aux),' '
   write(*,'($)')
   read(*,'(a)') line
   if(line /= "") read(line,*) dfault
   dfault = to_upper(dfault)
   if(dfault(1:1) == 'Y' .OR. dfault(1:1) == 'N') exit
end do
end subroutine yin
!...............................................................................

Integer(kind=2) Function t2i (text)
character(LEN=*), INTENT(IN) :: text
!character(LEN=LEN(text)) :: aux
!aux=adjustl(text)
!read(aux,'(I4)') t2i
read(text,*) t2i
end function t2i

!...............................................................................

real(8) function t2e(text,flag)
character(len=*), INTENT(IN) :: text
integer, optional :: flag
read(text,*) t2e
end function t2e

end module utils
