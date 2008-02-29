! **********************************************************************
! All subroutines licensed under the terms of LGPL or GPLv2 or above,
! as specified in the subroutine headers.
! **********************************************************************

module utils

implicit none

! **********************************************************************
!  ascii character functions

public                     ::  toupper, isdigit

! **********************************************************************
!  utility subroutines

public                     ::  deg2char, sortidx


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


! **********************************************************************
! Convert latitude or longitude from a decimal to the 
! sexagesimal system (degrees, minutes & seconds);
! the result is a character string length 16.
! Written by: Anna Kelbert, 3 Nov 2007
! This subroutine is distributed under the terms of 
! the GNU Lesser General Public License.

  function deg2char(loc) result (cloc)

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
	write(cmin,'(i2)') m

	s = ((aloc-d)*60-m)*60
	write(csec,'(f5.2)') s

	cloc = cdeg//':'//cmin//':'//csec	

! ----------------------------------------------------------------------
  
  end function deg2char

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

end module utils