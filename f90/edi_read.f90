module edi_read

  use global
  use utils

  implicit none
  private

  integer                        :: edifile
  character(len=200)             :: temp
  logical                        :: new_block, new_section
  character(len=80)              :: this_block, this_section
  integer                        :: ios,i,j

  save   :: edifile
  save   :: new_block, new_section
  save   :: this_block, this_section

  public :: initialize_edi_input
  public :: read_edi_header
  public :: read_edi_channels
  public :: read_edi_period
  public :: read_edi_info
  public :: end_edi_input


contains

  subroutine initialize_edi_input(fname,sitename)
     character(len=*), intent(in)   :: fname
     character(len=80), intent(out) :: sitename
     integer			  :: i,ios
     character(20)		  :: str

     ! passed an empty string
     if (fname == '') then
        edifile=5
        open (unit=edifile,iostat=ios)
     else
        edifile=55
        open (unit=edifile,file=fname,status='old',iostat=ios)
     end if

     if(ios/=0) then
        write(0,*) 'Error opening file:', fname
     endif

     ! parse the EDI file name for site name:
     ! the only robust source for a unique ID
     i = index(trim(fname),'.edi')
     if (i > 0) then
        sitename = trim(fname(1:i-1))
        if (.not.silent) then
            write(6,*) 'Initialized reading site ',trim(sitename),' from EDI file'
        end if
     else
        write(0,*) 'Error: ',trim(fname),' is not an EDI file'
     end if

  end subroutine initialize_edi_input


  subroutine parse_edi_line(line, var, value)
    character(len=200), intent(in)   :: line
    character(len=200), intent(out)  :: var, value
    ! local
    character(len=1)                 :: edichar1, edichar2
    integer                          :: i, N=200

    ! Does this contain a new block? new section? a comment?
    ! Use the first two non-space characters to determine
    new_block = .false.
    new_section = .false.
    temp = adjustl(line)
    edichar1 = temp(1:1)
    edichar2 = temp(2:2)
    if (edichar1 == '>') then
        select case (edichar2)
        case ('=')
            new_section = .true.
            this_section = trim(adjustl(temp(3:N)))
            var = 'SECTION'
            value = this_section
            return
        case ('!')
            var = 'COMMENT'
            value = trim(adjustl(temp(3:N)))
            return
        case default
            ! this block reads the first word in line
            ! but save full info in value
            new_block = .true.
            read (temp(2:N),*) this_block
            var = 'BLOCK'
            value = trim(adjustl(temp(2:N)))
            return
        end select
    end if

    ! In all other circumstances, continue parsing the line
    i = index(trim(temp),'=')
    if (i > 0) then
        var = trim(temp(1:i-1))
        value = trim(adjustl(temp(i+1:N)))
        edichar1 = value(1:1)
        if (isdigit(edichar1)) then
            ! just save the full line; could be date or anything
        elseif (edichar1 == char(ascii_qtm)) then
            ! find the second double quote and read what's in between
            i = index(trim(value(2:N)),char(ascii_qtm))
            value = trim(value(2:i))
         else
            ! read the first word in line as the value
            read (temp(i+1:N),*) value
        end if
    else
        ! Note: empty info line is still an info line;
        ! this preserves the formatting
        if (trim(this_block) == 'INFO') then
            var = 'INFO'
        elseif (isempty(line)) then
            var = 'EMPTY'
        else
            var = 'DATA'
        end if
        value = line
    end if

  end subroutine parse_edi_line


  subroutine read_edi_header(sitename, Site, Info)
    character(len=80), intent(inout) :: sitename
    type(Site_t),  intent(out)       :: Site
	type(RemoteRef_t), intent(out)   :: Info
	! local
    character(len=200)               :: line, var, value

	call init_remote_ref(Info)
	call init_site_info(Site)

    read (edifile,'(a200)',iostat=ios) line !read the first line in file
    call parse_edi_line(line,var,value)
    if (.not. (trim(var) == 'BLOCK' .or. trim(value) == 'HEAD')) then
        write(0,*) 'Error reading the first line of EDI header:'
        write(0,*) line
    end if

	do
        read (edifile,'(a200)',iostat=ios) line
	    if (ios /= 0) then
	        exit
	    end if
        call parse_edi_line(line,var,value)
        if (.not.silent) then
            write(*,*) 'Reading ',trim(var),' line: ',trim(value)
        end if
    end do

    if(Site%Location%lon > 180.0d0) then
		Site%Location%lon = Site%Location%lon - 360.0d0
	end if

  end subroutine read_edi_header


  subroutine read_edi_channels(Input, Output, decl)
    type(Channel_t), dimension(:), intent(inout) :: Input
    type(Channel_t), dimension(:), intent(inout) :: Output
    real(8), intent(inout), optional             :: decl
    real(8)                          :: declination
    character(len=3)                 :: temp
    integer                          :: num
    character(len=80)                :: chname
    real                             :: orientation
    real                             :: tilt

    read (edifile,*) temp !read the first comment

    if (.not. present(decl)) then
    	declination = 0.0
    else
    	declination = decl
    end if

    do i=1,2
       read (edifile,*) num, orientation, tilt, temp, chname
       call init_channel_info(Input(i))
       Input(i)%ID = chname
       Input(i)%orientation = orientation + declination
       Input(i)%tilt = tilt
       call init_channel_units(Input(i))
    end do

    do i=1,nch-2
       read (edifile,*) num, orientation, tilt, temp, chname
       call init_channel_info(Output(i))
       Output(i)%ID = chname
       Output(i)%orientation = orientation + declination
       Output(i)%tilt = tilt
       call init_channel_units(Output(i))
    end do

    read (edifile, *) ! read empty line at the end

  end subroutine read_edi_channels


  subroutine read_edi_period(F, TF, TFVar, InvSigCov, ResidCov)
    type(FreqInfo_t),           intent(out)   :: F
    complex(8), dimension(:,:), intent(inout) :: TF
    real(8),    dimension(:,:), intent(inout) :: TFVar
    complex(8), dimension(:,:), intent(inout) :: InvSigCov
    complex(8), dimension(:,:), intent(inout) :: ResidCov
    real(8)           :: period
    integer           :: dec_level
    integer           :: num_points
    real              :: sampling_freq
    character(len=10) :: units

    read (edifile,'(a100)',iostat=ios) temp
    i = index(temp,'period')
    !j = index(temp,'decimation level')

	call init_freq_info(F)

    read (temp(i+8:i+24),*) period
    !read (temp(i+8:j-1),*) period
    !read (temp(j+16:100),'(i3)') dec_level
    F%value = period   !1.0d0/period
    F%units = 'secs'   ! Hz
    F%info_type  = 'period' !'frequency'
    !F%dec_level = dec_level

    read (edifile,'(a100)',iostat=ios) temp
    i = index(temp,'data point')
    !j = index(temp,'sampling freq')

    read (temp(i+10:i+18),*) num_points
    !read (temp(i+10:j-1),'(i8)') num_points
    !read (temp(j+14:100),*) sampling_freq, units
    F%num_points = num_points

    TF=0.0d0
    read (edifile,*) temp !Transfer Functions
    do i=1,nch-2
       do j=1,2
          read (edifile,'(2E12.3)',iostat=ios,advance='no') TF(i,j)
       end do
       read (edifile,*)
    end do

    InvSigCov=0.0d0
    read (edifile,*) temp !Inverse Coherent Signal
    do i=1,2
       do j=1,i
          read (edifile,'(2E12.3)',iostat=ios,advance='no') InvSigCov(i,j)
          if (j<i) then
             InvSigCov(j,i) = conjg(InvSigCov(i,j))
          end if
       end do
       read (edifile,*)
    end do

    ResidCov=0.0d0
    read (edifile,*) temp !Residual Covariance
    do i=1,nch-2
       do j=1,i
          read (edifile,'(2E12.3)',iostat=ios,advance='no') ResidCov(i,j)
          if (j<i) then
             ResidCov(j,i) = conjg(ResidCov(i,j))
          end if
       end do
       read (edifile,*,iostat=ios)
    end do

    TFVar=0.0d0
    do i=1,nch-2
       do j=1,2
          TFVar(i,j) = (ResidCov(i,i)*InvSigCov(j,j))/2
       end do
    end do

  end subroutine read_edi_period


  subroutine read_edi_info(Notes)
  	character(100), dimension(:), pointer :: Notes
	integer                               :: n

	read (edifile,'(i8)',iostat=ios) n
	if (ios/=0) then
		if (associated(Notes)) deallocate(Notes)
		return
	end if

	if (associated(Notes)) deallocate(Notes)

	allocate(Notes(n))

	do i=1,n
		read (edifile,'(a100)',iostat=ios) Notes(i)
	end do

  end subroutine read_edi_info


  subroutine end_edi_input

    close(edifile)
  end subroutine end_edi_input

end module edi_read
