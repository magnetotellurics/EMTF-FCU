module z_read

  use global
  use utils

  implicit none
  private

  integer                        :: zfile
  character(len=120)             :: temp
  integer                        :: ios,i,j
  integer, save                  :: nchout
  integer, parameter             :: nchin=2

  save   :: zfile

  public :: initialize_z_input
  public :: read_z_header
  public :: read_z_channels
  public :: read_z_period
  public :: rotate_z_channels
  public :: rotate_z_period
  public :: read_z_notes
  public :: end_z_input

contains

  subroutine initialize_z_input(fname,Info)
     character(len=*), intent(in) :: fname
     type(UserInfo_t), intent(inout), optional :: Info
     integer			  :: ios,i,l
     character(80)		  :: str

     ! passed an empty string
     if (fname == '') then
        zfile=5
        open (unit=zfile,iostat=ios)
     else
        zfile=55
        open (unit=zfile,file=fname,status='old',iostat=ios)
     end if

     if(ios/=0) then
        write(0,*) 'Error opening file:', fname
     endif

     if (present(Info)) then
        i = index(fname,'/',.true.)
        l = len_trim(fname)
        if (i>0) then
            Info%Basename = fname(i+1:l-4)
        else
            Info%Basename = fname(1:l-4)
        end if
     end if

  end subroutine initialize_z_input

  !--------------------------------------------------------------------
  ! Parses the station, as written into a Gary Egbert's Z-file, or the
  ! name of a Z-file, such as: WAA05bc_A4
  ! Assumes no extra characters at the end of this string, and uses
  ! the following convention:
  ! a) no underscore - assume single station
  ! b) 2-char after the underscore - insert a zero, e.g. A4 -> WAA04
  ! c) 3-char atter the underscore with 2 digits, e.g. A11 -> WAA11
  ! d) 4-char atter the underscore with 3 digits, e.g. C016 -> MC016
  ! e) <2-char or >4-char after the underscore - do not attempt to parse
  ! Most importantly, assumes a 5-digit site name!!!!!!
  ! Please manually correct the station name in your Z-file to satisfy
  ! these criteria before attempting to run this program.
  subroutine parse_z_site_name(sitename, SiteID, RemoteSiteID, RunList)
  	character(len=80), intent(in)    :: sitename
  	character(len=5), intent(out)    :: SiteID
  	character(len=5), intent(out)    :: RemoteSiteID
  	character(len=80), intent(out)   :: RunList
  	character(len=80)                :: info
	character(len=20)                :: list, abbrev
  	integer                          :: l, k, i

  	SiteID = toupper(sitename(1:1))//toupper(sitename(2:2))//toupper(sitename(3:3))//sitename(4:5)
	RunList = ' '

    ! Find spaces in the sitename
    i = index(trim(sitename),' ')
    if (i > 0) then
        l = i-1
    else
  	    l = len_trim(sitename)
  	end if

  	if (l>5) then
  		i = index(sitename,'_')
		if (i==0) then ! Single Station - create the run list and exit
			list = sitename(6:l)
			abbrev = ' '
		else ! Remote Reference - parse the name of the remote site
			list = sitename(6:i-1)
			abbrev = sitename(i+1:l)
		end if
		! In either case, make a list of run names, store them in a string
		do k=1,len_trim(list)
			RunList = trim(RunList)//' '//trim(SiteID)//list(k:k)
		end do
	end if

  	if (len_trim(abbrev)==0) then
  		RemoteSiteID = ' '
  	else if (len_trim(abbrev)==2) then
  		RemoteSiteID = SiteID(1:2)//toupper(abbrev(1:1))//'0'//abbrev(2:2)
  	else if (len_trim(abbrev)==3) then
  		if (isdigit(abbrev(3:3))) then ! e.g. KSP35b_K33
  			RemoteSiteID = SiteID(1:2)//toupper(abbrev(1:1))//abbrev(2:3)
  		else ! Account for additional characters at the end e.g. ORG05bc_H6x
   			RemoteSiteID = SiteID(1:2)//toupper(abbrev(1:1))//'0'//abbrev(2:2)
  		end if
    else if (len_trim(abbrev)==4) then
        if (isdigit(abbrev(3:3)) .and. isdigit(abbrev(4:4))) then ! e.g. MF011a_M012
            RemoteSiteID = SiteID(1:1)//toupper(abbrev(1:1))//abbrev(2:4)
        else if (isdigit(abbrev(3:3))) then ! e.g. KSP35b_K33x
            RemoteSiteID = SiteID(1:2)//toupper(abbrev(1:1))//abbrev(2:3)
        else ! Account for additional characters at the end e.g. ORG05bc_H6xy
            RemoteSiteID = SiteID(1:2)//toupper(abbrev(1:1))//'0'//abbrev(2:2)
        end if
    else if (len_trim(abbrev)>=5) then
        if (isdigit(abbrev(3:3)) .and. isdigit(abbrev(4:4)) .and. isdigit(abbrev(5:5))) then ! e.g. MF011a_MM012
            RemoteSiteID = toupper(abbrev(1:1))//toupper(abbrev(2:2))//abbrev(3:5)
        else if (isdigit(abbrev(3:3)) .and. isdigit(abbrev(4:4))) then ! e.g. MF011a_M012xcoh
            RemoteSiteID = SiteID(1:1)//toupper(abbrev(1:1))//abbrev(2:4)
        else if (isdigit(abbrev(3:3))) then ! e.g. KSP35b_K33xcoh
            RemoteSiteID = SiteID(1:2)//toupper(abbrev(1:1))//abbrev(2:3)
        else ! Account for additional characters at the end e.g. ORG05bc_H6xcoh
            RemoteSiteID = SiteID(1:2)//toupper(abbrev(1:1))//'0'//abbrev(2:2)
        end if
	else
		write(0,*) 'Unable to extract remote reference site from the string ',trim(sitename)
		RemoteSiteID = ' '
	end if

    i = index(sitename,'remote')
    l = len_trim(sitename)
    if (i > 0) then
        write(*,*) 'Using remote site ID from the Z-file header...'
        RemoteSiteID = sitename(i+7:l)
    end if

  end subroutine parse_z_site_name


  subroutine read_z_header(sitename, Site, Info, nf, nch)
    character(len=80), intent(out)   :: sitename
    type(Site_t),  intent(out)       :: Site
	type(UserInfo_t), intent(inout)  :: Info
	integer, intent(out)             :: nf
    integer, intent(out), optional   :: nch

	call init_site_info(Site)

    read (zfile,*) temp
    read (zfile,*) temp
    read (zfile,'(a80)') Info%RemoteRefType
    read (zfile,'(a12,a80)') temp, sitename

	call parse_z_site_name(sitename, Site%ID, Info%RemoteSiteID, Site%RunList)

    if (.not.silent) then
       write(*,*) trim(sitename),': local ',Site%ID,' remote ',Info%RemoteSiteID
    end if

	if (index(Info%RemoteRefType,'Remote Reference')>0) then
		if (len_trim(Info%RemoteSiteID)>0) then
			Info%RemoteRef = .TRUE.
		end if
	end if

	Info%ProcessingSoftware = 'EMTF'
	Info%ProcessingTag = sitename

    read (zfile,'(a120)',iostat=ios) temp
    i = index(temp,'coordinate')
    j = index(temp,'declination')

    read (temp(i+12:j-1),*) Site%Location%lat, Site%Location%lon
    read (temp(j+12:120),*) Site%Declination
    
    if(Site%Location%lon > 180.0d0) then
		Site%Location%lon = Site%Location%lon - 360.0d0
	end if

    read (zfile,'(a120)',iostat=ios) temp
    i = index(temp,'channels')
    j = index(temp,'frequencies')

    read (temp(i+9:j-1),*) nch
    read (temp(j+12:120),*) nf

    if (.not.silent) then
       write(*,*) 'Number of channels ', nch
       write(*,*) 'Number of periods  ', nf
    end if

  end subroutine read_z_header


  subroutine read_z_channels(Input, OutputH, OutputE, nch, decl)
    type(Channel_t), pointer, intent(inout) :: Input(:)
    type(Channel_t), pointer, intent(inout) :: OutputH(:)
    type(Channel_t), pointer, intent(inout) :: OutputE(:)
    integer, intent(in)                     :: nch
    real(8), intent(inout), optional        :: decl
    ! local
    type(Channel_t), pointer         :: Output(:)
    real(8)                          :: declination
    character(len=3)                 :: temp
    integer                          :: num, istat
    integer                          :: nchoutH, nchoutE
    character(len=80)                :: chname
    real                             :: orientation
    real                             :: tilt

    read (zfile,*) temp !read the first comment

    if (.not. present(decl)) then
    	declination = 0.0
    else
    	declination = decl
    end if

    nchout = nch - nchin
    nchoutE = 0
    nchoutH = 0

    write(*,*) 'Reading  input channels from Z-file: ',nchin

    if (nchin>0) then
        allocate(Input(nchin), stat=istat)
    end if
    do i=1,nchin
       read (zfile,*) num, orientation, tilt, temp, chname
       call init_channel_info(Input(i))
       Input(i)%ID = chname
       Input(i)%Type = channel_type(chname)
       Input(i)%orientation = orientation + declination
       Input(i)%tilt = tilt
       call init_channel_units(Input(i))
    end do

    write(*,*) 'Reading output channels from Z-file: ',nchout

    allocate(Output(nchout), stat=istat)
    do i=1,nchout
       read (zfile,*) num, orientation, tilt, temp, chname
       if (channel_type(chname) .eq. 'H') then
            nchoutH = nchoutH + 1
       elseif (channel_type(chname) .eq. 'E') then
            nchoutE = nchoutE + 1
       else
            write(0,*) 'Warning: unknown channel name ',trim(chname),' in Z-file'
       end if
       call init_channel_info(Output(i))
       Output(i)%ID = chname
       Output(i)%Type = channel_type(chname)
       Output(i)%orientation = orientation + declination
       Output(i)%tilt = tilt
       call init_channel_units(Output(i))
    end do

    ! we are assuming that all magnetic field channels come first
    ! (that's always the case in all Z-files to date)
    if (nchoutH>0) then
        allocate(OutputH(nchoutH), stat=istat)
        OutputH(:) = Output(1:nchoutH)
    end if
    if (nchoutE>0) then
        allocate(OutputE(nchoutE), stat=istat)
        OutputE(:) = Output(nchoutH+1:nchout)
    end if

    deallocate(Output, stat=istat)
    read (zfile, *) ! read empty line at the end
    write(*,*) 'Done reading channels from Z-file'

  end subroutine read_z_channels


  subroutine read_z_period(F, TF, TFVar, InvSigCov, ResidCov)
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

    read (zfile,'(a100)',iostat=ios) temp
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

    read (zfile,'(a100)',iostat=ios) temp
    i = index(temp,'data point')
    !j = index(temp,'sampling freq')

    read (temp(i+10:i+18),*) num_points
    !read (temp(i+10:j-1),'(i8)') num_points
    !read (temp(j+14:100),*) sampling_freq, units
    F%num_points = num_points

    TF=0.0d0
    read (zfile,*) temp !Transfer Functions
    do i=1,nchout
       do j=1,nchin
          read (zfile,'(2E12.3)',iostat=ios,advance='no') TF(i,j)
       end do
       read (zfile,*)
    end do

    InvSigCov=0.0d0
    read (zfile,*) temp !Inverse Coherent Signal
    do i=1,nchin
       do j=1,i
          read (zfile,'(2E12.3)',iostat=ios,advance='no') InvSigCov(i,j)
          if (j<i) then
             InvSigCov(j,i) = conjg(InvSigCov(i,j))
          end if
       end do
       read (zfile,*)
    end do

    ResidCov=0.0d0
    read (zfile,*) temp !Residual Covariance
    do i=1,nchout
       do j=1,i
          read (zfile,'(2E12.3)',iostat=ios,advance='no') ResidCov(i,j)
          if (j<i) then
             ResidCov(j,i) = conjg(ResidCov(i,j))
          end if
       end do
       read (zfile,*,iostat=ios)
    end do

    TFVar=0.0d0
    do i=1,nchout
       do j=1,nchin
          TFVar(i,j) = (ResidCov(i,i)*InvSigCov(j,j))/2
       end do
    end do

  end subroutine read_z_period

  ! For now, only use the electric field outputs for rotation:
  ! no need to rotate the vertical magnetic field
  subroutine rotate_z_channels(Input, OutputE, U, V, azimuth)
    type(Channel_t), dimension(:), intent(inout)   :: Input
    type(Channel_t), dimension(:), intent(inout)   :: OutputE
   	real(8),         dimension(:,:), intent(out)   :: U, V ! rotation matrices
   	real(8),         optional, intent(inout)       :: azimuth ! new azimuth
    ! local variables
    real(8)                                        :: theta0
    real(8),         dimension(:), allocatable     :: theta, thetanew
    integer                                        :: i, j, istat

	! note: U and V must be allocated before calling this subroutine
    allocate(theta(2), thetanew(2), stat = istat)

	if (.not. present(azimuth)) then
    	theta0 = 0.0
    else
    	theta0 = azimuth
	end if

    ! create the rotation matrix for the two input channels first;
    ! assuming Hx and Hy are the input channels, - pretty much always the case
    theta(1) = Input(1)%orientation; thetanew(1) = theta0
    theta(2) = Input(2)%orientation; thetanew(2) = theta0 + 90.0

    call identity(U)

    U(1,1) = cos(D2R*(theta(1) - theta0))
    U(1,2) = sin(D2R*(theta(1) - theta0))
    U(2,1) = cos(D2R*(theta(2) - theta0))
    U(2,2) = sin(D2R*(theta(2) - theta0))

	call inverse22(U,U)

	! define the new channel orientations
    do i=1,size(Input)
		Input(i)%orientation = thetanew(i)
    end do

    ! now do output channels;
    ! we only rotate electric field channels, and we can only do two at a time
    if (size(OutputE) == 2) then
	    theta(1) = OutputE(1)%orientation; thetanew(1) = theta0
	    theta(2) = OutputE(2)%orientation; thetanew(2) = theta0 + 90.0
	else
        write(*,*) 'Unable to create rotation matrices: too many electric channels...'
        stop
    end if

	call identity(V)

	! if there are 3 or more output channels total, assume that H channels come first
    if (nchout == 2) then

    	V(1,1) = cos(D2R*(theta(1) - theta0))
    	V(1,2) = cos(D2R*(theta(2) - theta0))
    	V(2,1) = sin(D2R*(theta(1) - theta0))
    	V(2,2) = sin(D2R*(theta(2) - theta0))

    else if (nchout > 2) then

		V(nchout-1,nchout-1) = cos(D2R*(theta(1) - theta0))
		V(nchout-1,nchout  ) = cos(D2R*(theta(2) - theta0))
		V(nchout  ,nchout-1) = sin(D2R*(theta(1) - theta0))
		V(nchout  ,nchout  ) = sin(D2R*(theta(2) - theta0))

	end if

    do i=1,size(OutputE)
		OutputE(i)%orientation = thetanew(i)
    end do

	deallocate(theta, thetanew)

  end subroutine rotate_z_channels


  subroutine rotate_z_period(U, V, TF, TFVar, InvSigCov, ResidCov)
   	real(8),         dimension(:,:), intent(in)    :: U ! rotation matrix size 2x2 for input channels
   	real(8),         dimension(:,:), intent(in)    :: V ! rotation matrix size (nch-2)x(nch-2) for output channels
    complex(8),      dimension(:,:), intent(inout) :: TF
    real(8),         dimension(:,:), intent(inout) :: TFVar
    complex(8),      dimension(:,:), intent(inout) :: InvSigCov
    complex(8),      dimension(:,:), intent(inout) :: ResidCov
    ! local variables
    integer                                        :: i, j

	! rotate the transfer functions and covariance matrices (using * gives strange results!)
	TF = matmul(V, matmul(TF,transpose(U)))
	InvSigCov = matmul(U, matmul(InvSigCov,transpose(U)))
	ResidCov = matmul(V, matmul(ResidCov,transpose(V)))

	! finally, update the variances
    do i=1,nchout
       do j=1,nchin
          TFVar(i,j) = (ResidCov(i,i)*InvSigCov(j,j))/2
       end do
    end do

  end subroutine rotate_z_period


  subroutine read_z_notes(Notes)
  	character(100), dimension(:), pointer :: Notes
	integer                               :: nline

	read (zfile,'(i8)',iostat=ios) nline
	if (ios/=0) then
		if (associated(Notes)) deallocate(Notes)
		return
	end if

	if (associated(Notes)) deallocate(Notes)

	allocate(Notes(nline))

	do i=1,nline
		read (zfile,'(a100)',iostat=ios) Notes(i)
	end do

  end subroutine read_z_notes


  subroutine end_z_input

    close(zfile)
  end subroutine end_z_input

end module z_read
