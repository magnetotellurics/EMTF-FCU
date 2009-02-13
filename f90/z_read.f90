module z_read

  use global
  use utils

  implicit none
  private

  integer                        :: zfile
  character(len=120)             :: temp
  integer                        :: ios,i,j

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

  subroutine initialize_z_input(fname)
     character(len=*), intent(in) :: fname
     integer			  :: ios
     character(20)		  :: str

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

  end subroutine initialize_z_input

  !--------------------------------------------------------------------
  ! Parses the station, as written into a Gary Egbert's Z-file, or the
  ! name of a Z-file, such as: WAA05bc_A4
  ! Assumes no extra characters at the end of this string, and uses
  ! the following convention:
  ! a) no underscore - assume single station
  ! b) 2-char after the underscore - insert a zero, e.g. A4 -> WAA04
  ! c) 3-char atter the underscore - assume 2 digits, e.g. A11 -> WAA11
  ! d) <2-char or >3-char after the underscore - do not attempt to parse
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
  	else if (len_trim(abbrev)>=3) then
  		if (isdigit(abbrev(3:3))) then
  			RemoteSiteID = SiteID(1:2)//toupper(abbrev(1:1))//abbrev(2:3)
  		else ! Account for additional characters at the end e.g. ORG05bc_H6x
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


  subroutine read_z_header(sitename, Site, Info)
    character(len=80), intent(out)   :: sitename
    type(Site_t),  intent(out)       :: Site
	type(RemoteRef_t), intent(out)   :: Info

	call init_remote_ref(Info)
	call init_site_info(Site)

    read (zfile,*) temp
    read (zfile,*) temp
    read (zfile,'(a80)') Info%remote_ref_type
    read (zfile,'(a12,a80)') temp, sitename

	call parse_z_site_name(sitename, Site%ID, Info%remote_site_id, Site%RunList)

    if (.not.silent) then
       write(*,*) trim(sitename),': local ',Site%ID,' remote ',Info%remote_site_id
    end if

	if (index(Info%remote_ref_type,'Remote Reference')>0) then
		if (len_trim(Info%remote_site_id)>0) then
			Info%remote_ref = .TRUE.
		end if
	end if

	!Info%processed_by = processed_by
	Info%processing_id = sitename
	Info%software = 'EMTF'

    read (zfile,'(a120)',iostat=ios) temp
    i = index(temp,'coordinate')
    j = index(temp,'declination')

    read (temp(i+12:j-1),*) Site%Location%lat, Site%Location%lon
    read (temp(j+12:120),*) Site%Declination

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


  subroutine read_z_channels(Input, Output, decl)
    type(Channel_t), dimension(:), intent(inout) :: Input
    type(Channel_t), dimension(:), intent(inout) :: Output
    real(8), intent(inout), optional             :: decl
    real(8)                          :: declination
    character(len=3)                 :: temp
    integer                          :: num
    character(len=80)                :: chname
    real                             :: orientation
    real                             :: tilt

    read (zfile,*) temp !read the first comment

    if (.not. present(decl)) then
    	declination = 0.0
    else
    	declination = decl
    end if

    do i=1,2
       read (zfile,*) num, orientation, tilt, temp, chname
       Input(i)%ID = chname
       Input(i)%orientation = orientation + declination
       Input(i)%tilt = tilt
       call init_channel_units(Input(i))
    end do

    do i=1,nch-2
       read (zfile,*) num, orientation, tilt, temp, chname
       Output(i)%ID = chname
       Output(i)%orientation = orientation + declination
       Output(i)%tilt = tilt
       call init_channel_units(Output(i))
    end do

    read (zfile, *) ! read empty line at the end

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

    read (temp(i+8:i+24),*) period
    !read (temp(i+8:j-1),*) period
    !read (temp(j+16:100),'(i3)') dec_level
    F%value = period   !1.0d0/period
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
    do i=1,nch-2
       do j=1,2
          read (zfile,'(2E12.3)',iostat=ios,advance='no') TF(i,j)
       end do
       read (zfile,*)
    end do

    InvSigCov=0.0d0
    read (zfile,*) temp !Inverse Coherent Signal
    do i=1,2
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
    do i=1,nch-2
       do j=1,i
          read (zfile,'(2E12.3)',iostat=ios,advance='no') ResidCov(i,j)
          if (j<i) then
             ResidCov(j,i) = conjg(ResidCov(i,j))
          end if
       end do
       read (zfile,*,iostat=ios)
    end do

    TFVar=0.0d0
    do i=1,nch-2
       do j=1,2
          TFVar(i,j) = (ResidCov(i,i)*InvSigCov(j,j))/2
       end do
    end do

  end subroutine read_z_period


  subroutine rotate_z_channels(Input, Output, U, V, azimuth)
    type(Channel_t), dimension(:), intent(inout)   :: Input
    type(Channel_t), dimension(:), intent(inout)   :: Output
   	real(8),         dimension(:,:), intent(out)   :: U, V ! rotation matrices
   	real(8),         optional, intent(inout)       :: azimuth ! new azimuth
    ! local variables
    real(8)                                        :: theta0
    real(8),         dimension(:), allocatable     :: theta, thetanew
    integer                                        :: nch, i, j, istat

    nch = size(Input) + size(Output)

	! note: U and V must be allocated before calling this subroutine
    allocate(theta(nch), thetanew(nch), stat = istat)

	if (.not. present(azimuth)) then
    	theta0 = 0.0
    else
    	theta0 = azimuth
	end if

    ! assuming Hx and Hy are the input channels, - pretty much always the case
    theta(1) = Input(1)%orientation; thetanew(1) = theta0
    theta(2) = Input(2)%orientation; thetanew(2) = theta0 + 90.0

    U(1,1) = cos(D2R*(theta(1) - theta0))
    U(1,2) = sin(D2R*(theta(1) - theta0))
    U(2,1) = cos(D2R*(theta(2) - theta0))
    U(2,2) = sin(D2R*(theta(2) - theta0))

	call inverse22(U,U);

	! if there are 3 output channels, assume the first is Hz; otherwise no Hz
	if (size(Output) == 3) then

		theta(3) = Output(1)%orientation; thetanew(3) = 0.0
		theta(4) = Output(2)%orientation; thetanew(4) = theta0
		theta(5) = Output(3)%orientation; thetanew(5) = theta0 + 90.0

		V(:,:) = 0.0
		V(1,1) = 1.0
		V(2,2) = cos(D2R*(theta(4) - theta0))
		V(2,3) = cos(D2R*(theta(5) - theta0))
		V(3,2) = sin(D2R*(theta(4) - theta0))
		V(3,3) = sin(D2R*(theta(5) - theta0))

	else if (size(Output) == 2) then

		theta(3) = Output(1)%orientation; thetanew(3) = theta0
		theta(4) = Output(2)%orientation; thetanew(4) = theta0 + 90.0

    	V(1,1) = cos(D2R*(theta(3) - theta0))
    	V(1,2) = cos(D2R*(theta(4) - theta0))
    	V(2,1) = sin(D2R*(theta(3) - theta0))
    	V(2,2) = sin(D2R*(theta(4) - theta0))

	else
		write(*,*) 'Unable to create rotation matrices: too many channels...'
		deallocate(theta, thetanew)
		call identity(U)
		call identity(V)
		return
	end if

	! define the new channel orientations
    do i=1,2
		Input(i)%orientation = thetanew(i)
    end do

    do i=1,nch-2
		Output(i)%orientation = thetanew(i+2)
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
    do i=1,size(V,1)
       do j=1,2
          TFVar(i,j) = (ResidCov(i,i)*InvSigCov(j,j))/2
       end do
    end do

  end subroutine rotate_z_period


  subroutine read_z_notes(Notes)
  	character(100), dimension(:), pointer :: Notes
	integer                               :: n

	read (zfile,'(i8)',iostat=ios) n
	if (ios/=0) then
		if (associated(Notes)) deallocate(Notes)
		return
	end if

	if (associated(Notes)) deallocate(Notes)

	allocate(Notes(n))

	do i=1,n
		read (zfile,'(a100)',iostat=ios) Notes(i)
	end do

  end subroutine read_z_notes


  subroutine end_z_input

    close(zfile)
  end subroutine end_z_input

end module z_read
