module edi_read

  use global
  use utils

  implicit none
  private

  integer                        :: edifile
  character(len=120)             :: temp
  integer                        :: ios,i,j

  save   :: edifile

  public :: initialize_edi_input
  public :: read_edi_header
  public :: read_edi_channels
  public :: read_edi_period
  public :: read_edi_info
  public :: end_edi_input

contains

  subroutine initialize_edi_input(fname)
     character(len=*), intent(in) :: fname
     integer			  :: ios
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

  end subroutine initialize_edi_input

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
  subroutine parse_edi_site_name(sitename, SiteID, RemoteSiteID, RunList)
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

  end subroutine parse_edi_site_name


  subroutine read_edi_header(sitename, Site, Info)
    character(len=80), intent(out)   :: sitename
    type(Site_t),  intent(out)       :: Site
	type(RemoteRef_t), intent(out)   :: Info

	call init_remote_ref(Info)
	call init_site_info(Site)

    read (edifile,*) temp
    read (edifile,*) temp
    read (edifile,'(a80)') Info%remote_ref_type
    read (edifile,'(a12,a80)') temp, sitename

	call parse_edi_site_name(sitename, Site%ID, Info%remote_site_id, Site%RunList)

    if (.not.silent) then
       write(*,*) trim(sitename),': local ',Site%ID,' remote ',Info%remote_site_id
    end if

	if (index(Info%remote_ref_type,'Remote Reference')>0) then
		if (len_trim(Info%remote_site_id)>0) then
			Info%remote_ref = .TRUE.
		end if
	end if

	!Info%processed_by = processed_by
	Info%processing_tag = sitename
	!Info%software = 'EMTF'

    read (edifile,'(a120)',iostat=ios) temp
    i = index(temp,'coordinate')
    j = index(temp,'declination')

    read (temp(i+12:j-1),*) Site%Location%lat, Site%Location%lon
    read (temp(j+12:120),*) Site%Declination
    
    if(Site%Location%lon > 180.0d0) then
		Site%Location%lon = Site%Location%lon - 360.0d0
	end if

    read (edifile,'(a120)',iostat=ios) temp
    i = index(temp,'channels')
    j = index(temp,'frequencies')

    read (temp(i+9:j-1),*) nch
    read (temp(j+12:120),*) nf

    if (.not.silent) then
       write(*,*) 'Number of channels ', nch
       write(*,*) 'Number of periods  ', nf
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
