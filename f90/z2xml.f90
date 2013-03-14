program z2xml

  use global
  use config
  use read_lists
  use z_read
  use xml_write
  implicit none

  character(len=80) :: input_dir='./'
  character(len=80) :: z_file=''
  character(len=80) :: xml_file=''
  character(len=80) :: config_file = 'config.xml'
  character(len=80) :: zsitename, basename, verbose=''
  type(Dimensions_t):: N
  type(UserInfo_t)  :: UserInfo
  type(Site_t)      :: zLocalSite, xmlLocalSite, xmlRemoteSite
  type(Channel_t), dimension(:), pointer      :: InputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputElectric
  type(Run_t), dimension(:), pointer     :: Run, RemoteRun
  type(Data_t), dimension(:), pointer         :: Data
  type(DataType_t), dimension(:), pointer     :: DataType, Estimate
  type(FreqInfo_t), dimension(:), allocatable :: F
  character(100), dimension(:), pointer       :: Notes
  complex(8), dimension(:,:,:), allocatable    :: TF
  real(8),    dimension(:,:,:), allocatable    :: TFVar
  character(10), dimension(:,:), allocatable   :: TFName
  complex(8), dimension(:,:,:), allocatable    :: InvSigCov
  complex(8), dimension(:,:,:), allocatable    :: ResidCov
  real(8),    dimension(:,:), allocatable      :: U,V ! rotation matrices

  logical           :: config_exists, site_list_exists
  logical			:: run_list_exists, channel_list_exists
  integer           :: i, j, k, l, narg, istat
  integer           :: nf, nch, nchin, nchout, nchoutE, nchoutH

  narg = command_argument_count()

  if (narg<1) then
     write(0,*) 'Please specify the name of the input Z-file'
     stop
  else if (narg>=1) then
     call get_command_argument(1,z_file)
  end if

  if (narg>1) then
     call get_command_argument(2,xml_file)
  else
     l = len_trim(z_file)
     basename = z_file(1:l-4)
     xml_file = trim(basename)//'.xml'
  end if

  if (narg>2) then
     call get_command_argument(3,verbose)
     if (index(verbose,'silent')>0) then
        silent = .true.
     end if
  end if

  ! Update output file name
  if (index(xml_file,'.')==0) then
 	xml_file = trim(xml_file)//'.xml'
  end if

  ! Look for configuration file in the input directory
  i = index(z_file,'/',.true.)
  if (i>0) then
    input_dir = z_file(1:i-1)
  end if
  config_file = trim(input_dir)//'/'//trim(config_file)
  if (.not. silent) then
    write(*,*) 'Reading configuration file ',config_file
  end if

  ! Read the configuration file, if it exists
  inquire (file=config_file,exist=config_exists)
  if (config_exists) then
  	call read_xml_config(config_file,UserInfo,DataType,Estimate,input_dir)
  else
  	write(0,*) 'Please provide an XML configuration file [config.xml].'
  	write(0,*) 'This file should reside together with the input data.'
  	write(0,*) 'An example configuration is provided below:'
  	write(0,*)
  	write(0,*) '<Configuration>'
  	write(0,*) '	<Project>USArray</Project>'
  	write(0,*) '	<Survey>TA</Survey>'
  	write(0,*) '	<YearCollected>2011</YearCollected>'
  	write(0,*) '	<Tags>impedance,tipper</Tags>' 
    write(0,*) '    <Citation>'
    write(0,*) '      <Title>USArray TA Magnetotelluric Transfer Functions</Title>'
    write(0,*) '      <Authors>Adam Schultz, Gary D. Egbert, Anna Kelbert</Authors>'
    write(0,*) '      <Year>2012</Year>'
    write(0,*) '      <DOI>Unassigned</DOI>'
    write(0,*) '    </Citation>'
  	write(0,*) '	<ReleaseStatus>Unrestricted Release</ReleaseStatus>'
  	write(0,*) '	<AcquiredBy>Zonge Inc.</AcquiredBy>'
  	write(0,*) '	<Creator>'
  	write(0,*) '		<Name>Anna Kelbert</Name>'
  	write(0,*) '		<Email>anya@coas.oregonstate.edu</Email>'
  	write(0,*) '		<Org>Oregon State University</Org>'
  	write(0,*) '		<OrgUrl>http://oregonstate.edu</OrgUrl>'
  	write(0,*) '	</Creator>'
  	write(0,*) '	<Submitter>'
  	write(0,*) '		<Name>Anna Kelbert</Name>'
  	write(0,*) '		<Email>anya@coas.oregonstate.edu</Email>'
  	write(0,*) '		<Org>Oregon State University</Org>'
  	write(0,*) '		<OrgUrl>http://oregonstate.edu</OrgUrl>'		
  	write(0,*) '	</Submitter>'
  	write(0,*) '	<ProcessedBy>Anna Kelbert</ProcessedBy>'
  	write(0,*) '	<ProcessingSoftware>'
  	write(0,*) '		<Name>EMTF 1.0</Name>'
  	write(0,*) '		<LastMod>1987-10-12</LastMod>'
  	write(0,*) '		<Author>Gary Egbert</Author>'
  	write(0,*) '	</ProcessingSoftware>'
  	write(0,*) '	<OrthogonalGeographic>1</OrthogonalGeographic>'
  	write(0,*) '	<RunList>Runs.xml</RunList>'
  	write(0,*) '	<SiteList>Sites.xml</SiteList>'
  	write(0,*) '	<ChannelList>Channels.xml</ChannelList>'
  	write(0,*) '<Configuration>'
  	write(0,*)
  	write(0,*) 'Project and YearCollected (if present) help identify'
    write(0,*) 'a product in SPUD. They should not contain spaces.'
  	write(0,*) 'The optional OrthogonalGeographic field, if true,'
  	write(0,*) 'will rotate the data to orthogonal geographic coords.'
  	write(0,*) 'Use with caution: information about the original '
  	write(0,*) 'measurement coordinates might be lost in the process.'
    write(0,*) 'Rotation only works if there are four or five channels.'
  	write(0,*) 'Leave the RunList and SiteList elements out or empty'
  	write(0,*) 'if you do not have XML lists for this experiment.'
  	stop
  end if

  ! Initialize site structures
  call init_site_info(zLocalSite)
  call init_site_info(xmlLocalSite)
  call init_site_info(xmlRemoteSite)

  ! Initialize dimensions
  call init_dimensions(N)

  ! Initialize input and output
  call initialize_xml_output(xml_file,'EM_TF')

  ! Read the Z-file in full

  call initialize_z_input(z_file)

  call read_z_header(zsitename, zLocalSite, UserInfo, N)


  call read_z_channels(InputMagnetic, OutputMagnetic, OutputElectric, N, zLocalSite%Declination)

  ! Define local dimensions
  nf = N%f
  nch = N%ch
  nchin = N%chin
  nchout = N%chout
  nchoutE = N%choutE
  nchoutH = N%choutH

  ! Allocate space for transfer functions
  allocate(F(nf), TF(nf,nch-2,2), TFVar(nf,nch-2,2), TFName(nch-2,2), stat=istat)
  allocate(InvSigCov(nf,2,2), ResidCov(nf,nch-2,nch-2), stat=istat)
  allocate(U(2,2), V(nch-2,nch-2), stat=istat)

  ! Initialize conversion to orthogonal geographic coords
  if (UserInfo%OrthogonalGeographic > 0) then
     call rotate_z_channels(InputMagnetic, OutputElectric, U, V, N)
  end if

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:), N)

     ! rotate to orthogonal geographic coordinates (generality limited to 4 or 5 channels)
     if (UserInfo%OrthogonalGeographic > 0) then
     	call rotate_z_period(U, V, TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:), N)
     end if

  end do

  ! Create generic Data variables
  write(*,*) 'Allocating data structure for ',size(DataType),' data types'
  allocate(Data(size(DataType)), stat=istat)
  do i=1,size(DataType)
    select case (DataType(i)%Output)
    case ('H')
        call init_data(Data(i),DataType(i),nf,nchin,nchoutH)
        Data(i)%Matrix = TF(:,1:nchoutH,:)
        Data(i)%Var = TFVar(:,1:nchoutH,:)
        Data(i)%InvSigCov = InvSigCov(:,:,:)
        Data(i)%ResidCov = ResidCov(:,1:nchoutH,1:nchoutH)
    case ('E')
        call init_data(Data(i),DataType(i),nf,nchin,nchoutE)
        Data(i)%Matrix = TF(:,nchoutH+1:nchout,:)
        Data(i)%Var = TFVar(:,nchoutH+1:nchout,:)
        Data(i)%InvSigCov = InvSigCov(:,:,:)
        Data(i)%ResidCov = ResidCov(:,nchoutH+1:nchout,nchoutH+1:nchout)
    case default
        write(0,*) 'Error: unable to initialize the data variable #',i
    end select
  end do

  call read_z_notes(Notes)

  ! Finished reading Z-file

  ! Read information for this site from a list. If successfully read,
  ! trust this information rather than that from the Z-file
  call read_site_list(UserInfo%SiteList, zLocalSite%ID, xmlLocalSite, site_list_exists)

  if (len_trim(xmlLocalSite%ID)>0) then

  	call add_xml_header(xmlLocalSite, UserInfo, Notes)
  else

  	call add_xml_header(zLocalSite, UserInfo, Notes)
  end if

  ! Read runs (e.g. start and end times) information for each of the runs
  ! which were used in processing the data found in the Z-file
  call read_run_list(UserInfo%RunList, zLocalSite%RunList, Run, run_list_exists)

  ! Field notes go first
  if (run_list_exists) then
  	do i=1,size(Run)
		call read_channel_list(UserInfo%ChannelList, Run(i)%ID, InputMagnetic, channel_list_exists)
        call read_channel_list(UserInfo%ChannelList, Run(i)%ID, OutputElectric, channel_list_exists)
		call read_channel_list(UserInfo%ChannelList, Run(i)%ID, OutputMagnetic, channel_list_exists)
		if (channel_list_exists) then
			call add_FieldNotes(Run(i),InputMagnetic,OutputElectric)
		else
			call add_FieldNotes(Run(i))
		end if
  	end do
  end if
  
  ! Processing notes follow
  if (UserInfo%RemoteRef) then
  	call read_site_list(UserInfo%SiteList, UserInfo%RemoteSiteID, xmlRemoteSite, site_list_exists)
	call read_run_list (UserInfo%RunList, xmlRemoteSite%RunList, RemoteRun, run_list_exists)
  	if (run_list_exists) then
		call add_ProcessingInfo(UserInfo, xmlRemoteSite, RemoteRun)
	else
		call add_ProcessingInfo(UserInfo, xmlRemoteSite)
	end if
  else
  	call add_ProcessingInfo(UserInfo)
  end if

  call new_element('StatisticalEstimates')
  do i=1,size(Estimate)
    call add_Estimate(Estimate(i))
  end do
  call end_element('StatisticalEstimates')

  call new_element('DataTypes')
  do i=1,size(DataType)
    call add_DataType(DataType(i))
  end do
  call end_element('DataTypes')

  call new_channel_block('InputChannels')
  do i=1,size(InputMagnetic)
     call add_Channel(InputMagnetic(i), location=.false.)
  end do
  call end_block('InputChannels')

  call new_channel_block('OutputChannels')
  do i=1,size(OutputMagnetic)
     call add_Channel(OutputMagnetic(i), location=.false.)
  end do
  do i=1,size(OutputElectric)
     call add_Channel(OutputElectric(i), location=.false.)
  end do
  call end_block('OutputChannels')

  ! Read and write frequency blocks: transfer functions, variance, covariance
  call initialize_xml_freq_block_output(nf)

  do k=1,nf

	  call new_data_block('Period',F(k))

      do i=1,size(Data)
        select case (Data(i)%Type%Output)
        case ('H')
            call add_Data(Data(i), InputMagnetic, OutputMagnetic, k)
            call add_Var(Data(i), InputMagnetic, OutputMagnetic, k)
            call add_InvSigCov(Data(i), InputMagnetic, k)
            call add_ResidCov(Data(i), OutputMagnetic, k)
        case ('E')
            call add_Data(Data(i), InputMagnetic, OutputElectric, k)
            call add_Var(Data(i), InputMagnetic, OutputElectric, k)
            call add_InvSigCov(Data(i), InputMagnetic, k)
            call add_ResidCov(Data(i), OutputElectric, k)
        case default
            write(0,*) 'Error: unable to write the data variable #',i
        end select
      end do

      call end_block('Period')

  end do

  call end_xml_freq_block_output

  call add_PeriodRange(F)

  ! Exit nicely
  deallocate(Run)
  if (associated(RemoteRun)) deallocate(RemoteRun)
  if (associated(Notes)) deallocate(Notes)
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric)
  deallocate(F, TF, TFVar, InvSigCov, ResidCov)
  do i = 1,size(DataType)
    call deall_data(Data(i))
  end do
  deallocate(Data)

  call end_z_input

  call end_xml_output('EM_TF')

  if (.not.silent) then
     write(*,*) 'Written to file: ',xml_file
  end if

end program z2xml
