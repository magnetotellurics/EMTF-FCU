program edi2xml

  use global
  use config
  use edi_read
  use xml_write
  implicit none

  character(len=80) :: input_dir='./'
  character(len=80) :: edi_file=''
  character(len=80) :: xml_file=''
  character(len=80) :: config_file = 'config.xml'
  character(len=80) :: edisitename, basename, verbose=''
  type(RemoteRef_t) :: Info
  type(UserInfo_t)  :: UserInfo
  type(Site_t)      :: ediLocalSite, xmlLocalSite, xmlRemoteSite
  type(Run_t), dimension(:), pointer     :: Run, RemoteRun
  type(FreqInfo_t), dimension(:), allocatable :: F
  type(Channel_t), dimension(:), pointer	  :: InputChannel
  type(Channel_t), dimension(:), pointer	  :: OutputChannel
  character(100), dimension(:), pointer       :: Notes
  complex(8), dimension(:,:,:), allocatable    :: TF
  real(8),    dimension(:,:,:), allocatable    :: TFVar
  character(10), dimension(:,:), allocatable   :: TFName
  complex(8), dimension(:,:,:), allocatable    :: InvSigCov
  complex(8), dimension(:,:,:), allocatable    :: ResidCov
  real(8),    dimension(:,:), allocatable      :: U,V ! rotation matrices
  logical           :: config_exists, site_list_exists
  logical			:: run_list_exists, channel_list_exists
  integer           :: i, j, k, n, l, istat

  n = command_argument_count()

  if (n<1) then
     write(0,*) 'Please specify the name of the input Z-file'
     stop
  else if (n>=1) then
     call get_command_argument(1,edi_file)
  end if

  if (n>1) then
     call get_command_argument(2,xml_file)
  else
     l = len_trim(edi_file)
     basename = edi_file(1:l-4)
     xml_file = trim(basename)//'.xml'
  end if

  if (n>2) then
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
  i = index(edi_file,'/',.true.)
  if (i>0) then
    input_dir = edi_file(1:i-1)
  end if
  config_file = trim(input_dir)//'/'//trim(config_file)
  if (.not. silent) then
    write(*,*) 'Reading configuration file ',config_file
  end if

  ! Read the configuration file, if it exists
  inquire (file=config_file,exist=config_exists)
  if (config_exists) then
  	call read_xml_config(config_file,UserInfo,input_dir)
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
    write(0,*) 'a product in SPADE. They should not contain spaces.'
  	write(0,*) 'The optional OrthogonalGeographic field, if true,'
  	write(0,*) 'will rotate the data to orthogonal geographic coords.'
  	write(0,*) 'Use with caution: information about the original '
  	write(0,*) 'measurement coordinates might be lost in the process.'
    write(0,*) 'Rotation only works if there are four or five channels.'
  	write(0,*) 'Leave the RunList and SiteList elements out or empty'
  	write(0,*) 'if you do not have XML lists for this experiment.'
  	stop
  end if

  ! Initialize input and output
  call initialize_xml_output(xml_file,'EM_TF')

  ! Read the Z-file in full

  call initialize_edi_input(edi_file)

  call read_edi_header(edisitename, ediLocalSite, Info)

  ! Allocate space for channels and transfer functions
  allocate(InputChannel(2), OutputChannel(nch-2), stat=istat)
  allocate(F(nf), TF(nf,nch-2,2), TFVar(nf,nch-2,2), TFName(nch-2,2), stat=istat)
  allocate(InvSigCov(nf,2,2), ResidCov(nf,nch-2,nch-2), stat=istat)
  allocate(U(2,2), V(nch-2,nch-2), stat=istat)

  call read_edi_channels(InputChannel, OutputChannel, ediLocalSite%Declination)

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_edi_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

  end do

  call read_edi_info(Notes)

  ! Finished reading Z-file

  if (len_trim(xmlLocalSite%ID)>0) then

  	call add_xml_header(xmlLocalSite, UserInfo, Info, Notes)
  else

  	call add_xml_header(ediLocalSite, UserInfo, Info, Notes)
  end if

  ! Processing notes follow
  if (Info%remote_ref) then
	call add_ProcessingInfo(UserInfo, Info, xmlRemoteSite)
  else
  	call add_ProcessingInfo(UserInfo, Info)
  end if

  call new_channel_block('InputChannels')
  do i=1,2
     call add_Channel(InputChannel(i), location=.false.)
  end do
  call end_block('InputChannels')

  call new_channel_block('OutputChannels')
  do i=1,nch-2
     call add_Channel(OutputChannel(i), location=.false.)
  end do
  call end_block('OutputChannels')

  ! Read and write frequency blocks: transfer functions, variance, covariance
  call initialize_xml_freq_block_output(nf)

  do k=1,nf

	 call new_data_block('Period',F(k))

     call add_TF(TF(k,:,:), InputChannel, OutputChannel)
     call add_TFVar(TFVar(k,:,:), InputChannel, OutputChannel)
     call add_InvSigCov(InvSigCov(k,:,:), InputChannel)
     call add_ResidCov(ResidCov(k,:,:), OutputChannel)

     call end_block('Period')

  end do

  call end_xml_freq_block_output

  !call add_PeriodRange(F)

  ! Exit nicely
  deallocate(Run)
  if (associated(RemoteRun)) deallocate(RemoteRun)
  if (associated(Notes)) deallocate(Notes)
  deallocate(InputChannel, OutputChannel)
  deallocate(F, TF, TFVar, InvSigCov, ResidCov)

  call end_edi_input

  call end_xml_output('EM_TF')

  if (.not.silent) then
     write(*,*) 'Written to file: ',xml_file
  end if

end program edi2xml
