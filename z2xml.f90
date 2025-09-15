! **********************************************************************
! z2xml reads in electromagnetic tranfer functions (EMTFs) in the Egbert
! Z-file format; rotates to chosen coordinate system; writes an XML file.
!
! Example usage:
! >  ./z2xml filename.zrr
! converts to filename.xml by default. Does not rotate.
! >  ./z2xml filename.zrr filename.xml [verbose|silent] 0.0
! to rotate to geographic North.
! >  ./z2xml filename.zrr filename.xml [verbose|silent] sitelayout
! to rotate to original site layout as saved in the channels block.
!
! Requires a local config.xml file with metadata that's missing from
! the Z-file. The config.xml file supplies survey-specific metadata;
! only one is needed for each survey. Note that 'SurveyDOI' sets a
! unique data object identifier for the survey, which will then get
! minted at IRIS upon submission to IRIS EMTF database. Do not make one
! unless you plan to submit to IRIS. Finally, note that the 'Tags'
! element is critical for file parsing, so make sure that you have the
! right data types there. Type ./z2xml to print an example config.xml.
!
! Optional 'SiteList', 'RunList' and 'ChannelList' files may be supplied
! through config.xml for additional metadata. See documentation.
!
! Note: Rotation of Z-files is mathematically valid since the full
! error covariances are present in this file format. This information
! also gets propagated to EMTF XML.
!
! Component of EMTF File Conversion Utilities 2018 (c) A. Kelbert
! **********************************************************************

program z2xml

  use global
  use config
  use rotation
  use read_lists
  use z_read
  use xml_write
  implicit none

  character(len=200):: input_dir='./'
  character(len=200):: z_file=''
  character(len=200):: xml_file=''
  character(len=80) :: config_file = 'config.xml'
  character(len=200):: zsitename, basename, verbose='',rotinfo=''
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

  l = len_trim(z_file)
  basename = z_file(1:l-4)
  if (narg>1) then
     call get_command_argument(2,xml_file)
  else
     xml_file = trim(basename)//'.xml'
  end if

  if (index(z_file(l-2:l),'zmm')>0) then ! different logic for multistation metadata
    multistation = .true.
  else
    multistation = .false.
  end if

  silent = .false.
  if (narg>2) then
     call get_command_argument(3,verbose)
     if (index(verbose,'silent')>0) then
        silent = .true.
     end if
  end if

  if (narg>3) then
    rotate = .true.
    call get_command_argument(4,rotinfo)
    if (index(rotinfo,'original')>0 .or. index(rotinfo,'sitelayout')>0) then
        write(0,*) 'Unable to rotate Z-file to site layout: can only rotate to specified orthogonal coordinates. Exiting...'
        stop
    else
        read(rotinfo, *) azimuth
        if ((azimuth)<0.01) then
           write(*,*) 'Rotate ',trim(basename),' to orthogonal geographic coords. '
        else
           write(*,*) 'Rotate ',trim(basename),' to the new azimuth ',azimuth
        end if
        orthogonalORsitelayout = 'orthogonal'
    end if
  else
    write(*,*) 'No further rotation requested for ',trim(basename),'. Using original coords. '
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
    write(0,*) '      <Authors>Schultz, A., G. D. Egbert, A. Kelbert</Authors>'
    write(0,*) '      <Year>2006-2016</Year>'
    write(0,*) '      <SurveyDOI>doi:10.17611/DP/EMTF/USARRAY/TA</SurveyDOI>'
    write(0,*) '    </Citation>'
    write(0,*) '    <SelectedPublications>Some ref&lt;br/&gt;Second ref</SelectedPublications>'
    write(0,*) '    <Acknowledgements>No special acknowledgements are required.</Acknowledgements>'
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
 	  write(0,*) '	<ParseProcessingTag>1</ParseProcessingTag>'
 	  write(0,*) '	<FileNameAsSiteID>0</FileNameAsSiteID>'
  	write(0,*) '	<RunList>Runs.xml</RunList>'
  	write(0,*) '	<SiteList>Sites.xml</SiteList>'
  	write(0,*) '	<ChannelList>Channels.xml</ChannelList>'
  	write(0,*) '</Configuration>'
  	write(0,*)
  	write(0,*) 'Project and YearCollected (if present) help identify'
    write(0,*) 'a product in SPUD. They should not contain spaces.'
   	write(0,*) 'Leave the RunList and SiteList elements out or empty'
  	write(0,*) 'if you do not have XML lists for this experiment.'
  	write(0,*) 'To rotate to an angle A, append this angle to command line.'
  	stop
  end if

  ! Initialize site structures
  call init_site_info(zLocalSite)
  call init_site_info(xmlLocalSite)
  call init_site_info(xmlRemoteSite)

  ! Initialize input and output
  call initialize_xml_output(xml_file,'EM_TF')

  ! Read the Z-file in full

  call initialize_z_input(z_file, UserInfo)

  call read_z_header(zsitename, zLocalSite, UserInfo, nf, nch)
  if (UserInfo%FileNameAsSiteID) then
    zLocalSite%ID = basename
  end if


  call read_z_channels(InputMagnetic, OutputMagnetic, OutputElectric, nch, nchoutH, nchoutE, zLocalSite%Declination)

  ! Define local dimensions 
  ! A.K. NOTE AS OF 3/17/2023: we cannot obtain nchout from output channel 
  ! dimensions; as it turns out they if they are not associated, their sizes
  ! are not well-defined and that kills the program
  nchin = size(InputMagnetic)
  nchout = nchoutH + nchoutE

  ! Allocate space for transfer functions and rotation matrices
  allocate(F(nf),TF(nf,nchout,nchin), TFVar(nf,nchout,nchin), stat=istat)
  allocate(InvSigCov(nf,nchin,nchin), ResidCov(nf,nchout,nchout), stat=istat)
  allocate(U(nchin,nchin), V(nchout,nchout), stat=istat)

  ! Initialize conversion to orthogonal geographic coords
  !if (UserInfo%OrthogonalGeographic > 0) then
  !   call rotate_z_channels(InputMagnetic, OutputElectric, U, V)
  !end if

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

     ! rotate to orthogonal geographic coordinates (generality limited to 4 or 5 channels)
     !if (UserInfo%OrthogonalGeographic > 0) then
     !	call rotate_z_period(U, V, TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))
     !end if

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
        Data(i)%fullcov = .true.
        Data(i)%orthogonal = .false.
    case ('E')
        call init_data(Data(i),DataType(i),nf,nchin,nchoutE)
        Data(i)%Matrix = TF(:,nchoutH+1:nchout,:)
        Data(i)%Var = TFVar(:,nchoutH+1:nchout,:)
        Data(i)%InvSigCov = InvSigCov(:,:,:)
        Data(i)%ResidCov = ResidCov(:,nchoutH+1:nchout,nchoutH+1:nchout)
        Data(i)%fullcov = .true.
        Data(i)%orthogonal = .false.
    case default
        write(0,*) 'Error: unable to initialize the data variable #',i
    end select
  end do

  call read_z_notes(Notes)

  ! Finished reading Z-file

  if (rotate) then
      zLocalSite%Orientation = trim(orthogonalORsitelayout)
      zLocalSite%AngleToGeogrNorth = azimuth
      do i=1,size(DataType)
        write(*,'(a9,a20,a4,a10,a14,f9.6)') &
            'Rotating ',trim(DataType(i)%Tag),' to ',trim(orthogonalORsitelayout),' with azimuth ',azimuth
        select case (DataType(i)%Output)
        case ('H')
            call rotate_data(Data(i),InputMagnetic,OutputMagnetic,orthogonalORsitelayout,azimuth)
        case ('E')
            call rotate_data(Data(i),InputMagnetic,OutputElectric,orthogonalORsitelayout,azimuth)
        case default
            ! do nothing
        end select
      end do
  end if

  ! Read information for this site from a list. If successfully read,
  ! trust this information rather than that from the Z-file
  call read_site_list(UserInfo%SiteList, zLocalSite%ID, xmlLocalSite, site_list_exists)

  if (len_trim(xmlLocalSite%ID)>0) then

    xmlLocalSite%Orientation = zLocalSite%Orientation
    xmlLocalSite%AngleToGeogrNorth = zLocalSite%AngleToGeogrNorth
  	call add_xml_header(xmlLocalSite, UserInfo, Notes)
  else

  	call add_xml_header(zLocalSite, UserInfo, Notes)
  end if

  ! Read runs (e.g. start and end times) information for each of the runs
  ! which were used in processing the data found in the Z-file
  ! If multistation, don't try to parse the name in Z-file, use site list
  run_list_exists = .false.
  if (.not. multistation .and. UserInfo%ParseProcessingTag) then
    call read_run_list(UserInfo%RunList, zLocalSite%RunList, Run, run_list_exists)
  else if (len_trim(xmlLocalSite%RunList)>0) then
    call read_run_list(UserInfo%RunList, xmlLocalSite%RunList, Run, run_list_exists)
  end if

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
  if (UserInfo%RemoteRef .and. UserInfo%ParseProcessingTag) then
  	call read_site_list(UserInfo%SiteList, UserInfo%RemoteSiteID, xmlRemoteSite, site_list_exists)
	call read_run_list (UserInfo%RunList, xmlRemoteSite%RunList, RemoteRun, run_list_exists)
  	if (run_list_exists .and. .not. multistation) then
		call add_ProcessingInfo(UserInfo, xmlRemoteSite, RemoteRun)
	else
		call add_ProcessingInfo(UserInfo, xmlRemoteSite)
	end if
  else
  	call add_ProcessingInfo(UserInfo)
  end if

  if (.not. UserInfo%MetadataOnly) then
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
  end if

  call new_element('SiteLayout')
  call new_channel_block('InputChannels')
  do i=1,size(InputMagnetic)
     call add_Channel(InputMagnetic(i), location=.false.)
  end do
  call end_block('InputChannels')

  call new_channel_block('OutputChannels')
  if (associated(OutputMagnetic)) then
    do i=1,nchoutH
        call add_Channel(OutputMagnetic(i), location=.false.)
    end do
  end if
  if (associated(OutputElectric)) then
    do i=1,nchoutE
        call add_Channel(OutputElectric(i), location=.false.)
    end do
  end if
  call end_block('OutputChannels')
  call end_element('SiteLayout')

  ! Read and write frequency blocks: transfer functions, variance, covariance
  if (.not. UserInfo%MetadataOnly) then
  call initialize_xml_freq_block_output(nf)

  do k=1,nf

	  call new_data_block('Period',F(k))

      do i=1,size(Data)
        select case (Data(i)%Type%Output)
        case ('H')
            call add_Data(Data(i), k, InputMagnetic, OutputMagnetic)
            call add_Var(Data(i), k, InputMagnetic, OutputMagnetic)
            call add_InvSigCov(Data(i), k, InputMagnetic)
            call add_ResidCov(Data(i), k, OutputMagnetic)
        case ('E')
            call add_Data(Data(i), k, InputMagnetic, OutputElectric)
            call add_Var(Data(i), k, InputMagnetic, OutputElectric)
            call add_InvSigCov(Data(i), k, InputMagnetic)
            call add_ResidCov(Data(i), k, OutputElectric)
        case default
            write(0,*) 'Error: unable to write the data variable #',i
        end select
      end do

      call end_block('Period')

  end do

  call end_xml_freq_block_output
  end if

  call add_PeriodRange(F)

  ! Exit nicely
  if (associated(Run)) nullify(Run)
  if (associated(RemoteRun)) nullify(RemoteRun)
  if (associated(Notes)) nullify(Notes)
  deallocate(InputMagnetic, stat=istat)
  if (associated(OutputMagnetic)) nullify(OutputMagnetic)
  if (associated(OutputElectric)) nullify(OutputElectric)
  deallocate(F, TF, TFVar, InvSigCov, ResidCov, stat=istat)
  do i = 1,size(DataType)
    call deall_data(Data(i))
  end do
  deallocate(Data)

  call end_z_input

  call end_xml_output('EM_TF')

  write(*,*) 'Written to file: ',trim(xml_file)

end program z2xml
