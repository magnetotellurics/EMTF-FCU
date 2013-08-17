program edi2xml

  use global
  use config
  use edi_read
  use xml_write
  use read_lists
  implicit none

  character(len=80) :: input_dir='./'
  character(len=80) :: edi_file=''
  character(len=80) :: xml_file=''
  character(len=80) :: config_file = 'config.xml'
  character(len=80) :: edisitename, basename, verbose=''
  type(UserInfo_t)  :: UserInfo
  type(Site_t)      :: ediLocalSite, ediRemoteSite, xmlLocalSite
  type(Channel_t), dimension(:), pointer      :: InputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputElectric
  type(Run_t), dimension(:), pointer     :: Run, RemoteRun
  type(Data_t), dimension(:), pointer         :: Data
  type(DataType_t), dimension(:), pointer     :: DataType, Estimate
  type(FreqInfo_t), dimension(:), allocatable :: F
  character(200), dimension(:), pointer       :: Notes
  character(10)     :: TFname
  real(8),    dimension(:), allocatable        :: value
  logical           :: config_exists, site_list_exists
  logical			:: run_list_exists, channel_list_exists
  integer           :: NotesLength
  integer           :: i, j, k, narg, l, maxblks, istat
  integer           :: nf, nchin, nchout, nchoutE, nchoutH
  character(80)     :: type

  narg = command_argument_count()

  if (narg<1) then
     write(0,*) 'Please specify the name of the input EDI file'
     stop
  else if (narg>=1) then
     call get_command_argument(1,edi_file)
  end if

  if (narg>1) then
     call get_command_argument(2,xml_file)
  else
     l = len_trim(edi_file)
     basename = edi_file(1:l-4)
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
  	call read_xml_config(config_file,UserInfo,DataType,Estimate,input_dir)
  else
  	write(0,*) 'Please provide an XML configuration file [config.xml].'
  	write(0,*) 'This file should reside together with the input data.'
  	write(0,*) 'An example configuration is provided below:'
  	write(0,*)
  	write(0,*) '<Configuration>'
    write(0,*) '    <TimeSeriesArchived>0</TimeSeriesArchived>'
    write(0,*) '    <Network>EM</Network>'
  	write(0,*) '	<Project>USArray</Project>'
  	write(0,*) '	<Survey>TA</Survey>'
  	write(0,*) '	<YearCollected>2011</YearCollected>'
    write(0,*) '    <Country>USA</Country>'
  	write(0,*) '	<Tags>impedance,tipper</Tags>' 
    write(0,*) '    <Citation>'
    write(0,*) '      <Title>USArray TA Magnetotelluric Transfer Functions</Title>'
    write(0,*) '      <Authors>Adam Schultz, Gary D. Egbert, Anna Kelbert</Authors>'
    write(0,*) '      <Year>2012</Year>'
    write(0,*) '      <DOI>(omit if not assigned)</DOI>'
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
    write(0,*) '    <Image>png</Image>'
    write(0,*) '    <Original>edi</Original>'
    write(0,*) '    <ParseEDIInfo>1</ParseEDIInfo>'
    write(0,*) '    <WriteEDIInfo>0</WriteEDIInfo>'
    write(0,*) '    <MetadataOnly>0</MetadataOnly>'
    write(0,*) '    <DateFormat>MM/DD/YY</DateFormat>'
  	write(0,*) '<Configuration>'
  	write(0,*)
  	write(0,*) 'Project and YearCollected (if present) help identify'
    write(0,*) 'a product in SPADE. They should not contain spaces.'
    write(0,*) 'DateFormat (e.g., MM/DD/YY) helps read EDI files.'
  	write(0,*) 'EDI files do not have enough information for rotation'
    write(0,*) 'so output data will be in the same coordinate system'
    write(0,*) 'as they are in the original EDI file. Use caution.'
  	stop
  end if

  ! Initialize site structures
  call init_site_info(ediLocalSite)
  call init_site_info(ediRemoteSite)

  ! Initialize input and output
  call initialize_xml_output(xml_file,'EM_TF')

  ! Obtain siteID from the file name (most reliable)
  call initialize_edi_input(edi_file, edisitename)

  ! On input, UserInfo comes from the XML configuration; fill it in from the EDI
  call read_edi_header(edisitename, ediLocalSite, UserInfo)

  ! Read EDI info always, but do not necessarily parse
  call read_edi_info(ediLocalSite, UserInfo, Notes, NotesLength)

  ! This allocates and fills in the channels and updates the local site coords
  call read_edi_channels(InputMagnetic, OutputMagnetic, OutputElectric, ediLocalSite, UserInfo)

  ! Define channel dimensions
  nchin = size(InputMagnetic)
  nchoutH = size(OutputMagnetic)
  nchoutE = size(OutputElectric)
  nchout = nchoutH + nchoutE

  ! Read EDI data header and create generic Data variables
  call read_edi_data_header(nf)
  write(*,*) 'Allocating data structure for ',size(DataType),' data types, ',nf,' frequencies'
  allocate(Data(size(DataType)), stat=istat)
  do i=1,size(DataType)
    select case (DataType(i)%Output)
    case ('H')
        call init_data(Data(i),DataType(i),nf,nchin,nchoutH)
    case ('E')
        call init_data(Data(i),DataType(i),nf,nchin,nchoutE)
    case default
        !write(0,*) 'Error: unable to initialize the data variable #',i,' for data type ',DataType(i)%Tag
        call init_data(Data(i),DataType(i),nf,1,1) ! allocate for scalar data
    end select
  end do

  ! Allocate periods and read in the EDI data
  allocate(F(nf), stat=istat)
  call read_edi_data(nf,F,Data)

  ! Read information for this site from a list. If successfully read,
  ! trust this information rather than that from the edi-file
  call read_site_list(UserInfo%SiteList, ediLocalSite%IRIS_ID, xmlLocalSite, site_list_exists)

  ! Write XML header
  if (len_trim(xmlLocalSite%ID)>0) then
    write(*,*) 'Using site name and location from a site list, but using the original site IDs.'
    xmlLocalSite%ID = ediLocalSite%ID
    if (UserInfo%WriteEDIInfo) then
        call add_xml_header(xmlLocalSite, UserInfo, Notes, NotesLength)
    else
        call add_xml_header(xmlLocalSite, UserInfo)
    end if
  else
    write(*,*) 'Not using a site list. To change this, edit the XML configuration file.'
    if (UserInfo%WriteEDIInfo) then
        call add_xml_header(ediLocalSite, UserInfo, Notes, NotesLength)
    else
        call add_xml_header(ediLocalSite, UserInfo)
    end if
  end if

  ! Processing notes follow
  if (UserInfo%RemoteRef) then
	call add_ProcessingInfo(UserInfo, ediRemoteSite)
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

  call add_GridOrigin(ediLocalSite)

  call new_channel_block('InputChannels')
  do i=1,nchin
     call add_Channel(InputMagnetic(i), location=.false.)
  end do
  call end_block('InputChannels')

  call new_channel_block('OutputChannels')
  do i=1,nchoutH
     call add_Channel(OutputMagnetic(i), location=.false.)
  end do
  do i=1,nchoutE
     call add_Channel(OutputElectric(i), location=.false.)
  end do
  call end_block('OutputChannels')

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
        case ('E')
            call add_Data(Data(i), k, InputMagnetic, OutputElectric)
            call add_Var(Data(i), k, InputMagnetic, OutputElectric)
        case default
            ! use only for scalar data
            call add_Data(Data(i), k)
            call add_Var(Data(i), k)
        end select
      end do

      call end_block('Period')

  end do

  call end_xml_freq_block_output
  end if

  call add_PeriodRange(F)

  ! Exit nicely
  if (associated(Run)) deallocate(Run)
  if (associated(RemoteRun)) deallocate(RemoteRun)
  if (associated(Notes)) deallocate(Notes)
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric)
  deallocate(F)
  do i = 1,size(DataType)
    call deall_data(Data(i))
  end do
  deallocate(Data)

  call end_edi_input

  call end_xml_output('EM_TF')

  if (.not.silent) then
     write(*,*) 'Written to file: ',xml_file
  end if

end program edi2xml
