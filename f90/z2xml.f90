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
  type(RemoteRef_t) :: Info
  type(UserInfo_t)  :: UserInfo
  type(Site_t)      :: zLocalSite, xmlLocalSite, xmlRemoteSite
  type(Run_t), dimension(:), pointer     :: Run, RemoteRun
  type(FreqInfo_t), dimension(:), allocatable :: F
  type(Channel_t), dimension(:), allocatable :: InputChannel
  type(Channel_t), dimension(:), allocatable :: OutputChannel
  character(100), dimension(:), pointer       :: Notes
  complex(8), dimension(:,:,:), allocatable    :: TF
  real(8),    dimension(:,:,:), allocatable    :: TFVar
  complex(8), dimension(:,:,:), allocatable    :: InvSigCov
  complex(8), dimension(:,:,:), allocatable    :: ResidCov
  real(8),    dimension(:,:), allocatable      :: U,V ! rotation matrices
  logical           :: config_exists, run_list_exists, site_list_exists
  integer           :: i, j, k, n, l, istat

  n = command_argument_count()

  if (n<1) then
     write(0,*) 'Please specify the name of the input Z-file'
     stop
  else if (n>=1) then
     call get_command_argument(1,z_file)
  end if

  if (n>1) then
     call get_command_argument(2,xml_file)
  else
     l = len_trim(z_file)
     basename = z_file(1:l-4)
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
  	call read_xml_config(config_file,UserInfo,input_dir)
  else
  	write(0,*) 'Please provide an XML configuration file [config.xml].'
  	write(0,*) 'This file should reside together with the input data.'
  	write(0,*) 'An example configuration is provided below:'
  	write(0,*)
  	write(0,*) '<Configuration>'
  	write(0,*) '<Source>OSU</Source>'
  	write(0,*) '<Project>USArray</Project>'
  	write(0,*) '<Experiment>USArray Cascadia</Experiment>'
  	write(0,*) '<YearCollected>2007</YearCollected>'
  	write(0,*) '<OrthogonalGeographic>0</OrthogonalGeographic>'
  	write(0,*) '<ProcessedBy>Gary Egbert</ProcessedBy>'
  	write(0,*) '<ProcessingSoftware>EMTF</ProcessingSoftware>'
  	write(0,*) '<ProcessingTag></ProcessingTag>'
  	write(0,*) '<RunList>Runs.xml</RunList>'
  	write(0,*) '<SiteList>Sites.xml</SiteList>'
  	write(0,*) '</Configuration>'
  	write(0,*)
  	write(0,*) 'Source, Project and ProcessingSoftware help identify'
    write(0,*) 'a product in SPADE. They should not contain spaces.'
    write(0,*) 'Same is true about the optional ProcessingTag.'
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
  call initialize_xml_output(xml_file,'MT_TF')

  ! Read the Z-file in full

  call initialize_z_input(z_file)

  call read_z_header(zsitename, zLocalSite, Info)

  ! Allocate space for channels and transfer functions
  allocate(InputChannel(2), OutputChannel(nch-2), stat=istat)
  allocate(F(nf), TF(nf,nch-2,2), TFVar(nf,nch-2,2), InvSigCov(nf,2,2), ResidCov(nf,nch-2,nch-2), stat=istat)
  allocate(U(2,2), V(nch-2,nch-2), stat=istat)

  call read_z_channels(InputChannel, OutputChannel, zLocalSite%Declination)

  ! Initialize conversion to orthogonal geographic coords
  if (UserInfo%OrthogonalGeographic > 0) then
     call rotate_z_channels(InputChannel, OutputChannel, U, V)
  end if

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

     ! rotate to orthogonal geographic coordinates (generality limited to 4 or 5 channels)
     if (UserInfo%OrthogonalGeographic > 0) then
     	call rotate_z_period(U, V, TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))
     end if

  end do

  call read_z_notes(Notes)

  ! Finished reading Z-file

  ! Read information for this site from a list. If successfully read,
  ! trust this information rather than that from the Z-file
  call read_site_list(UserInfo%SiteList, zLocalSite%ID, xmlLocalSite, site_list_exists)

  if (len_trim(xmlLocalSite%ID)>0) then
  	call add_USArray_MT_header(xmlLocalSite, UserInfo, Info, Notes)
  else
  	call add_USArray_MT_header(zLocalSite, UserInfo, Info, Notes)
  end if

  if (Info%remote_ref) then
  	call read_site_list(UserInfo%SiteList, Info%remote_site_id, xmlRemoteSite, site_list_exists)
	call read_run_list (UserInfo%RunList, xmlRemoteSite%RunList, RemoteRun, run_list_exists)
  	if (run_list_exists) then
		call add_ProcessingInfo(UserInfo, Info, xmlRemoteSite, RemoteRun)
	else
		call add_ProcessingInfo(UserInfo, Info, xmlRemoteSite)
	end if
  else
  	call add_ProcessingInfo(UserInfo, Info)
  end if

  ! Read runs (e.g. start and end times) information for each of the runs
  ! which were used in processing the data found in the Z-file
  call read_run_list(UserInfo%RunList, zLocalSite%RunList, Run, run_list_exists)

  if (run_list_exists) then
  	do i=1,size(Run)
		call add_Info(Run(i))
  	end do

  	call new_element('TimeDataCollected')
  	do i=1,size(Run)
		call add_TimePeriod(Run(i))
  	end do
  	call end_element('TimeDataCollected')
  end if

  call new_element('InputChannels')
  do i=1,2
     call add_Channel(InputChannel(i), location=.false.)
  end do
  call end_element('InputChannels')

  call new_element('OutputChannels')
  do i=1,nch-2
     call add_Channel(OutputChannel(i), location=.false.)
  end do
  call end_element('OutputChannels')

  ! Read and write frequency blocks: transfer functions, variance, covariance
  call initialize_xml_freq_block_output(nf)

  do k=1,nf

     call new_Frequency(F(k))

     call add_TF(TF(k,:,:), InputChannel, OutputChannel)
     call add_TFVar(TFVar(k,:,:), InputChannel, OutputChannel)
     call add_InvSigCov(InvSigCov(k,:,:), InputChannel)
     call add_ResidCov(ResidCov(k,:,:), OutputChannel)

     call end_element('Frequency')

  end do

  call end_xml_freq_block_output

  call add_FrequencyRange(F)

  ! Exit nicely
  deallocate(Run)
  if (associated(RemoteRun)) deallocate(RemoteRun)
  if (associated(Notes)) deallocate(Notes)
  deallocate(InputChannel, OutputChannel)
  deallocate(F, TF, TFVar, InvSigCov, ResidCov)

  call end_z_input

  call end_xml_output('MT_TF')

  if (.not.silent) then
     write(*,*) 'Written to file: ',xml_file
  end if

end program z2xml
