program xml2z

  use global
  use z_write
  use xml_read
  implicit none

  character(len=80) :: z_file=''
  character(len=80) :: xml_file=''  
  character(len=80) :: site_info_list='USArray_2006_Sites.xml'
  character(len=80) :: run_info_list='USArray_2006_Runs.xml'
  character(len=80) :: description='My favourite station'
  character(len=80) :: zsitename, basename, verbose=''
  type(UserInfo_t)  :: UserInfo
  type(RemoteRef_t) :: Info
  type(Site_t)                               :: zLocalSite, xmlLocalSite, xmlRemoteSite
  type(Run_t), dimension(:), allocatable     :: Run
  type(FreqInfo_t)                           :: F
  type(Channel_t), dimension(:), allocatable :: InputChannel
  type(Channel_t), dimension(:), allocatable :: OutputChannel
  complex(8), dimension(:,:), allocatable    :: TF
  real(8),    dimension(:,:), allocatable    :: TFVar
  complex(8), dimension(:,:), allocatable    :: InvSigCov
  complex(8), dimension(:,:), allocatable    :: ResidCov
  integer           :: i, j, k, n, l

  n = iargc()

  if (n<1) then
     write(0,*) 'Please specify the name of the input XML-file'
     stop
  else if (n>=1) then
     call getarg(1,xml_file)
  end if

  if (n>1) then
     call getarg(2,z_file)
  else
     l = len_trim(xml_file)
     basename = xml_file(1:l-4)
     z_file = trim(basename)
  end if

  if (n>2) then
     call getarg(3,verbose)
     if (index(verbose,'silent')>0) then
        silent = .true.
     end if
  end if

  ! Initialize input and output
  call initialize_xml_input(xml_file)

  call read_xml_header(zsitename, xmlLocalSite, UserInfo, Info)
  
  ! Update output file name and initialize output
  if (index(z_file,'.')==0) then
  	if (Info%remote_ref) then
 		z_file = trim(z_file)//'.zrr'
  	else
 		z_file = trim(z_file)//'.zss'  
  	end if
  end if

  call initialize_z_output(z_file)

  call write_z_header(zsitename, xmlLocalSite, Info)

  ! Read and write channels
  allocate(InputChannel(2), OutputChannel(nch-2))
  allocate(TF(nch-2,2), TFVar(nch-2,2), InvSigCov(2,2), ResidCov(nch-2,nch-2))

  call read_xml_channels(InputChannel, OutputChannel)

  call write_z_channels(zsitename, InputChannel, OutputChannel)

  do k=1,nf
    
     call read_xml_period(k, F, TF, TFVar, InvSigCov, ResidCov)     
     
	 call write_z_period(F, TF, TFVar, InvSigCov, ResidCov)
     
  end do

  ! Exit nicely
  deallocate(InputChannel, OutputChannel)
  deallocate(TF, TFVar, InvSigCov, ResidCov)

  call end_xml_input

  call end_z_output

  if (.not.silent) then
     write(*,*) 'Written to file: ',z_file
  end if

end program xml2z
