program xml2edi

  use global
  use edi_write
  use xml_read
  implicit none

  character(len=80) :: edi_file=''
  character(len=80) :: xml_file=''
  character(len=19) :: xml_time, edi_date 
  character(len=80) :: site_info_list='USArray_2006_Sites.xml'
  character(len=80) :: run_info_list='USArray_2006_Runs.xml'
  character(len=80) :: description='My favourite station'
  character(len=80) :: zsitename, basename, verbose=''
  type(UserInfo_t)  :: UserInfo
  type(RemoteRef_t) :: Info
  type(Site_t)                               :: zLocalSite, xmlLocalSite, xmlRemoteSite
  type(Run_t), dimension(:), allocatable     :: Run
  type(FreqInfo_t), dimension(:), allocatable:: F
  type(Channel_t), dimension(:), allocatable :: InputChannel
  type(Channel_t), dimension(:), allocatable :: OutputChannel
  complex(8), dimension(:,:,:), allocatable    :: TF
  real(8),    dimension(:,:,:), allocatable    :: TFVar
  complex(8), dimension(:,:,:), allocatable    :: InvSigCov
  complex(8), dimension(:,:,:), allocatable    :: ResidCov
  integer           :: i, j, k, n, l

  n = command_argument_count()

  if (n<1) then
     write(0,*) 'Please specify the name of the input XML-file'
     stop
  else if (n>=1) then
     call get_command_argument(1,xml_file)
  end if

  if (n>1) then
     call get_command_argument(2,edi_file)
  else
     l = len_trim(xml_file)
     basename = xml_file(1:l-4)
     edi_file = trim(basename)//'.edi'
  end if

  if (n>2) then
     call get_command_argument(3,verbose)
     if (index(verbose,'silent')>0) then
        silent = .true.
     end if
  end if

  ! Update output file name
  if (index(edi_file,'.')==0) then
 	edi_file = trim(edi_file)//'.edi'
  end if
  
  ! Initialize input and output
  call initialize_xml_input(xml_file, xml_time)

  call read_xml_header(zsitename, xmlLocalSite, UserInfo, Info)

  ! Read and write channels
  allocate(InputChannel(2), OutputChannel(nch-2))
  allocate(F(nf),TF(nf,nch-2,2), TFVar(nf,nch-2,2), InvSigCov(nf,2,2), ResidCov(nf,nch-2,nch-2))

  call read_xml_channels(InputChannel, OutputChannel)

  do k=1,nf
    
     call read_xml_period(k, F(k), TF(k,:,:), TFVar(k,:,:))     
          
  end do

  edi_date = xml_time(6:7)//'/'//xml_time(9:10)//'/'//xml_time(3:4)

  call write_edi_file(edi_file,edi_date,zsitename,xmlLocalSite, &
  			InputChannel,OutputChannel,F,TF,TFVar,Info,UserInfo)

  ! Exit nicely
  deallocate(InputChannel, OutputChannel)
  deallocate(F, TF, TFVar, InvSigCov, ResidCov)

  call end_xml_input

  if (.not.silent) then
     write(*,*) 'Written to file: ',edi_file
  end if

end program xml2edi
