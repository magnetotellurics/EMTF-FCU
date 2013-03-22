program xml2z

  use global
  use z_write
  use xml_read
  implicit none

  character(len=80) :: z_file=''
  character(len=80) :: xml_file=''  
  character(len=80) :: site_info_list='Sites.xml'
  character(len=80) :: run_info_list='Runs.xml'
  character(len=80) :: description='My favourite station'
  character(len=80) :: zsitename, basename, verbose=''
  type(UserInfo_t)  :: UserInfo
  type(Site_t)      :: xmlLocalSite, xmlRemoteSite
  type(Channel_t), dimension(:), pointer      :: InputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputElectric
  type(Data_t), dimension(:), pointer         :: Data
  type(DataType_t), dimension(:), pointer     :: DataType, Estimate
  type(FreqInfo_t), dimension(:), pointer     :: F
  type(Run_t), dimension(:), allocatable      :: Run
  complex(8), dimension(:,:,:), allocatable   :: TF
  real(8),    dimension(:,:,:), allocatable   :: TFVar
  complex(8), dimension(:,:,:), allocatable   :: InvSigCov
  complex(8), dimension(:,:,:), allocatable   :: ResidCov
  integer           :: i, j, k, narg, l, istat, iT, iZ
  integer           :: nf, nch, ndt, nchin, nchout, nchoutE, nchoutH

  narg = command_argument_count()

  if (narg<1) then
     write(0,*) 'Please specify the name of the input XML-file'
     stop
  else if (narg>=1) then
     call get_command_argument(1,xml_file)
  end if

  if (narg>1) then
     call get_command_argument(2,z_file)
  else
     l = len_trim(xml_file)
     basename = xml_file(1:l-4)
     z_file = trim(basename)
  end if

  if (narg>2) then
     call get_command_argument(3,verbose)
     if (index(verbose,'silent')>0) then
        silent = .true.
     end if
  end if

  ! Initialize site structures
  call init_site_info(xmlLocalSite)
  call init_site_info(xmlRemoteSite)

  ! Initialize input and output
  call initialize_xml_input(xml_file)

  call read_xml_header(zsitename, xmlLocalSite, UserInfo, nf, nch, ndt)
  
  ! Update output file name (../ or ./ allowed) and initialize output
  if (index(z_file,'.')<=2) then
  	if (UserInfo%RemoteRef) then
 		z_file = trim(z_file)//'.zrr'
  	else
 		z_file = trim(z_file)//'.zss'  
  	end if
  end if

  call initialize_z_output(z_file)

  call write_z_header(zsitename, xmlLocalSite, UserInfo, nf, nch)

  ! Read and write channels
  call read_xml_channels(InputMagnetic, OutputMagnetic, OutputElectric)

  call write_z_channels(zsitename, InputMagnetic, OutputMagnetic, OutputElectric)

  nchin = size(InputMagnetic)
  nchout = size(OutputMagnetic) + size(OutputElectric)

  ! Allocate space for transfer functions and rotation matrices
  call read_xml_periods(F)
  allocate(TF(nf,nchout,nchin), TFVar(nf,nchout,nchin), stat=istat)
  allocate(InvSigCov(nf,nchin,nchin), ResidCov(nf,nchout,nchout), stat=istat)
  TF = 0.0d0
  TFVar = 0.0d0
  InvSigCov = 0.0d0
  ResidCov = 0.0d0

  call read_xml_data_types(DataType)

  allocate(Data(size(DataType)), stat=istat)

  do i=1,size(DataType)
    select case (DataType(i)%Output)
    case ('H')
        call read_xml_data(DataType(i), Data(i), InputMagnetic, OutputMagnetic)
    case ('E')
        call read_xml_data(DataType(i), Data(i), InputMagnetic, OutputElectric)
    case default
        write(0,*) 'Error: unknown data type ',trim(DataType(i)%Name),' with output ',trim(DataType(i)%Output)
    end select
  end do

  nchoutH = size(OutputMagnetic)
  nchoutE = size(OutputElectric)

  ! Forming the matrices for Z-files (cross-datatype diagonal entries will be zero)
  iT = find_data_type(Data,'T')
  if (iT>0) then
    TF(:,1:nchoutH,:) = Data(iT)%Matrix
    TFVar(:,1:nchoutH,:) = Data(iT)%Var
    InvSigCov(:,:,:) = Data(iT)%InvSigCov
    ResidCov(:,1:nchoutH,1:nchoutH) = Data(iT)%ResidCov
  end if

  iZ = find_data_type(Data,'Z')
  if (iZ > 0) then
    TF(:,nchoutH+1:nchout,:) = Data(iZ)%Matrix
    TFVar(:,nchoutH+1:nchout,:) = Data(iZ)%Var
    InvSigCov(:,:,:) = Data(iZ)%InvSigCov
    ResidCov(:,nchoutH+1:nchout,nchoutH+1:nchout) = Data(iZ)%ResidCov
  end if

  do k=1,size(F)

     call write_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

  end do

  ! Exit nicely
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric)
  deallocate(F, TF, TFVar, InvSigCov, ResidCov)

  call end_xml_input

  call end_z_output

  if (.not.silent) then
     write(*,*) 'Written to file: ',z_file
  end if

end program xml2z
