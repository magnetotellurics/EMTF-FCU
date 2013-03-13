program z2edi

  use global
  use z_read
  use edi_write
  implicit none

  character(len=80) :: z_file=''
  character(len=80) :: edi_file=''
  character(len=80) :: site_info_list='Sites.xml'
  character(len=80) :: run_info_list='Runs.xml'
  character(len=80) :: description='My favourite station'
  character(len=80) :: zsitename, basename, verbose=''
  type(Dimensions_t):: N
  type(UserInfo_t)  :: Info
  type(Site_t)                                 :: zLocalSite
  type(Run_t), dimension(:), allocatable       :: Run
  type(FreqInfo_t), dimension(:), allocatable  :: F
  type(Channel_t), dimension(:), pointer       :: InputMagnetic
  type(Channel_t), dimension(:), pointer       :: OutputMagnetic
  type(Channel_t), dimension(:), pointer       :: OutputElectric
  complex(8), dimension(:,:,:), allocatable    :: TF
  real(8),    dimension(:,:,:), allocatable    :: TFVar
  complex(8), dimension(:,:,:), allocatable    :: InvSigCov
  complex(8), dimension(:,:,:), allocatable    :: ResidCov
  character(len=80)     :: edi_date,xml_date
  real              :: Ex_len, Ey_len
  integer           :: i, j, k, narg, len, nf, nch

  narg = command_argument_count()

  if (narg<1) then
     write(0,*) 'Please specify the name of the input Z-file'
     stop
  else if (narg>=1) then
     call get_command_argument(1,z_file)
  end if

  if (narg>1) then
     call get_command_argument(2,edi_file)
  else
     len = len_trim(z_file)
     basename = z_file(1:len-4)
     edi_file = trim(basename)//'.edi'
  end if

  if (narg>2) then
     call get_command_argument(3,verbose)
     if (index(verbose,'silent')>0) then
        silent = .true.
     end if
  end if

  ! Update output file name
  if (index(edi_file,'.')==0) then
 	edi_file = trim(edi_file)//'.edi'
  end if

  ! Initialize site structures
  call init_site_info(zLocalSite)

  ! Initialize input
  call initialize_z_input(z_file)

  call init_user_info(Info)

  call read_z_header(zsitename, zLocalSite, Info, N)

  ! Define local dimensions
  nf = N%f
  nch = N%ch

  ! Read and write channels
  allocate(F(nf),TF(nf,nch-2,2), TFVar(nf,nch-2,2), InvSigCov(nf,2,2), ResidCov(nf,nch-2,nch-2))

  call read_z_channels(InputMagnetic, OutputMagnetic, OutputElectric, N)

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:), N)

  end do

  call date_and_time(date)

  edi_date = date(5:6)//'/'//date(7:8)//'/'//date(3:4)

  call write_edi_file(edi_file,edi_date,zsitename,zLocalSite, &
						InputMagnetic,OutputMagnetic,OutputElectric,F,TF,TFVar,Info)

  ! Exit nicely
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric)
  deallocate(F,TF, TFVar, InvSigCov, ResidCov)

  call end_z_input

  if (.not.silent) then
     write(*,*) 'Written to file: ',edi_file
  end if

end program z2edi
