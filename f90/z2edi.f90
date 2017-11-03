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
  character(len=80) :: zsitename, basename, verbose='',coords=''
  type(UserInfo_t)  :: Info
  type(Site_t)                                 :: zLocalSite
  type(Run_t), dimension(:), allocatable       :: Run
  type(FreqInfo_t), dimension(:), allocatable  :: F
  type(Channel_t), dimension(:), pointer       :: InputMagnetic
  type(Channel_t), dimension(:), pointer       :: OutputMagnetic
  type(Channel_t), dimension(:), pointer       :: OutputElectric
  type(Data_t), dimension(:), pointer          :: Data
  type(DataType_t), dimension(:), pointer      :: DataType
  complex(8), dimension(:,:,:), allocatable    :: TF
  real(8),    dimension(:,:,:), allocatable    :: TFVar
  complex(8), dimension(:,:,:), allocatable    :: InvSigCov
  complex(8), dimension(:,:,:), allocatable    :: ResidCov
  real(8),    dimension(:,:), allocatable      :: U,V ! rotation matrices
  character(len=80)     :: edi_date,xml_date
  real              :: Ex_len, Ey_len
  integer           :: i, j, k, narg, len, istat
  integer           :: nf, nch, nchin, nchout, nchoutE, nchoutH

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

  if (narg>3) then
     rotate = .true.
     call get_command_argument(4,coords)
     read(coords, *) azimuth
     if (.not.silent) then
        if ((azimuth)<0.01) then
           write(*,*) 'Rotate to orthogonal geographic coords. '
        else
           write(*,*) 'Rotate to the new azimuth ',azimuth
        end if
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

  call read_z_header(zsitename, zLocalSite, Info, nf, nch)

  ! Read and write channels
  call read_z_channels(InputMagnetic, OutputMagnetic, OutputElectric, nch)

  nchin = size(InputMagnetic)
  nchoutH = size(OutputMagnetic)
  nchoutE = size(OutputElectric)
  nchout = nchoutH + nchoutE

  allocate(F(nf),TF(nf,nchout,nchin), TFVar(nf,nchout,nchin), stat=istat)
  allocate(InvSigCov(nf,nchin,nchin), ResidCov(nf,nchout,nchout), stat=istat)
  allocate(U(nchin,nchin), V(nchout,nchout), stat=istat)

  ! Initialize rotation to a different azimuth on output (zero indicates orthogonal geographic)
  !  - note that rotations will only work with two input and two output electric channels at present
  if (rotate) then
     call rotate_z_channels(InputMagnetic, OutputElectric, U, V, azimuth)
  end if

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

     ! rotate to new azimuth (generality limited to 4 or 5 channels)
     if (rotate) then
        call rotate_z_period(U, V, TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))
     end if

  end do

  ! Save into Data structure
  allocate(DataType(2), Data(2), stat=istat)

  if (nchoutH > 0) then
      call init_data_type(DataType(1),'tipper')
      call init_data(Data(1), DataType(1), nf, nchin, nchoutH)
      do i = 1,nchoutH
        do j = 1,nchin
            Data(1)%Matrix(:,i,j) = TF(:,i,j) !T
            Data(1)%Var(:,i,j) = TFVar(:,i,j)
        end do
      end do
  else
      call init_data_type(DataType(1))
      call init_data(Data(1), DataType(1), nf, nchin, nchoutH)
  end if

  if (nchoutE > 0) then
      call init_data_type(DataType(2),'impedance')
      call init_data(Data(2), DataType(2), nf, nchin, nchoutE)
      do i = 1,nchoutE
        do j = 1,nchin
            Data(2)%Matrix(:,i,j) = TF(:,i+nchoutH,j) !Z
            Data(2)%Var(:,i,j) = TFVar(:,i+nchoutH,j)
        end do
      end do
  else
      call init_data_type(DataType(2))
      call init_data(Data(2), DataType(2), nf, nchin, nchoutH)
  end if

  call date_and_time(date)

  edi_date = date(5:6)//'/'//date(7:8)//'/'//date(3:4)

  call write_edi_file(edi_file,edi_date,zsitename,zLocalSite, &
						InputMagnetic,OutputMagnetic,OutputElectric,F,Data,Info)

! Correct workflow to implement:
! call initialize_edi_output(edi_file)
! call write_edi_header(edi_date, zsitename, xmlLocalSite, UserInfo)
! call write_edi_channels(InputMagnetic, OutputMagnetic, OutputElectric, xmlLocalSite)
! call write_edi_data(F, Data)
! call end_edi_output()

  ! Exit nicely
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric, stat=istat)
  deallocate(F,TF, TFVar, InvSigCov, ResidCov, stat=istat)

  call end_z_input

  if (.not.silent) then
     write(*,*) 'Written to file: ',edi_file
  end if

end program z2edi
