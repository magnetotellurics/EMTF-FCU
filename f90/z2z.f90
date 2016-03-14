program z2z

  use global
  use z_read
  use z_write
  implicit none

  character(len=80) :: input_dir='./'
  character(len=80) :: z_file=''
  character(len=80) :: z_file_out=''
  character(len=80) :: config_file = 'config.xml'
  character(len=80) :: zsitename, basename, ext, verbose='',coords=''
  character(len=80) :: header1, header2
  type(UserInfo_t)  :: Info
  type(Site_t)      :: zLocalSite
  type(Run_t), dimension(:), pointer     :: Run, RemoteRun
  type(FreqInfo_t), dimension(:), allocatable :: F
  type(Channel_t), dimension(:), pointer      :: InputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputElectric
  character(100), dimension(:), pointer       :: Notes
  complex(8), dimension(:,:,:), allocatable    :: TF
  real(8),    dimension(:,:,:), allocatable    :: TFVar
  complex(8), dimension(:,:,:), allocatable    :: InvSigCov
  complex(8), dimension(:,:,:), allocatable    :: ResidCov
  real(8),    dimension(:,:), allocatable      :: U,V ! rotation matrices
  real(8)           :: azimuth
  logical           :: config_exists, run_list_exists, site_list_exists
  integer           :: i, j, k, l, narg, istat
  integer           :: nf, nch, nchin, nchout

  narg = command_argument_count()

  if (narg<1) then
     write(0,*) 'Please specify the name of the input Z-file'
     stop
  else if (narg>=1) then
     call get_command_argument(1,z_file)
  end if

  l = len_trim(z_file)
  basename = z_file(1:l-4)
  ext = z_file(l-3:l)

  if (narg>1) then
     call get_command_argument(2,z_file_out)
  else

     z_file_out = trim(basename)//'_z2z'//trim(ext)
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
     read(coords, '(f9.6)') azimuth
     if (.not.silent) then
        if ((azimuth)<0.01) then
           write(*,*) 'Rotate to orthogonal geographic coords. '
        else
           write(*,*) 'Rotate to the new azimuth ',azimuth
        end if
     end if
  end if

  ! Update output file name
  if (index(z_file_out,'.')==0) then
 	z_file_out = trim(z_file_out)//trim(ext)
  end if

  ! Initialize site structures
  call init_site_info(zLocalSite)

  ! Initialize output

  call initialize_z_output(z_file_out)

  ! Read the Z-file in full

  call initialize_z_input(z_file)

  call init_user_info(Info)

  call read_z_header(zsitename, zLocalSite, Info, nf, nch, header1, header2)

  if (rotate .and. ((azimuth)<0.01)) then
     header1 = 'TRANSFER FUNCTIONS IN GEOGRAPHIC COORDINATES'
     header2 = '******** WITH FULL ERROR COVARIANCE ********'
  elseif (rotate) then
     header1 = 'TRANSFER FUNCTIONS ROTATED TO AZIMUTH '//coords
     header2 = '********* WITH FULL ERROR COVARIANCE ********'
  else
     ! use original file header
  end if

  call write_z_header(zsitename, zLocalSite, Info, nf, nch, header1, header2)

  call read_z_channels(InputMagnetic, OutputMagnetic, OutputElectric, nch, zLocalSite%Declination)

  nchin = size(InputMagnetic)
  nchout = size(OutputMagnetic) + size(OutputElectric)

  ! Allocate space for transfer functions and rotation matrices
  allocate(F(nf),TF(nf,nchout,nchin), TFVar(nf,nchout,nchin), stat=istat)
  allocate(InvSigCov(nf,nchin,nchin), ResidCov(nf,nchout,nchout), stat=istat)
  allocate(U(nchin,nchin), V(nchout,nchout), stat=istat)

  ! Initialize rotation to a different azimuth on output (zero indicates orthogonal geographic)
  !  - note that rotations will only work with two input and two output electric channels at present
  if (rotate) then
     call rotate_z_channels(InputMagnetic, OutputElectric, U, V, azimuth)
  end if

  call write_z_channels(zsitename, InputMagnetic, OutputMagnetic, OutputElectric)

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

     ! rotate to new azimuth (generality limited to 4 or 5 channels)
     if (rotate) then
     	call rotate_z_period(U, V, TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))
     end if

	 call write_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

  end do

  call read_z_notes(Notes)

  ! Finished writing to Z-file
  call end_z_output

  ! Finished reading Z-file
  call end_z_input

  ! Exit nicely
  if (associated(Notes)) deallocate(Notes)
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric)
  deallocate(F, TF, TFVar, InvSigCov, ResidCov, U, V)


  if (.not.silent) then
     write(*,*) 'Written to file: ',z_file_out
  end if

end program z2z
