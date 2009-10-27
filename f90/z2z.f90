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
  real(8)           :: azimuth
  logical           :: config_exists, run_list_exists, site_list_exists
  integer           :: i, j, k, n, l, istat

  n = iargc()

  if (n<1) then
     write(0,*) 'Please specify the name of the input Z-file'
     stop
  else if (n>=1) then
     call getarg(1,z_file)
  end if

  l = len_trim(z_file)
  basename = z_file(1:l-4)
  ext = z_file(l-3:l)

  if (n>1) then
     call getarg(2,z_file_out)
  else

     z_file_out = trim(basename)//'_z2z'//trim(ext)
  end if

  if (n>2) then
     call getarg(3,verbose)
     if (index(verbose,'silent')>0) then
        silent = .true.
     end if
  end if

  if (n>3) then
     rotate = .true.
     call getarg(4,coords)
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

  ! Initialize output

  call initialize_z_output(z_file_out)

  ! Read the Z-file in full

  call initialize_z_input(z_file)

  call read_z_header(zsitename, zLocalSite, Info)

  if (rotate .and. ((azimuth)<0.01)) then
     header1 = 'TRANSFER FUNCTIONS IN GEOGRAPHIC COORDINATES'
     header2 = '******** WITH FULL ERROR COVARIANCE ********'
  else
     header1 = 'TRANSFER FUNCTIONS IN MEASUREMENT COORDINATES'
     header2 = '********* WITH FULL ERROR COVARIANCE ********'
  end if

  call write_z_header(zsitename, zLocalSite, Info, header1, header2)

  ! Allocate space for channels and transfer functions
  allocate(InputChannel(2), OutputChannel(nch-2), stat=istat)
  allocate(F(nf), TF(nf,nch-2,2), TFVar(nf,nch-2,2), InvSigCov(nf,2,2), ResidCov(nf,nch-2,nch-2), stat=istat)
  allocate(U(2,2), V(nch-2,nch-2), stat=istat)

  call read_z_channels(InputChannel, OutputChannel, zLocalSite%Declination)

  ! Initialize conversion to orthogonal geographic coords
  if (rotate) then
     call rotate_z_channels(InputChannel, OutputChannel, U, V, azimuth)
  end if

  call write_z_channels(zsitename, InputChannel, OutputChannel)

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

     ! rotate to orthogonal geographic coordinates (generality limited to 4 or 5 channels)
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
  deallocate(InputChannel, OutputChannel)
  deallocate(F, TF, TFVar, InvSigCov, ResidCov)


  if (.not.silent) then
     write(*,*) 'Written to file: ',z_file_out
  end if

end program z2z
