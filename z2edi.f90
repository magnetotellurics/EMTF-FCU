program z2edi

  use global
  use rotation
  use z_read
  use edi_write
  implicit none

  character(len=200):: z_file=''
  character(len=200):: edi_file=''
  character(len=80) :: site_info_list='Sites.xml'
  character(len=80) :: run_info_list='Runs.xml'
  character(len=80) :: description='My favourite station'
  character(len=200):: zsitename, basename, verbose='',coords=''
  type(UserInfo_t)  :: Info
  type(Site_t)                                 :: zLocalSite
  type(Run_t), dimension(:), allocatable       :: Run
  type(FreqInfo_t), dimension(:), allocatable  :: F
  character(100), dimension(:), pointer        :: Notes
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
  call read_z_channels(InputMagnetic, OutputMagnetic, OutputElectric, nch, nchoutH, nchoutE)

  ! Define local dimensions 
  ! A.K. NOTE AS OF 3/17/2023: we cannot obtain nchout from output channel 
  ! dimensions; as it turns out they if they are not associated, their sizes
  ! are not well-defined and that kills the program
  nchin = size(InputMagnetic)
  nchout = nchoutH + nchoutE

  allocate(F(nf),TF(nf,nchout,nchin), TFVar(nf,nchout,nchin), stat=istat)
  allocate(InvSigCov(nf,nchin,nchin), ResidCov(nf,nchout,nchout), stat=istat)
  allocate(U(nchin,nchin), V(nchout,nchout), stat=istat)

  do k=1,nf

     !write (*,*) 'Reading period number ', k
     call read_z_period(F(k), TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))

     ! rotate to orthogonal geographic coordinates (generality limited to 4 or 5 channels)
     !if (UserInfo%OrthogonalGeographic > 0) then
     !  call rotate_z_period(U, V, TF(k,:,:), TFVar(k,:,:), InvSigCov(k,:,:), ResidCov(k,:,:))
     !end if

  end do

  ! Save into Data structure
  allocate(DataType(2), stat=istat)
  call init_data_type(DataType(1),'tipper')
  call init_data_type(DataType(2),'impedance')

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
        if (nchoutH == 0) Data(i)%allocated = .false.
    case ('E')
        call init_data(Data(i),DataType(i),nf,nchin,nchoutE)
        Data(i)%Matrix = TF(:,nchoutH+1:nchout,:)
        Data(i)%Var = TFVar(:,nchoutH+1:nchout,:)
        Data(i)%InvSigCov = InvSigCov(:,:,:)
        Data(i)%ResidCov = ResidCov(:,nchoutH+1:nchout,nchoutH+1:nchout)
        Data(i)%fullcov = .true.
        Data(i)%orthogonal = .false.
        if (nchoutE == 0) Data(i)%allocated = .false.
    case default
        write(0,*) 'Error: unable to initialize the data variable #',i
    end select
  end do

  call read_z_notes(Notes)

  ! Finished reading Z-file


  if (rotate) then
      zLocalSite%Orientation = trim(orthogonalORsitelayout)
      zLocalSite%AngleToGeogrNorth = azimuth
      write(0,*) 'WARNING: by writing to EDI file, full error covariances are LOST'
      write(0,*) '         but first, we are using them to rotate'
      do i=1,size(DataType)
        if (DataType(i)%derivedType) then
            write(*,*) 'Rotation of derived types is presently not supported. ', &
                'Data type ',trim(DataType(i)%Tag),' will NOT be rotated and may need to be recomputed.'
            cycle
        else if (.not. Data(i)%allocated) then
            write(*,*) 'Data type ',trim(DataType(i)%Tag),' is not allocated and will not be rotated.'
            cycle
        end if
        write(*,'(a10,a20,a4,a10,a14,f9.6)') &
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

  call date_and_time(date)

  edi_date = date(5:6)//'/'//date(7:8)//'/'//date(3:4)

  ! Implemented correct EDI output workflow that can accommodate any rotations and metadata;
  ! currently writing out impedances and tippers, if present
  call initialize_edi_output(edi_file)
  call write_edi_header(edi_date, zsitename, zLocalSite, Info)
  call write_edi_info(zLocalSite, Info)
  call write_edi_channels(InputMagnetic, OutputMagnetic, OutputElectric, zLocalSite)
  call write_edi_data(zsitename, F, Data)
  call end_edi_output()

  ! Exit nicely
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric, stat=istat)
  deallocate(F,TF, TFVar, InvSigCov, ResidCov, stat=istat)

  call end_z_input

  if (.not.silent) then
     write(*,*) 'Written to file: ',edi_file
  end if

end program z2edi
