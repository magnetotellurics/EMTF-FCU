! **********************************************************************
! xml2z reads in electromagnetic tranfer functions (EMTFs) in the XML
! format; rotates to chosen coordinate system; writes out an Egbert Z-file.
!
! Example usage:
! >  ./xml2z filename.xml
! converts to filename.zrr by default; uses data orientation as specified
! in the XML file ('sitelayout', or 'orthogonal' with given azimuth).
! >  ./xml2z filename.xml filename.edi [verbose|silent] 0.0
! to rotate to geographic North.
! >  ./xml2z filename.xml filename.edi [verbose|silent] sitelayout
! to rotate to original site layout as saved in the channels metadata.
!
! Warning: Rotation of XML files is only (mathematically) valid if the
! full error covariances are present in the file. Otherwise, rotation
! invalidates error bar estimates.
!
! Component of EMTF File Conversion Utilities 2018 (c) A. Kelbert
! **********************************************************************

program xml2z

  use global
  use rotation
  use z_write
  use xml_read
  implicit none

  character(len=80) :: z_file=''
  character(len=80) :: xml_file=''  
  character(len=80) :: site_info_list='Sites.xml'
  character(len=80) :: run_info_list='Runs.xml'
  character(len=80) :: description='My favourite station'
  character(len=80) :: zsitename, basename, verbose='',rotinfo=''
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

  if (narg>3) then
    rotate = .true.
    call get_command_argument(4,rotinfo)
    if (index(rotinfo,'original')>0 .or. index(rotinfo,'sitelayout')>0) then
        orthogonalORsitelayout = 'sitelayout'
    else
        read(rotinfo, *) azimuth
        if ((azimuth)<0.01) then
           write(*,*) 'Rotate ',trim(basename),' to orthogonal geographic coords. '
        else
           write(*,*) 'Rotate ',trim(basename),' to the new azimuth ',azimuth
        end if
        orthogonalORsitelayout = 'orthogonal'
    end if
  else
    write(*,*) 'No further rotation requested for ',trim(basename),'. Using original coords. '
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
        if (trim(xmlLocalSite%Orientation) .eq. 'orthogonal') then
            Data(i)%orthogonal = .true.
        end if
    case ('E')
        call read_xml_data(DataType(i), Data(i), InputMagnetic, OutputElectric)
        if (trim(xmlLocalSite%Orientation) .eq. 'orthogonal') then
            Data(i)%orthogonal = .true.
        end if
    case default
        write(0,*) 'Error: unknown data type ',trim(DataType(i)%Name),' with output ',trim(DataType(i)%Output)
    end select
  end do


  if (rotate) then
      xmlLocalSite%Orientation = trim(orthogonalORsitelayout)
      xmlLocalSite%AngleToGeogrNorth = azimuth
      do i=1,size(DataType)
        write(*,'(a9,a20,a4,a10,a14,f9.6)') &
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

  ! For Z-file output only, update channels orientation to reflect the TFs in file, or as rotated
  if (xmlLocalSite%Orientation .ne. 'sitelayout') then
    call rotate_channels(InputMagnetic, OutputMagnetic, xmlLocalSite%AngleToGeogrNorth)
    call rotate_channels(InputMagnetic, OutputElectric, xmlLocalSite%AngleToGeogrNorth)
  end if

  call write_z_channels(zsitename, InputMagnetic, OutputMagnetic, OutputElectric)

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
