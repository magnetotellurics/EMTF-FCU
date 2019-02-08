! **********************************************************************
! edi2edi reads in electromagnetic tranfer functions (EMTFs) in the EDI
! format; rotates to chosen coordinate system; write out an EDI file.
!
! Example usage:
! >  ./edi2edi filename.edi filename_out.edi [verbose|silent] 0.0
! to rotate to geographic North.
! >  ./edi2edi filename.edi filename_out.edi [verbose|silent] sitelayout
! to rotate to original site layout as saved in the channels block.
!
! Warning: Rotation of EDI files (mathematically) invalidates the error
! bars since EDI files do not contain the data covariance matrices.
!
! Component of EMTF File Conversion Utilities 2018 (c) A. Kelbert
! **********************************************************************

program edi2edi

  use global
  use edi_read
  use edi_write
  use rotation
  implicit none

  character(len=80) :: edi_file=''
  character(len=80) :: edi_file_out=''
  character(len=80) :: site_info_list='Sites.xml'
  character(len=80) :: run_info_list='Runs.xml'
  character(len=80) :: description='My favourite station'
  character(len=80) :: edisitename, basename, verbose='',rotinfo=''
  type(UserInfo_t)  :: UserInfo
  type(Site_t)                                 :: ediLocalSite, ediRemoteSite
  type(Run_t), dimension(:), allocatable       :: Run
  type(FreqInfo_t), dimension(:), allocatable  :: F
  type(Channel_t), dimension(:), pointer       :: InputMagnetic
  type(Channel_t), dimension(:), pointer       :: OutputMagnetic
  type(Channel_t), dimension(:), pointer       :: OutputElectric
  type(Data_t), dimension(:), pointer          :: Data
  type(DataType_t), dimension(:), pointer      :: DataType
  character(200), dimension(:), pointer       :: Notes
  logical           :: is_spectra
  logical           :: config_exists, site_list_exists
  logical           :: run_list_exists, channel_list_exists
  integer           :: NotesLength
  character(len=80)     :: edi_date,xml_date
  real              :: Ex_len, Ey_len
  integer           :: i, j, k, narg, len, istat
  integer           :: nf, nch, nchin, nchout, nchoutE, nchoutH

  narg = command_argument_count()

  if (narg<1) then
    write(0,*) 'Please specify the name of the input EDI-file'
    stop
  else if (narg>=1) then
    call get_command_argument(1,edi_file)
  end if

  len = len_trim(edi_file)
  basename = edi_file(1:len-4)
  if (narg>1) then
    call get_command_argument(2,edi_file_out)
  else
    edi_file_out = trim(basename)//'_out.edi'
  end if

  if (narg>2) then
    call get_command_argument(3,verbose)
    if (index(verbose,'silent')>0) then
       silent = .true.
    elseif (index(verbose,'debug')>0 .or. index(verbose,'verbose')>0) then
       silent = .false.
    end if
  end if

  if (narg>3) then
    rotate = .true.
    call get_command_argument(4,rotinfo)
    if (index(rotinfo,'original')>0 .or. index(rotinfo,'sitelayout')>0) then
        orthogonalORsitelayout = 'sitelayout'
    else
        read(rotinfo, *) azimuth
        !azimuth = t2e(rotinfo)
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

  ! Update output file name
  if (index(edi_file_out,'.')==0) then
 	edi_file_out = trim(edi_file_out)//'.edi'
  end if

  ! Initialize user info - no requirement for XML file here
  call init_user_info(UserInfo)


  ! Initialize site structures
  call init_site_info(ediLocalSite)
  call init_site_info(ediRemoteSite)

  ! Initialize input; obtain siteID from the file name (most reliable)
  call initialize_edi_input(edi_file, edisitename)

  ! On input, UserInfo comes from the XML configuration; fill it in from the EDI
  call read_edi_header(edisitename, ediLocalSite, UserInfo)

  ! Read EDI info always, but do not necessarily parse
  call read_edi_info(ediLocalSite, UserInfo, Notes, NotesLength)

  ! This allocates and fills in the channels and updates the local site coords
  call read_edi_channels(InputMagnetic, OutputMagnetic, OutputElectric, ediLocalSite, UserInfo)

  nchin = size(InputMagnetic)
  nchoutH = size(OutputMagnetic)
  nchoutE = size(OutputElectric)
  nchout = nchoutH + nchoutE
  nch = nchin + nchout

  ! Read EDI data header and create generic Data variables
  call read_edi_data_header(nf,nch,is_spectra)

  ! Create generic Data variables
  allocate(DataType(2), Data(2), stat=istat)

  if (nchoutH > 0) then
      call init_data_type(DataType(1),'tipper')
      call init_data(Data(1), DataType(1), nf, nchin, nchoutH)
  end if

  if (nchoutE > 0) then
      call init_data_type(DataType(2),'impedance')
      call init_data(Data(2), DataType(2), nf, nchin, nchoutE)
  end if

  ! Allocate periods and read in the EDI data
  allocate(F(nf), stat=istat)
  if (is_spectra) then
    call read_edi_spectra(nf,nch,F,Data)
  else
    call read_edi_data(nf,F,Data)
  end if

  call end_edi_input

  if (rotate) then
      if (.not. is_spectra) then
            ! In the absence of covariance matrices, rotate the variances matrix just like we rotate the TF
            ! and let's be clear that this is not the right way to do it!
            write(0,*) 'WARNING: rotating the TF variances without the full covariance matrix is WRONG'
            write(0,*) '         but since you insist, we are doing it anyway'
      end if
      do i=1,size(DataType)
        if (DataType(i)%derivedType) then
            write(*,*) 'Rotation of derived types is presently not supported. ', &
                'Data type ',trim(DataType(i)%Tag),' will NOT be rotated and may need to be recomputed.'
            cycle
        end if
        write(*,*) 'Rotating ',trim(DataType(i)%Tag),' to ',trim(orthogonalORsitelayout),' with azimuth ',azimuth
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
  call initialize_edi_output(edi_file_out)
  call write_edi_header(edi_date, edisitename, ediLocalSite, UserInfo)
  call write_edi_info(ediLocalSite,UserInfo,Notes,NotesLength)
  call write_edi_channels(InputMagnetic, OutputMagnetic, OutputElectric, ediLocalSite)
  call write_edi_data(edisitename, F, Data)
  call end_edi_output()


  ! Exit nicely
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric, stat=istat)
  deallocate(F, stat=istat)


  write(*,*) 'Written to file: ',trim(edi_file_out)

end program edi2edi
