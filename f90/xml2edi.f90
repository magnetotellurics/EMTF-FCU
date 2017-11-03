program xml2edi

  use global
  use rotation
  use edi_write
  use xml_read
  implicit none

  character(len=80) :: edi_file=''
  character(len=80) :: xml_file=''
  character(len=19) :: xml_time, edi_date 
  character(len=80) :: site_info_list='Sites.xml'
  character(len=80) :: run_info_list='Runs.xml'
  character(len=80) :: description='My favorite station'
  character(len=80) :: xmlsitename, basename, verbose='',rotinfo=''
  type(UserInfo_t)  :: UserInfo
  type(Site_t)      :: xmlLocalSite, xmlRemoteSite
  type(Channel_t), dimension(:), pointer      :: InputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputElectric
  type(Data_t), dimension(:), pointer         :: Data
  type(DataType_t), dimension(:), pointer     :: DataType, Estimate
  type(FreqInfo_t), dimension(:), pointer     :: F
  type(Run_t), dimension(:), allocatable      :: Run
  integer           :: i, j, k, narg, l, istat
  integer           :: nf, nch, ndt

  narg = command_argument_count()

  if (narg<1) then
     write(0,*) 'Please specify the name of the input XML-file'
     stop
  else if (narg>=1) then
     call get_command_argument(1,xml_file)
  end if

  l = len_trim(xml_file)
  basename = xml_file(1:l-4)
  if (narg>1) then
     call get_command_argument(2,edi_file)
  else
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

  ! Update output file name
  if (index(edi_file,'.')==0) then
 	edi_file = trim(edi_file)//'.edi'
  end if
  
  ! Initialize site structures
  call init_site_info(xmlLocalSite)
  call init_site_info(xmlRemoteSite)

  ! Initialize input and output
  call initialize_xml_input(xml_file, xml_time)

  call read_xml_header(xmlsitename, xmlLocalSite, UserInfo, nf, nch, ndt)

  call read_xml_channels(InputMagnetic, OutputMagnetic, OutputElectric)

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
        write(0,*) 'Skipping unknown data type ',trim(DataType(i)%Name),' with output ',trim(DataType(i)%Output)
        !call read_xml_data(DataType(i), Data(i)) ! read scalar valued data currently causes segmentation fault
    end select
  end do

  call read_xml_periods(F)


  if (rotate) then
      xmlLocalSite%Orientation = trim(orthogonalORsitelayout)
      xmlLocalSite%AngleToGeogrNorth = azimuth
      write(0,*) 'WARNING: by writing to EDI file, full error covariances are LOST'
      write(0,*) '         but we are using them, if present in XML file, to rotate'
      do i=1,size(DataType)
        write(*,'(a10,a20,a4,a10,a14,f9.6)') 'Rotating ',trim(DataType(i)%Tag),' to ',trim(orthogonalORsitelayout),' with azimuth ',azimuth
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


  edi_date = xml_time(6:7)//'/'//xml_time(9:10)//'/'//xml_time(3:4)

  ! Implemented correct EDI output workflow that can accommodate any rotations and metadata;
  ! currently writing out impedances and tippers, if present
  call initialize_edi_output(edi_file)
  call write_edi_header(edi_date, xmlsitename, xmlLocalSite, UserInfo)
  call write_edi_info(xmlLocalSite,UserInfo)
  call write_edi_channels(InputMagnetic, OutputMagnetic, OutputElectric, xmlLocalSite)
  call write_edi_data(xmlsitename, F, Data)
  call end_edi_output()

  ! Exit nicely
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric)

  call end_xml_input

  write(*,*) 'Written to file: ',trim(edi_file)

end program xml2edi
