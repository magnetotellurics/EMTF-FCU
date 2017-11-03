program xml2xml

  use global
  use rotation
  use xml_read
  use xml_write
  implicit none

  character(len=80) :: xml_file=''
  character(len=80) :: xml_file_out=''
  character(len=19) :: xml_time, edi_date 
  character(len=80) :: site_info_list='Sites.xml'
  character(len=80) :: run_info_list='Runs.xml'
  character(len=80) :: description='My favorite station'
  character(len=80) :: xmlsitename, basename, verbose='',rotinfo=''
  type(UserInfo_t)  :: UserInfo
  type(Site_t)      :: xmlLocalSite
  type(Channel_t), dimension(:), pointer      :: InputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputMagnetic
  type(Channel_t), dimension(:), pointer      :: OutputElectric
  type(Data_t), dimension(:), pointer         :: Data
  type(DataType_t), dimension(:), pointer     :: DataType, Estimate
  type(FreqInfo_t), dimension(:), pointer     :: F
  type(Run_t), dimension(:), allocatable      :: Run
  integer           :: nchin, nchout, nchoutE, nchoutH
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
     call get_command_argument(2,xml_file_out)
  else
     xml_file_out = trim(basename)//'_out.xml'
  end if

  silent = .true.
  if (narg>2) then
     call get_command_argument(3,verbose)
     if (index(verbose,'silent')>0) then
        silent = .true.
     else if (index(verbose,'debug')>0 .or. index(verbose,'verbose')>0) then
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
  if (index(xml_file_out,'.')==0) then
 	xml_file = trim(xml_file_out)//'.xml'
  end if
  
  ! Initialize site structures
  call init_site_info(xmlLocalSite)

  ! Initialize input and output
  call initialize_xml_input(xml_file, xml_time)

  call read_xml_header(xmlsitename, xmlLocalSite, UserInfo, nf, nch, ndt)

  call read_xml_channels(InputMagnetic, OutputMagnetic, OutputElectric)

  call read_xml_statistical_estimates(Estimate)

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

  call end_xml_input

  if (rotate) then
      xmlLocalSite%Orientation = trim(orthogonalORsitelayout)
      xmlLocalSite%AngleToGeogrNorth = azimuth
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

  ! Define channel dimensions
  nchin = size(InputMagnetic)
  nchoutH = size(OutputMagnetic)
  nchoutE = size(OutputElectric)
  nchout = nchoutH + nchoutE

  ! Initialize output
  call initialize_xml_output(xml_file_out,'EM_TF')

  ! Write XML header
  call add_xml_header(xmlLocalSite, UserInfo)

  ! Processing notes follow
  call add_ProcessingInfo(UserInfo)

  call new_element('StatisticalEstimates')
  do i=1,size(Estimate)
    call add_Estimate(Estimate(i))
  end do
  call end_element('StatisticalEstimates')

  call new_element('DataTypes')
  do i=1,size(DataType)
    call add_DataType(DataType(i))
  end do
  call end_element('DataTypes')

  !call add_GridOrigin(xmlLocalSite)

  call new_element('SiteLayout')
  call new_channel_block('InputChannels')
  do i=1,nchin
     call add_Channel(InputMagnetic(i), location=.false.)
  end do
  call end_block('InputChannels')

  call new_channel_block('OutputChannels')
  do i=1,nchoutH
     call add_Channel(OutputMagnetic(i), location=.false.)
  end do
  do i=1,nchoutE
     call add_Channel(OutputElectric(i), location=.false.)
  end do
  call end_block('OutputChannels')
  call end_element('SiteLayout')

  ! Read and write frequency blocks: transfer functions, variance, covariance
  if (.not. UserInfo%MetadataOnly) then
  call initialize_xml_freq_block_output(nf)

  do k=1,nf

      call new_data_block('Period',F(k))

      do i=1,size(Data)
        select case (Data(i)%Type%Output)
        case ('H')
            call add_Data(Data(i), k, InputMagnetic, OutputMagnetic)
            call add_Var(Data(i), k, InputMagnetic, OutputMagnetic)
            if (Data(i)%fullcov) then
                call add_InvSigCov(Data(i), k, InputMagnetic)
                call add_ResidCov(Data(i), k, OutputMagnetic)
            end if
        case ('E')
            call add_Data(Data(i), k, InputMagnetic, OutputElectric)
            call add_Var(Data(i), k, InputMagnetic, OutputElectric)
            if (Data(i)%fullcov) then
                call add_InvSigCov(Data(i), k, InputMagnetic)
                call add_ResidCov(Data(i), k, OutputElectric)
            end if
        case default
            ! use only for scalar data
            call add_Data(Data(i), k)
            call add_Var(Data(i), k)
        end select
      end do

      call end_block('Period')

  end do

  call end_xml_freq_block_output
  end if

  call add_PeriodRange(F)

  ! Exit nicely
  deallocate(InputMagnetic, OutputMagnetic, OutputElectric, stat=istat)
  deallocate(F, stat=istat)
  do i = 1,size(DataType)
    call deall_data(Data(i))
  end do
  deallocate(Data, stat=istat)

  call end_xml_output('EM_TF')

  write(*,*) 'Written to file: ',trim(xml_file_out)

end program xml2xml
