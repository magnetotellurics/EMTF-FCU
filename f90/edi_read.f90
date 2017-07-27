module edi_read

  use global
  use utils

  implicit none
  private

  integer                        :: edifile
  character(len=200)             :: temp
  logical                        :: new_block, new_section
  character(len=80)              :: this_block, this_section
  integer                        :: missing_value_count=0
  character(len=80)              :: dummy_data_value
  integer                        :: ios,i,j,k,maxblks=0

  save   :: edifile
  save   :: new_block, new_section
  save   :: this_block, this_section
  save   :: missing_value_count
  save   :: dummy_data_value
  save   :: maxblks

  public :: initialize_edi_input
  public :: read_edi_header
  public :: read_edi_channels
  public :: read_edi_data_header
  public :: read_edi_data_block
  public :: read_edi_data
  public :: parse_edi_data_block_name
  public :: read_edi_info
  public :: end_edi_input


contains

  subroutine initialize_edi_input(fname,sitename)
     character(len=*), intent(in)   :: fname
     character(len=80), intent(out) :: sitename
     integer			  :: i,ios
     character(20)		  :: str

     ! passed an empty string
     if (fname == '') then
        edifile=5
        open (unit=edifile,iostat=ios)
     else
        edifile=55
        open (unit=edifile,file=fname,status='old',iostat=ios)
     end if

     if(ios/=0) then
        write(0,*) 'Error opening file:', fname
     endif

     ! parse the EDI file name for site name:
     ! the only robust source for a unique ID
     i = index(trim(fname),'/',.true.)
     if (i<=0) then
        i = 0
     end if
     j = index(trim(fname),'.edi')
     if (j > 0) then
        sitename = trim(fname(i+1:j-1))
        k=index( trim(sitename),'_imp')
        if(k.gt.0)sitename=sitename(1:k-1)

        if (.not.silent) then
            write(6,*) 'Initialized reading site ',trim(sitename),' from EDI file'
        end if
     else
        write(0,*) 'Error: ',trim(fname),' is not an EDI file'
     end if

  end subroutine initialize_edi_input


  subroutine parse_edi_line(line, var, value)
    character(len=200), intent(in)   :: line
    character(len=200), intent(out)  :: var, value
    ! local
    character(len=1)                 :: edichar1, edichar2
    integer                          :: i, N=200

    ! Does this contain a new block? new section? a comment?
    ! Use the first two non-space characters to determine
    new_block = .false.
    new_section = .false.
    temp = adjustl(line)
    edichar1 = temp(1:1)
    edichar2 = temp(2:2)
    if (edichar1 == '>') then
        select case (edichar2)
        case ('=')
            new_section = .true.
            this_section = trim(adjustl(temp(3:N)))
            var = 'SECTION'
            value = this_section
            return
        case ('!')
            var = 'COMMENT'
            value = trim(adjustl(temp(3:N)))
            return
        case default
            ! this block reads the first word in line
            ! but save full info in value
            new_block = .true.
            read (temp(2:N),*) this_block
            var = 'BLOCK'
            value = trim(adjustl(temp(2:N)))
            return
        end select
    end if

    ! In all other circumstances, continue parsing the line
    i = index(trim(temp),'=')
    if (i > 0) then
        var = trim(temp(1:i-1))
        value = trim(adjustl(temp(i+1:N)))
        edichar1 = value(1:1)
        if (isdigit(edichar1)) then
            ! just save the full line; could be date or anything
        elseif (len_trim(value) == 0) then
            ! this is an empty string; meaningless, do nothing
        elseif (edichar1 == char(ascii_qtm)) then
            ! find the second double quote and read what's in between
            i = index(trim(value(2:N)),char(ascii_qtm))
            value = trim(value(2:i))
        else
            ! read the first word in line as the value
            read (temp(i+1:N),*) value
        end if
    else
        ! Note: empty info line is still an info line;
        ! this preserves the formatting
        if (trim(this_block) == 'INFO') then
            var = 'INFO'
        elseif (isempty(line)) then
            var = 'DUMMY' ! avoiding conflict with EMPTY value in header
        else
            var = 'DATA'
        end if
        value = line
    end if

  end subroutine parse_edi_line


  subroutine read_edi_header(sitename, Site, Info)
    character(len=80), intent(inout) :: sitename
    type(Site_t),  intent(out)       :: Site
	type(UserInfo_t), intent(inout)  :: Info
	! local
    character(len=200)               :: line, var, value
    character(len=200)               :: country, state, county
    character(len=2)                 :: elevunits

	call init_site_info(Site)
	country = ''
	state = ''
	county = ''
	elevunits = 'M'
	Info%Basename = trim(sitename)

    read (edifile,'(a200)',iostat=ios) line !read the first line in file
    call parse_edi_line(line,var,value)
    if (.not. (trim(var) == 'BLOCK' .or. trim(value) == 'HEAD')) then
        write(0,*) 'Error reading the first line of EDI header:'
        write(0,*) line
    end if

	do
        read (edifile,'(a200)',iostat=ios) line
        if ((ios /= 0) .or. (trim(this_block) .eq. 'END')) then
            exit
        end if
        call parse_edi_line(line,var,value)
        if (trim(this_block) .ne. 'HEAD') then ! next block encountered
            exit
        end if
        if (.not.silent) then
            write(*,*) 'Reading ',trim(var),' line: ',trim(value)
        end if

        select case (trim(var))
        case ('DATAID')
            ! do nothing: info extracted from file name more reliable
        case ('ACQBY')
            if (isempty(Info%AcquiredBy)) then ! prefer the XML config info
                Info%AcquiredBy = value
            end if
        case ('FILEBY') ! only use this if the information is missing from config
            if (len_trim(Info%ProcessedBy) == 0) then
                Info%ProcessedBy = value
            end if
        case ('ACQDATE') ! date of (start of) data acquisition
            Site%Start = datestr(value,Info%DateFormat,'XML')
            if (.not. silent) then
                write(*,*) 'Acquired date in XML format: ',trim(Site%Start)
            end if
        case ('ENDDATE')
            Site%End = datestr(value,Info%DateFormat,'XML')
        case ('FILEDATE')
            if (.not. Info%IgnoreProcessDateInFile) then
                Info%ProcessDate = datestr(value,Info%DateFormat,'YYYY-MM-DD')
                if (.not. silent) then
                    write(*,*) 'Process date in XML format: ',trim(Info%ProcessDate)
                end if
            end if
        case ('COUNTRY')
            country = value
        case ('STATE')
            state = value
        case ('COUNTY')
            county = value
        case ('PROSPECT')
            if (isempty(Info%Survey)) then ! prefer the XML config info
                Info%Survey = value
            end if
        case ('LOC')
            if (.not. (trim(adjustl(value)) .eq. 'Area Name')) then
                Site%Description = value
            end if
        case ('LAT')
            if (index(value,':')>0) then
                Site%Location%lat=dms2deg(value)
                if (.not. silent) then
                    write(*,*) 'Latitude conversion: ',trim(value),' to ',Site%Location%lat
                end if
            else ! assume decimal
                read(value,*) Site%Location%lat
                if (.not. silent) then
                    write(*,*) 'Decimal latitude value of ',trim(value),' found in EDI header'
                end if
            end if
        case ('LONG')
            if (index(value,':')>0) then
                Site%Location%lon=dms2deg(value)
                if (.not. silent) then
                    write(*,*) 'Longitude conversion: ',trim(value),' to ',Site%Location%lon
                end if
            else ! assume decimal
                read(value,*) Site%Location%lon
                if (.not. silent) then
                    write(*,*) 'Decimal longitude value of ',trim(value),' found in EDI header'
                end if
            end if
        case ('ELEV')
            read(value,*) Site%Location%elev
        case ('UNITS') ! units for elevation
            elevunits = value
        case ('DECL')
            read(value,*) Site%Declination
        case ('STDVERS,MAXSECT')
            ! EDI file information; ignore - needed only for reading
        case ('PROGVERS')
            Info%ProcessingSoftware = value
        case ('PROGDATE')
            Info%ProcessingSoftwareLastMod = datestr(value,Info%DateFormat,'YYYY-MM-DD')
            if (.not. silent) then
                write(*,*) 'Software lastmod date in XML format: ',trim(Info%ProcessingSoftwareLastMod)
            end if
        case ('BINDATA')
            ! ignore for now; may revisit if needed
            write(0,*) 'Warning: BINDATA value ',value,' ignored'
        case ('EMPTY') ! represents 'no data'
            Info%DummyDataValue = value
            dummy_data_value = value
        case ('DUMMY')
            ! empty line
        case default
            write(0,*) 'Warning: HEAD block ',trim(var),' value ',trim(value),' ignored'
        end select
    end do

    ! use sitename for siteID
    Site%ID = trim(sitename)

    ! IRIS site ID is a 5-digit, uppercase abbreviation
    if (len_trim(Site%ID)==3) then
        Site%IRIS_ID = toupper(Site%ID(1:1))//toupper(Site%ID(2:2))//toupper(Site%ID(3:3))
    else if (len_trim(Site%ID)==4) then
        Site%IRIS_ID = toupper(Site%ID(1:1))//toupper(Site%ID(2:2))//toupper(Site%ID(3:3))//Site%ID(4:4)
    else if (len_trim(Site%ID)==5) then
        Site%IRIS_ID = toupper(Site%ID(1:1))//toupper(Site%ID(2:2))//toupper(Site%ID(3:3))//Site%ID(4:5)
    else if (len_trim(Site%ID)>=6) then
        Site%IRIS_ID = toupper(Site%ID(1:1))//toupper(Site%ID(2:2))//Site%ID(4:6)
        !Site%IRIS_ID = toupper(Site%ID(1:1))//toupper(Site%ID(2:2))//Site%ID(3:5)
    end if

    ! typical case: ENDDATE not present in the EDI file; use ACQDATE
    if(isempty(Site%End)) then
        Site%End = Site%Start
    end if
    if (isempty(Info%YearCollected)) then ! again, prefer the XML config info to the EDI header
        Info%YearCollected = datestr(Site%End,'XML','YYYY')
    end if

    ! typical case: LOC not present; use COUNTRY etc for site description
    if(isempty(Site%Description) .and. .not. isempty(county) .and. .not. isempty(country) .and. .not. isempty(state)) then
        Site%Description = trim(county)//', '//trim(state)//', '//trim(country)
    else if(isempty(Site%Description) .and. .not. isempty(country) .and. .not. isempty(state)) then
        Site%Description = 'Unknown, '//trim(state)//', '//trim(country)
    else if (isempty(Site%Description)) then
        Site%Description = Info%DefaultSiteName
    end if

    ! but if this setting is on, just ignore it all in favor of the default name in the config file
    if(Info%IgnoreSiteNameInFile) then
        Site%Description = Info%DefaultSiteName
    end if

    ! default data quality from configuration file
    Site%QualityRating = Info%DefaultDataQuality
    Site%QualityComments = Info%DataQualityComment

    ! convert elevation to meters
    if(trim(elevunits) .eq. 'FT') then
        Site%Location%elev = 0.3048 * Site%Location%elev
    end if

    ! make sure the longitude is -180 to 180
    if(Site%Location%lon > 180.0d0) then
		Site%Location%lon = Site%Location%lon - 360.0d0
	end if

	! success message
    if (.not. silent) then
        write(*,*) 'Done reading the EDI header. Next block: ',trim(this_block)
    end if

  end subroutine read_edi_header


  subroutine read_edi_info(Site,Info,Notes,n)
    type(Site_t),  intent(inout)          :: Site
    type(UserInfo_t), intent(inout)       :: Info
    character(200), dimension(:), pointer :: Notes
    integer, intent(inout)                :: n
    ! local
    character(len=200)                    :: line, var, value
    character(len=2)                      :: elevunits
    logical                               :: readlat,readlon,readelev
    logical                               :: readinfo
    integer                               :: i1

    elevunits = 'M'

    read (edifile,'(a200)',iostat=ios) line !read the first line in file
    call parse_edi_line(line,var,value)
    if (.not. (trim(var) == 'MAXINFO')) then
        write(0,*) 'Error reading the first line of EDI INFO block:'
        write(0,*) line
        n = 1000
    else
        read (value,'(i8)',iostat=ios) n
    end if

    if (associated(Notes)) deallocate(Notes)

    allocate(Notes(n))

    if (.not.silent) then
        write(*,*) 'Before entering the INFO block, latitude  = ',Site%Location%lat
        write(*,*) 'Before entering the INFO block, longitude = ',Site%Location%lon
        write(*,*) 'Before entering the INFO block, elevation = ',Site%Location%elev
    end if


    if (index(to_upper(Info%ProcessedBy),'PHOENIX')>0) then

        ! parse special Phoenix info format
        do i=1,n
            read (edifile,'(a200)',iostat=ios) line
            ! exit if we're no longer in the INFO block
            if (ios /= 0 .or. .not. (trim(this_block) == 'INFO')) then
                n = i-1
                exit
            end if
            Notes(i) = line

            ! electric field channel
            i1 = index(trim(temp),'Ex1=')

        end do

    else

        ! parse the general INFO block
        do i=1,n
            read (edifile,'(a200)',iostat=ios) line
            ! exit if we're no longer in the INFO block
            if (ios /= 0 .or. .not. (trim(this_block) == 'INFO')) then
                n = i-1
                exit
            end if
            Notes(i) = line

            call parse_edi_line(line,var,value)
            if (.not.silent) then
                write(*,*) 'Reading ',trim(var),' line: ',trim(value)
            end if
            ! ... or if the new section DEFINEMEAS is encountered
            if (new_section) then
                n = i-1
                exit
            end if

            if (Info%ParseEDIinfo) then

                ! to parse the INFO block, first find an occurence of the site ID
                ! then allow parsing of only one value of each type after site ID
                if (trim(var) .eq. 'INFO') then
                    if (index(to_upper(value),trim(to_upper(Site%ID)))>0) then
                        readinfo = .true.
                        readlat = .false.
                        readlon = .false.
                        readelev = .false.
                    end if
                end if

                if (readinfo) then
                select case (trim(var))
                case ('AZIMUTH')
                    !read(value,*) Site%Declination
                case ('LATITUDE','SITE LATITUDE') ! assume it's decimal if found in INFO block
                    read(value,*) Site%Location%lat
                    readlat = .true.
                case ('LONGITUDE','SITE LONGITUDE') ! assume it's decimal if found in INFO block
                    read(value,*) Site%Location%lon
                    readlon = .true.
                case ('ELEVATION','SITE ELEVATION') ! read elevation and convert to meters if needed
                    read(value,*) Site%Location%elev
                    if(trim(elevunits) .eq. 'FT') then
                        Site%Location%elev = 0.3048 * Site%Location%elev
                    end if
                    readelev = .true.
                case ('INFO','DUMMY','COMMENT')
                    ! these are already saved in the Notes if needed
                case default
                    ! but there might be something else that requires parsing
                    write(0,*) 'Warning: INFO block ',trim(var),' value ',trim(value),' ignored'
                end select
                end if

                ! exit the reading loop when everything has been read
                if (readlat .and. readlon .and. readelev) then
                    readinfo = .false.
                end if

            end if

        end do

    end if ! End the Info%ProcessedBy if statement

    if (.not.silent) then
        write(*,*) 'Before exiting the INFO block, latitude  = ',Site%Location%lat
        write(*,*) 'Before exiting the INFO block, longitude = ',Site%Location%lon
        write(*,*) 'Before exiting the INFO block, elevation = ',Site%Location%elev
    end if

  end subroutine read_edi_info


  subroutine parse_edi_channel(value, Channel, need_second_line)
    character(len=*), intent(in)     :: value
    type(Channel_t), intent(inout)   :: Channel
    logical, intent(inout), optional :: need_second_line
    ! local
    character(len=200)               :: line, var
    character(len=1)                 :: edichar1, edichar2
    integer                          :: i, i1, i2, j(10), N=200, istat

    ! Use the first non-space character to determine channel type
    call init_channel_info(Channel)
    temp = adjustl(value)
    N = len_trim(value)
    i1 = 0
    i2 = 0
    i = 0
    j(:) = 0
    edichar1 = temp(1:1)
    Channel%Type = edichar1

    if (.not. silent) then
        write(*,*) 'Parsing EDI channel: ',trim(Channel%Type)
    end if

    ! Note - the use of spaces in channel lines is not permitted by
    ! the EDI standard but still sometimes encountered in practice
    select case (edichar1)
    case ('E')
        ! electric field channel
        i1 = index(trim(temp),'CHTYPE=')
        i2 = index(trim(temp),'CHTYPE =')
        i = max(i1,i2+1)
        if (i > 1) then
            var = trim(temp(i+7:i+8))
            edichar2 = var(2:2) !temp(i+8:i+8)
        else
            write(*,*) 'Error: unknown EDI channel type'
            return
        end if
        Channel%ID = trim(var)
        call init_channel_units(Channel)
        ! read in the numeric ID
        i1 = index(trim(temp),'ID=')
        i2 = index(trim(temp),'ID =')
        i = max(i1,i2+1)
        read(temp(i+3:N),*,iostat=istat) Channel%NumericID
        ! read in the XYZ coordinates
        i1 = index(trim(temp),'X=')
        i2 = index(trim(temp),'X =')
        i = max(i1,i2+1)
        read(temp(i+2:N),*,iostat=istat) Channel%X
        i1 = index(trim(temp),'Y=')
        i2 = index(trim(temp),'Y =')
        i = max(i1,i2+1)
        read(temp(i+2:N),*,iostat=istat) Channel%Y
        i1 = index(trim(temp),'Z=')
        i2 = index(trim(temp),'Z =')
        i = max(i1,i2+1)
        if (i > 1) then
            read(temp(i+2:N),*) Channel%Z
        end if
        i1 = index(trim(temp),'X2=')
        i2 = index(trim(temp),'X2 =')
        i = max(i1,i2+1)
        read(temp(i+3:N),*,iostat=istat) Channel%X2
        i1 = index(trim(temp),'Y2=')
        i2 = index(trim(temp),'Y2 =')
        i = max(i1,i2+1)
        read(temp(i+3:N),*,iostat=istat) Channel%Y2
        i1 = index(trim(temp),'Z2=')
        i2 = index(trim(temp),'Z2 =')
        i = max(i1,i2+1)
        if (i > 1) then
            read(temp(i+3:N),*) Channel%Z2
        end if
        ! compute the dipole length
        select case (edichar2)
        case ('X')
            Channel%DipoleLength = abs(Channel%X2 - Channel%X)
        case ('Y')
            Channel%DipoleLength = abs(Channel%Y2 - Channel%Y)
        end select
        ! for now, the optional metadata will be ignored: have never
        ! seen it yet in a real EDI file. Will deal with it later!
        j(1) = index(trim(temp),'ACQCHAN=')
        j(2) = index(trim(temp),'FILTER=')
        j(3) = index(trim(temp),'GAIN=')
        j(4) = index(trim(temp),'MEASDATE=')
        if (sum(j) > 0) then
            write(0,*) 'Warning: optional metadata ignored for channel ',trim(Channel%ID)
        end if

    case ('H')        
        ! magnetic field channel
        i1 = index(trim(temp),'CHTYPE=')
        i2 = index(trim(temp),'CHTYPE =')
        i = max(i1,i2+1)
        if (i > 1) then
            var = trim(temp(i+7:i+9))
        else
            write(0,*) 'Error: unknown EDI channel type'
            return
        end if
        Channel%ID = trim(var)
        call init_channel_units(Channel)
        ! read in the numeric ID
        i1 = index(trim(temp),'ID=')
        i2 = index(trim(temp),'ID =')
        i = max(i1,i2+1)
        read(temp(i+3:N),*,iostat=istat) Channel%NumericID
        ! read in the XYZ coordinates
        i1 = index(trim(temp),'AZM=')
        i2 = index(trim(temp),'AZM =')
        i = max(i1,i2+1)
        read(temp(i+4:N),*,iostat=istat) Channel%Orientation
        i1 = index(trim(temp),'X=')
        i2 = index(trim(temp),'X =')
        i = max(i1,i2+1)
        read(temp(i+2:N),*,iostat=istat) Channel%X
        i1 = index(trim(temp),'Y=')
        i2 = index(trim(temp),'Y =')
        i = max(i1,i2+1)
        read(temp(i+2:N),*,iostat=istat) Channel%Y
        i1 = index(trim(temp),'Z=')
        i2 = index(trim(temp),'Z =')
        i = max(i1,i2+1)
        if (i > 1) then
            read(temp(i+2:N),*) Channel%Z
        end if
        i1 = index(trim(temp),'DIP=')
        i2 = index(trim(temp),'DIP =')
        i = max(i1,i2+1)
        if (i > 1) then
            read(temp(i+4:N),*) Channel%Tilt
        end if
        ! for now, the optional metadata will be ignored: have never
        ! seen it yet in a real EDI file. Will deal with it later!
        j(1) = index(trim(temp),'ACQCHAN=')
        j(2) = index(trim(temp),'FILTER=')
        j(3) = index(trim(temp),'SENSOR=')
        j(4) = index(trim(temp),'GAIN=')
        j(5) = index(trim(temp),'MEASDATE=')
        if (sum(j) > 0) then
            write(0,*) 'Warning: optional metadata ignored for channel ',trim(Channel%ID)
        end if

    case default
        write(0,*) 'Warning: not a known EDI channel line: ',trim(value)
        return
    end select

    if (present(need_second_line)) then
        need_second_line = .false.
        if (istat .ne. 0) then
            need_second_line = .true.
        end if
    end if

  end subroutine parse_edi_channel

  ! From the channel block >=DEFINEMEAS we obtain:
  ! Site XYZ cartesian coordinates relative to an origin;
  ! Site lat/lon/z location (IF not present in the header OR info blocks);
  ! Channel location *relative to the site* (NOT to the origin);
  ! Dipole lengths;
  ! Channel orientations unless overwritten by data rotation angles;
  ! Other instrument information.
  subroutine read_edi_channels(Input, OutputH, OutputE, Site, UserInfo)
    type(Channel_t), dimension(:), pointer, intent(inout) :: Input
    type(Channel_t), dimension(:), pointer, intent(inout) :: OutputH, OutputE
    type(Site_t), intent(inout)                  :: Site
    type(UserInfo_t), intent(in)                 :: UserInfo
    ! local
    type(Channel_t), dimension(:), pointer  :: Channel ! all channels
    logical                          :: electric_channels_first,channel_on_two_lines
    character(len=200)               :: line, var, value
    integer                          :: num,i,j,istat,nchin,nch,hch,ech
    real(8)                          :: xy(2,1),ll(2,1)

    nch = 5 ! default total number of channels that are really used
    allocate(Channel(nch), stat=istat)

    hch = 0 ! start counting magnetic channels
    ech = 0 ! start counting electric channels
    num = 0 ! start counting channels

    do
        read (edifile,'(a200)',iostat=ios) line
        if ((ios /= 0) .or. (trim(this_block) .eq. 'END')) then
            exit
        end if
        call parse_edi_line(line,var,value)
        if (trim(this_section) .ne. 'DEFINEMEAS') then ! next section encountered
            exit
        end if
        if (.not.silent) then
            write(*,*) 'Reading ',trim(var),' line: ',trim(value)
        end if

        select case (trim(var))
        case ('MAXCHAN')
            ! NOTE: if present, this always comes before
            ! the channel block lines so safe to reallocate
            ! However, this causes segmentation fault with gcc compiler.
            ! For now, issue a warning but stick to 5 channels, hardcoded here.
            !read(value,*,iostat=ios) nch
            !deallocate(Channel, stat=istat)
            !allocate(Channel(nch), stat=istat)
            write(0,*) 'Warning: we are only using the first ',nch,' channels. This is because the EDI files'
            write(0,*) 'tend to include remote channels for which there are no data. Change this in edi_read.f90'
        case ('BLOCK')
            num = num+1
            if (num > nch) then
                write(0,*) 'Warning: skipping channel number ',num
            else
                call parse_edi_channel(value,Channel(num),channel_on_two_lines)
                if (channel_on_two_lines) then
                    read (edifile,'(a200)',iostat=ios) line
                    value = trim(value)//trim(line)
                    call parse_edi_channel(value,Channel(num))
                end if
                if (trim(Channel(num)%Type) .eq. 'H') then
                    hch = hch+1
                elseif (trim(Channel(num)%Type) .eq. 'E') then
                    ech = ech+1
                else
                    write(0,*) 'Warning: Unknown channel type ',Channel(num)%Type,' found for channel #',num
                end if
            end if
        case ('REFLOC')
            Site%Coords%Origin%ID = trim(value)
        case ('REFLAT')
            if (index(value,':')>0) then
                Site%Coords%Origin%lat=dms2deg(value)
                if (.not. silent) then
                    write(*,*) 'Latitude conversion for the origin: ',trim(value),' to ',Site%Coords%Origin%lat
                end if
            else ! assume decimal
                read(value,*) Site%Coords%Origin%lat
                if (.not. silent) then
                    write(*,*) 'Decimal latitude value of ',trim(value),' found for the origin'
                end if
            end if
        case ('REFLONG')
            if (index(value,':')>0) then
                Site%Coords%Origin%lon=dms2deg(value)
                if (.not. silent) then
                    write(*,*) 'Longitude conversion for the origin: ',trim(value),' to ',Site%Coords%Origin%lon
                end if
            else ! assume decimal
                read(value,*) Site%Coords%Origin%lon
                if (.not. silent) then
                    write(*,*) 'Decimal longitude value of ',trim(value),' found for the origin'
                end if
            end if
        case ('REFELEV')
            read(value,*) Site%Coords%Origin%elev
        case ('UNITS') ! units for the cartesian location
            Site%Coords%Units = trim(value)
        case ('REFTYPE')
            ! this isn't used correctly in practical EDI files
            Site%Coords%Type = trim(value)
        case ('MAXRUN','MAXMEAS')
            ! do nothing - these are only used to parse the numeric channel ID
        case ('DUMMY')
            ! empty line
        case default
            ! but there might be something else that requires parsing
            write(0,*) 'Warning: DEFINEMEAS block ',trim(var),' value ',trim(value),' ignored'
        end select
    end do


    ! first convert site and channel locations to meters, if needed
    if(trim(Site%Coords%Units) .eq. 'FT') then
        Site%Coords%X = 0.3048 * Site%Coords%X
        Site%Coords%Y = 0.3048 * Site%Coords%Y
        Site%Coords%Z = 0.3048 * Site%Coords%Z
        Site%Coords%Units = 'meters'
        write(0,*)'DEBUG FT->M, channel units:', Channel(1)%Units
        write(0,*)'DEBUG FT->M, before:',Channel(1)%X,Channel(1)%Y,Channel(1)%Z
        do i=1,nch
            call convert_channel_to_meters(Channel(i))
        end do
        write(0,*)'DEBUG FT->M, after:',Channel(1)%X,Channel(1)%Y,Channel(1)%Z
    end if

    ! now, obtain site coordinates relative to an origin from the channels
    ! (use the first input channel - usually Hx - for the absolute "site location")
    Site%Coords%X = Channel(1)%X
    Site%Coords%Y = Channel(1)%Y
    Site%Coords%Z = Channel(1)%Z

    ! adjust the channel location so that it's now relative to the site
    do i=1,nch
        Channel(i)%X = Channel(i)%X - Site%Coords%X
        Channel(i)%Y = Channel(i)%Y - Site%Coords%Y
        Channel(i)%Z = Channel(i)%Z - Site%Coords%Z
        ! for electric output channels, also update the end of the dipole
        if (trim(Channel(i)%Type) .eq. 'E') then
            Channel(i)%X2 = Channel(i)%X2 - Site%Coords%X
            Channel(i)%Y2 = Channel(i)%Y2 - Site%Coords%Y
            Channel(i)%Z2 = Channel(i)%Z2 - Site%Coords%Z
        end if
        Channel(i)%Units = Site%Coords%Units
    end do

    ! Usually, input channels come first; output channels follow.
    ! However some old EDI files have electric channels first, and magnetic channels to follow.
    ! Since the electric channels are never used as input in MT transfer functions,
    ! I added additional logic to deal with channels that are written in the wrong order.
    electric_channels_first = .false.
    if (Channel(1)%Type == 'E') then
        electric_channels_first = .true.
    end if

    ! now, allocate and initialize input and output channels
    nchin = 2
    allocate(Input(nchin), OutputE(ech), stat=istat)
    if (hch > nchin) then
        allocate(OutputH(hch-nchin), stat=istat)
    end if
    do num=1,nchin
        i = num
        if (electric_channels_first) then
            Input(i) = Channel(num+2)
        else
            Input(i) = Channel(num)
        end if
        if (.not. silent) then
            write(*,*) 'Input channel #',i,' is ',Input(i)%ID
        end if
    end do
    i = 0
    j = 0
    do num=1,nch
        if (electric_channels_first) then
            if (num == 3 .or. num == 4) then
                cycle
            end if
        else
            if (num < 3) then
                cycle
            end if
        end if
        if (trim(Channel(num)%Type) .eq. 'H') then
            i = i+1
            OutputH(i) = Channel(num)
            if (.not. silent) then
                write(*,*) 'Output magnetic channel #',i,' is ',OutputH(i)%ID
            end if
        else
            j = j+1
            OutputE(j) = Channel(num)
            if (.not. silent) then
                write(*,*) 'Output electric channel #',j,' is ',OutputE(j)%ID
            end if
        end if

    end do
    deallocate(Channel, stat=istat)

    ! finally, compute and verify the site lat/lon coordinates
    xy(1,1) = Site%Coords%X
    xy(2,1) = Site%Coords%Y
    call xy2ll(xy,ll,Site%Coords%Origin%lat,Site%Coords%Origin%lon)
    if (.not. silent) then
        write(*,*) 'KM per Degree at origin: ',kmPerDeg(Site%Coords%Origin%lat)
        write(*,*) 'KM per Degree at site  : ',kmPerDeg(Site%Location%lat)
        write(*,*) 'Computed site latitude : ',ll(1,1)
        write(*,*) 'Computed site longitude: ',ll(2,1)
        write(*,*) 'Recorded site latitude : ',Site%Location%lat
        write(*,*) 'Recorded site longitude: ',Site%Location%lon
    end if
    if (UserInfo%ComputeSiteCoords) then
        if (.not.silent) then
            write(*,*) 'Using computed coordinates for site location'
        end if
        Site%Location%lat = ll(1,1)
        Site%Location%lon = ll(2,1)
        Site%Location%elev = Site%Coords%Origin%elev
        if (.not.silent) then
            write(*,*) 'Based on our computation, site latitude  = ',Site%Location%lat
            write(*,*) 'Based on our computation, site longitude = ',Site%Location%lon
            write(*,*) 'Based on our computation, site elevation = ',Site%Location%elev
        end if
    else
        write(0,*) 'By default, using recorded location. Change in XML configuration file'
    end if

  end subroutine read_edi_channels


  subroutine read_edi_data_header(nf)
    integer, intent(inout) :: nf
   ! local
    character(len=200)               :: line, var, value
    integer                          :: num,nch
    character(len=80)                :: chname

    maxblks = 100

    do
        read (edifile,'(a200)',iostat=ios) line
        if ((ios /= 0) .or. (trim(this_block) .eq. 'END')) then
            exit
        end if
        call parse_edi_line(line,var,value)
        if (new_block) then
            exit ! proceed to read the data
        end if
        if (.not.silent) then
            write(*,*) 'Reading ',trim(var),' line: ',trim(value)
        end if
        if (trim(this_section) .eq. 'MTSECT') then ! reading MT data
            select case (trim(var))
            case ('SECTID')
                !Site%Description = value
            case ('NFREQ')
               read(value,*) nf
            case ('MAXBLKS')
                read(value,*) maxblks
            case ('HX','HY','HZ','EX','EY','RX','RY')
                ! do something
            case ('DUMMY','COMMENT')
                ! do not store comments and empty lines
            case default
                ! but there might be something else that requires parsing
                write(0,*) 'Warning: MTSECT block ',trim(var),' value ',trim(value),' ignored'
            end select
        elseif (trim(this_section) .eq. 'EMAPSECT') then ! reading EMAP data
            select case (trim(var))
            case ('SECTID')
                !Site%Description = value
            case ('NFREQ')
                read(value,*) nf
            case ('MAXBLKS')
                read(value,*) maxblks
            case ('NDIPOLE')
                ! do nothing
            case ('TYPE')
                ! do nothing
            case ('CHKSUM')
                ! do nothing
            case ('HX','HY','RX','RY')
                ! do something
            case ('DUMMY','COMMENT')
                ! do not store comments and empty lines
            case default
                ! but there might be something else that requires parsing
                write(0,*) 'Warning: EMAPSECT block ',trim(var),' value ',trim(value),' ignored'
            end select
        elseif (trim(this_section) .eq. 'SPECTRASECT') then
            ! do nothing
            select case (trim(var))
            case ('SECTID')
                !Site%Description = value
            case ('NFREQ')
                read(value,*) nf
            case ('NCHAN')
                read(value,*) nch
            case ('MAXBLKS')
                read(value,*) maxblks
            case ('CHKSUM')
                ! do nothing
            case ('DUMMY','COMMENT')
                ! do not store comments and empty lines
            case default
                ! but there might be something else that requires parsing
                write(0,*) 'Warning: SPECTRASECT block ',trim(var),' value ',trim(value),' ignored'
            end select
        elseif (trim(this_section) .eq. 'TSERIESSECT') then
            ! do nothing
            write(0,*) 'Warning: TSERIESSECT block ignored (time series reading not supported)'
            return
        elseif (trim(this_section) .eq. 'OTHERSECT') then ! skipping these for now
            ! do nothing
            write(0,*) 'Warning: OTHERSECT block ignored (parsing not yet implemented)'
            return
        else
            write(0,*) 'Warning: unknown ',trim(this_section),' section encountered!'
            return
        end if

    end do

  end subroutine read_edi_data_header


  subroutine read_edi_data_block(nf,data)
    integer, intent(in)         :: nf ! number of freq to read
    real(8), intent(out)        :: data(nf) ! real data values
    character(len=200)          :: line, var, value, data_block
    character(len=20000)        :: datalist
    integer                     :: i

    data_block = this_block
    datalist = ''

    do
        read (edifile,'(a200)',iostat=ios) line
        if ((ios /= 0) .or. (trim(this_block) .eq. 'END')) then
            return
        end if
        call parse_edi_line(line,var,value)
        if (.not.silent) then
            write(*,*) 'Reading ',trim(var),' line: ',trim(value)
        end if
        ! if we enter a new block, exit
        if (new_block) then
            exit
        else if (trim(var) .eq. 'DATA') then
            datalist = trim(datalist)//' '//trim(value)
        end if
    end do

    ! if we find a missing value, provide a warning (for now)
    ! will implement replacing these with NaNs if needed
    !  - this will require a little book keeping
    i=index(datalist,dummy_data_value)
    if (i>0) then
        write(*,*) 'Warning: found a missing value ',trim(dummy_data_value)
        write(*,*) 'Warning: replacing with NaN not currently implemented'
        missing_value_count = missing_value_count + 1
    end if

    read (datalist, *, iostat=ios) data
    if (ios /= 0) then
        write(*,*) 'Warning: unable to read ',nf,' data values from block ',trim(data_block)
    end if

  end subroutine read_edi_data_block


  subroutine read_edi_data(nf,F,Data)
    type(FreqInfo_t), intent(inout) :: F(:) ! periods
    type(Data_t), intent(inout)	:: Data(:) ! one for each data type
    integer, intent(in)         :: nf ! number of freq to read
    real(8)        		:: value(nf) ! real data values
    complex(8)			:: cvalue(nf) ! complex data values
    character(4)		:: type ! real/imag
    integer		  	    :: row,col ! row & column in a matrix
    integer		  	    :: i,j,k,ind,istat
    character(len=80)   :: TFcomp, TFname, info, stat

      do j=1,maxblks
        if ((ios /= 0) .or. (trim(this_block) .eq. 'END')) then
            if (missing_value_count>0) then
                write(*,*) 'Found ',missing_value_count,' data type components with missing values'
                write(*,*) 'Warning: these are not dealt with correctly yet; need to replace with NaNs'
                write(*,*) 'Warning: the necessary book keeping has not yet been implemented'
            else
                write(*,*) 'No missing values encountered in EDI file'
            end if
            return
        end if
        if (.not.silent) then
            write(*,*) 'Reading DATA from block ',trim(this_block)
        end if
        TFname = ''
        row = 1
        col = 1
        type = 'real'
        stat = ''
        value = 0.0d0
        call parse_edi_data_block_name(this_block,TFname,row,col,type,stat)
        call read_edi_data_block(nf,value)
        if (type .eq. 'imag') then
            cvalue = dcmplx(0.0d0,value)
        else
            cvalue = dcmplx(value,0.0d0)
        end if
        ! based on TF name store all periods in Data
        select case (trim(TFname))
        case ('FREQ')
            do k=1,nf
                call init_freq_info(F(k))
                F(k)%value = 1.0d0/value(k)
                F(k)%units = 'secs'
                F(k)%info_type  = 'period'
            end do
        case ('DUMMY')
            ! empty line
        case default
            ind = find_data_type(Data,TFname)
            if (ind == 0) then
                write(0,*) 'Error: data type for TF name ',trim(TFname),' not allocated: please update your tags'
                stop
            end if
            select case (trim(stat))
            case ('EXP','')
                ! initialize data to zero (not NaN!) to make this work
                if (Data(ind)%Type%isScalar) then
                    Data(ind)%Matrix(:,1,1) = Data(ind)%Matrix(:,1,1) + cvalue
                else
                    Data(ind)%Matrix(:,row,col) = Data(ind)%Matrix(:,row,col) + cvalue
                end if
            case ('ROT')
                Data(ind)%Rot(:) = dreal(cvalue) ! WILL USE THIS TO OVERWRITE ELECTRIC CHANNEL ORIENTATIONS
            case ('VAR')
                Data(ind)%Var(:,row,col) = dreal(cvalue)
            case ('ERR')
                Data(ind)%Var(:,row,col) = dreal(cvalue)**2
            case default
                ! but there might be something else that requires parsing
                write(0,*) 'Warning: data component ',trim(TFcomp),' not recognized; ignored'
            end select
        end select
      end do

  end subroutine read_edi_data

  ! TF names that are supported:
  ! Z, RHO, PHS, ZSTRIKE, ZSKEW, ZELLIP
  ! T, TIPMAG, TIPPHS, TSTRIKE, TSKEW, TELLIP, INDMAG, INDANG
  ! Statistics that are supported:
  ! VAR, ERR
  ! We also intent to support (but haven't parsed yet):
  ! COH, EPREDCOH, HPREDCOH, SIGAMP, SIGNOISE
  ! Exceptions that are not supported:
  ! SPECTRA, RES1D, DEP1D
  ! & COV statistic (which doesn't make sense unless it's pairwise between TF components)
  subroutine parse_edi_data_block_name(blockdef,TFname,row,col,type,stat)
    character(*), intent(in)    :: blockdef ! full name of data block
    character(80), intent(out)  :: TFname ! ROT etc from block definition
    integer, intent(out)  	    :: row,col ! row & column in a matrix
    character(4),  intent(out)  :: type ! real/imag
    character(80), intent(out)  :: stat ! VAR, ERR, etc
    character(len=200)          :: line, var, value, data_block
    character(len=80)           :: TFcomp,info,str,str1,str2
    integer			            :: i,j,lenstr,len1,len2,idot
    logical                     :: isComplex

    str = adjustl(blockdef)
    i = index(str,' ')
    if (i>0) then
        TFcomp = trim(str(1:i-1))
        info = trim(str(i+2:80))
    else
        TFcomp = trim(str)
        info = ''
    end if

    ! separate the TF component name into before & after the dot
    lenstr = len_trim(TFcomp)
    idot = index(TFcomp,'.')
    if (idot>0) then
        len1 = idot - 1
        len2 = lenstr - len1 - 1
    else
        len1 = lenstr
        len2 = 0
    end if
    str1 = trim(TFcomp(1:len1))
    str2 = trim(TFcomp(len1+2:lenstr))

    ! deal with a special case: rotations (ZROT,TROT,RHOROT)
    i = index(str1,'ROT')
    if (i>0) then
        str1 = trim(str1(1:len1-3))
        len1 = len1 - 3
        str2 = 'ROT'
        len2 = 3
    end if

    ! deal with another special case: TXVAR.EXP, TYVAR.EXP:
    ! if VAR comes before the dot, cut it out out into stat
    i = index(str1,'VAR')
    if (i>0) then
        str1 = trim(str1(1:len1-3))
        len1 = len1 - 3
        str2 = 'VAR'
        len2 = 3
    end if

    ! real/imag
    if (str1(len1:len1) .eq. 'R') then
        len1 = len1 - 1
        isComplex = .true.
        type = 'real'
    else if (str1(len1:len1) .eq. 'I') then
        len1 = len1 - 1
        isComplex = .true.
        type = 'imag'
    else
        isComplex = .false.
        type = 'real'
    end if

    ! statistic: VAR, ERR etc
    select case (trim(str2))
    case ('EXP','')
        stat = ''
    case default
        stat = trim(str2)
    end select

    ! now, separate into TFname and X/Y
    row = 1
    col = 1
    if (index(str1,'XX')>0) then
        row = 1
        col = 1
        TFname = str1(1:len1-2)
    elseif (index(str1,'XY')>0) then
        row = 1
        col = 2
        TFname = str1(1:len1-2)
    elseif (index(str1,'YX')>0) then
        row = 2
        col = 1
        TFname = str1(1:len1-2)
    elseif (index(str1,'YY')>0) then
        row = 2
        col = 2
        TFname = str1(1:len1-2)
    elseif (index(str1,'X')>0) then
        row = 1
        col = 1
        TFname = str1(1:len1-1)
    elseif (index(str1,'Y')>0) then
        row = 1
        col = 2
        TFname = str1(1:len1-1)
    else
        TFname = trim(str1(1:len1))
    end if

    if (.not.silent) then
        if (len_trim(stat)>0) then
            write(*,*) 'Reading ',trim(stat),' for variable ',trim(TFname),' from block ',trim(TFcomp)
        else
            write(*,*) 'Reading variable ',trim(TFname),' from block ',trim(TFcomp)
        end if
    end if


    select case (trim(TFcomp))
    case ('FREQ')
    case ('ZROT','TROT','RHOROT')
    case ('ZXXR','ZXXI','ZYYR','ZYYI','ZXYR','ZXYI','ZYXR','ZYXI')
    case ('ZXXR.VAR','ZXXI.VAR','ZYYR.VAR','ZYYI.VAR','ZXYR.VAR','ZXYI.VAR','ZYXR.VAR','ZYXI.VAR')
    case ('ZXX.VAR','ZYY.VAR','ZXY.VAR','ZYX.VAR')
    case ('ZXX.COV','ZYY.COV','ZXY.COV','ZYX.COV')
    case ('TXR.EXP','TXI.EXP','TYR.EXP','TYI.EXP')
    case ('TXVAR.EXP','TYVAR.EXP')
    case ('TIPMAG','TIPPHS')
    case ('TIPMAG.VAR','TIPPHS.VAR')
    case ('TIPMAG.ERR','TIPPHS.ERR')
    case ('RHOXX','RHOYY','RHOXY','RHOYX')
    case ('RHOXX.VAR','RHOYY.VAR','RHOXY.VAR','RHOYX.VAR')
    case ('RHOXX.ERR','RHOYY.ERR','RHOXY.ERR','RHOYX.ERR')
    case ('PHSXX','PHSYY','PHSXY','PHSYX')
    case ('PHSXX.VAR','PHSYY.VAR','PHSXY.VAR','PHSYX.VAR')
    case ('PHSXX.ERR','PHSYY.ERR','PHSXY.ERR','PHSYX.ERR')
    case ('ZSTRIKE','ZSKEW','ZELLIP')
    case ('TSTRIKE','TSKEW','TELLIP')
    case ('INDMAGR.EXP','INDMAGI.EXP','INDANGR.EXP','INDANGI.EXP')
    case ('COH','EPREDCOH','HPREDCOH','SIGAMP','SIGNOISE')
    case ('RES1D','DEP1D')
    case ('SPECTRA')
    case ('DUMMY')
        ! empty line
    case default
        ! but there might be something else that requires parsing
        write(0,*) 'Warning: unknown DATA block ',trim(TFcomp),' encountered and ignored!'
    end select


  end subroutine parse_edi_data_block_name



  subroutine end_edi_input

    close(edifile)
  end subroutine end_edi_input

end module edi_read
