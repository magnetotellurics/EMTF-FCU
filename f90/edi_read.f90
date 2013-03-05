module edi_read

  use global
  use utils

  implicit none
  private

  integer                        :: edifile
  character(len=200)             :: temp
  logical                        :: new_block, new_section
  character(len=80)              :: this_block, this_section
  integer                        :: ios,i,j

  save   :: edifile
  save   :: new_block, new_section
  save   :: this_block, this_section

  public :: initialize_edi_input
  public :: read_edi_header
  public :: read_edi_channels
  public :: read_edi_data_header
  public :: read_edi_data_block
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
     i = index(trim(fname),'.edi')
     if (i > 0) then
        sitename = trim(fname(1:i-1))
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
            Info%AcquiredBy = value
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
            Info%ProcessDate = datestr(value,Info%DateFormat,'YYYY-MM-DD')
            if (.not. silent) then
                write(*,*) 'Process date in XML format: ',trim(Info%ProcessDate)
            end if
        case ('COUNTRY')
            country = value
        case ('STATE')
            state = value
        case ('COUNTY')
            county = value
        case ('PROSPECT')
            Info%Survey = value
        case ('LOC')
            Site%Description = value
        case ('LAT')
            Site%Location%lat=dms2deg(value)
            if (.not. silent) then
                write(*,*) 'Latitude conversion: ',trim(value),' to ',Site%Location%lat
            end if
        case ('LONG')
            Site%Location%lon=dms2deg(value)
            if (.not. silent) then
                write(*,*) 'Longitude conversion: ',trim(value),' to ',Site%Location%lon
            end if
        case ('ELEV')
            Site%Location%elev=dms2deg(value)
        case ('UNITS') ! units for elevation
            elevunits = value
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
        case ('DUMMY')
            ! empty line
        case default
            write(0,*) 'Warning: HEAD block ',trim(var),' value ',trim(value),' ignored'
        end select
    end do

    ! use sitename to for siteID
    Site%ID = trim(sitename)

    ! typical case: ENDDATE not present in the EDI file; use ACQDATE
    if(isempty(Site%End)) then
        Site%End = Site%Start
    end if
    Info%YearCollected = datestr(Site%End,'XML','YYYY')

    ! typical case: LOC not present; use COUNTRY etc for site description
    if(isempty(Site%Description)) then
        Site%Description = trim(country)//', '//trim(state)//', '//trim(county)
    end if

    ! convert elevation to meters
    if(trim(elevunits) .eq. 'FT') then
        Site%Location%elev = 0.3048 * Site%Location%elev
    end if

    ! make sure the longitude is -180 to 180
    if(Site%Location%lon > 180.0d0) then
		Site%Location%lon = Site%Location%lon - 360.0d0
	end if

  end subroutine read_edi_header


  subroutine read_edi_info(Site,Info,Notes,n)
    type(Site_t),  intent(out)            :: Site
    type(UserInfo_t), intent(inout)       :: Info
    character(200), dimension(:), pointer :: Notes
    integer, intent(out)                  :: n
    ! local
    character(len=200)                    :: line, var, value
    character(len=2)                      :: elevunits
    logical                               :: readlat,readlon,readelev
    logical                               :: readinfo

    elevunits = 'M'

    read (edifile,'(a200)',iostat=ios) line !read the first line in file
    call parse_edi_line(line,var,value)
    if (.not. (trim(var) == 'MAXINFO')) then
        write(0,*) 'Error reading the first line of EDI INFO block:'
        write(0,*) line
    end if
    read (value,'(i8)',iostat=ios) n

    if (associated(Notes)) deallocate(Notes)

    allocate(Notes(n))

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
        case ('LATITUDE') ! assume it's decimal if found in INFO block
            read(value,*) Site%Location%lat
            readlat = .true.
        case ('LONGITUDE') ! assume it's decimal if found in INFO block
            read(value,*) Site%Location%lon
            readlon = .true.
        case ('ELEVATION')
            read(value,*) Site%Location%elev
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

    end do

    ! convert elevation to meters
    if(trim(elevunits) .eq. 'FT') then
        Site%Location%elev = 0.3048 * Site%Location%elev
    end if

  end subroutine read_edi_info


  subroutine parse_edi_channel(value, Channel)
    character(len=*), intent(in)     :: value
    type(Channel_t), intent(out)     :: Channel
    ! local
    character(len=200)               :: line, var
    character(len=1)                 :: edichar1, edichar2
    integer                          :: i, j(10), N=200

    ! Use the first non-space character to determine channel type
    call init_channel_info(Channel)
    temp = adjustl(value)
    N = len_trim(value)
    i = 0
    j(:) = 0
    edichar1 = temp(1:1)

    select case (edichar1)
    case ('E')
        ! electric field channel
        i = index(trim(temp),'CHTYPE=')
        if (i > 0) then
            var = trim(temp(i+7:i+8))
            edichar2 = var(2:2) !temp(i+8:i+8)
        else
            write(*,*) 'Error: unknown EDI channel type'
            return
        end if
        Channel%ID = trim(var)
        call init_channel_units(Channel)
        ! read in the numeric ID
        i = index(trim(temp),'ID=')
        read(temp(i+3:N),*) Channel%NumericID
        ! read in the XYZ coordinates
        i = index(trim(temp),'X=')
        read(temp(i+2:N),*) Channel%X
        i = index(trim(temp),'Y=')
        read(temp(i+2:N),*) Channel%Y
        i = index(trim(temp),'Z=')
        if (i > 0) then
            read(temp(i+2:N),*) Channel%Z
        end if
        i = index(trim(temp),'X2=')
        read(temp(i+3:N),*) Channel%X2
        i = index(trim(temp),'Y2=')
        read(temp(i+3:N),*) Channel%Y2
        i = index(trim(temp),'Z2=')
        if (i > 0) then
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
            write(*,*) 'Warning: optional metadata ignored for channel ',trim(Channel%ID)
        end if

    case ('H')
        ! magnetic field channel
        i = index(trim(temp),'CHTYPE=')
        if (i > 0) then
            var = trim(temp(i+7:i+9))
        else
            write(*,*) 'Error: unknown EDI channel type'
            return
        end if
        Channel%ID = trim(var)
        call init_channel_units(Channel)
        ! read in the numeric ID
        i = index(trim(temp),'ID=')
        read(temp(i+3:N),*) Channel%NumericID
        ! read in the XYZ coordinates
        i = index(trim(temp),'AZM=')
        read(temp(i+4:N),*) Channel%Orientation
        i = index(trim(temp),'X=')
        read(temp(i+2:N),*) Channel%X
        i = index(trim(temp),'Y=')
        read(temp(i+2:N),*) Channel%Y
        i = index(trim(temp),'Z=')
        if (i > 0) then
            read(temp(i+2:N),*) Channel%Z
        end if
        i = index(trim(temp),'DIP=')
        if (i > 0) then
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
            write(*,*) 'Warning: optional metadata ignored for channel ',trim(Channel%ID)
        end if

    case default
        write(*,*) 'Warning: not a known EDI channel line: ',trim(value)
        return
    end select

  end subroutine parse_edi_channel

  ! From the channel block >=DEFINEMEAS we obtain:
  ! Site XYZ cartesian coordinates relative to an origin;
  ! Site lat/lon/z location (IF not present in the header OR info blocks);
  ! Channel location *relative to the site* (NOT to the origin);
  ! Dipole lengths;
  ! Channel orientations unless overwritten by data rotation angles;
  ! Other instrument information.
  subroutine read_edi_channels(Input, Output, Site, UserInfo)
    type(Channel_t), dimension(:), pointer, intent(out) :: Input
    type(Channel_t), dimension(:), pointer, intent(out) :: Output
    type(Site_t), intent(inout)                  :: Site
    type(UserInfo_t), intent(in)                 :: UserInfo
    ! local
    type(Channel_t)                  :: Channel
    character(len=200)               :: line, var, value
    integer                          :: num,chin,chout,i,istat
    real(8)                          :: xy(2,1),ll(2,1)

    nch = 5 ! default number of channels; number of input channels hardcoded
    nchout = nch - nchin
    allocate(Input(nchin), Output(nchout), stat=istat)

    chin = 0 ! start counting input channels
    chout = 0 ! start counting output channels
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
            read(value,*) nch
            nchout = nch - nchin
            deallocate(Output, stat=istat)
            allocate(Output(nchout), stat=istat)
        case ('BLOCK')
            num = num+1
            call parse_edi_channel(value,Channel)
            if (num<=nchin) then
                chin = chin+1
                Input(chin) = Channel
            else
                chout = chout+1
                Output(chout) = Channel
            end if
        case ('REFLOC')
            Site%Coords%Origin%ID = trim(value)
        case ('REFLAT')
            Site%Coords%Origin%lat=dms2deg(value)
            if (.not. silent) then
                write(*,*) 'Latitude conversion for the origin: ',trim(value),' to ',Site%Coords%Origin%lat
            end if
        case ('REFLONG')
            Site%Coords%Origin%lon=dms2deg(value)
            if (.not. silent) then
                write(*,*) 'Longitude conversion for the origin: ',trim(value),' to ',Site%Coords%Origin%lon
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
        do i=1,nchin
            call convert_channel_to_meters(Input(i))
        end do
        do i=1,nchout
            call convert_channel_to_meters(Output(i))
        end do
    end if

    ! now, obtain site coordinates relative to an origin from the channels
    ! (use the first input channel - usually Hx - for the absolute "site location")
    Site%Coords%X = Input(1)%X
    Site%Coords%Y = Input(1)%Y
    Site%Coords%Z = Input(1)%Z

    ! adjust the channel location so that it's now relative to the site
    do i=1,nchin
        Input(i)%X = Input(i)%X - Site%Coords%X
        Input(i)%Y = Input(i)%Y - Site%Coords%Y
        Input(i)%Z = Input(i)%Z - Site%Coords%Z
        Input(i)%Units = Site%Coords%Units
    end do
    do i=1,nchout
        Output(i)%X = Output(i)%X - Site%Coords%X
        Output(i)%Y = Output(i)%Y - Site%Coords%Y
        Output(i)%Z = Output(i)%Z - Site%Coords%Z
        ! for electric output channels, also update the end of the dipole
        if (index(Output(i)%ID,'E')>0) then
            Output(i)%X2 = Output(i)%X2 - Site%Coords%X
            Output(i)%Y2 = Output(i)%Y2 - Site%Coords%Y
            Output(i)%Z2 = Output(i)%Z2 - Site%Coords%Z
        end if
        Output(i)%Units = Site%Coords%Units
     end do

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
        write(*,*) 'By default, using recorded location'
    end if

  end subroutine read_edi_channels


  subroutine read_edi_data_header(nf,maxblks)
    integer, intent(out) :: nf,maxblks
   ! local
    character(len=200)               :: line, var, value
    character(len=3)                 :: temp
    integer                          :: num
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


  subroutine read_edi_data_block(nf,type,data,info,comp,row,col)
    integer, intent(in)         :: nf ! number of freq to read
    real(8), intent(out)        :: data(nf) ! real data values
    character(80), intent(out)  :: info ! ROT etc from block definition
    character(80), intent(out)  :: type ! which variable (e.g., TF, TFVAR etc)
    character(4),  intent(out)  :: comp ! real/imag
    integer, intent(out), optional  :: row,col ! row & column in a matrix
    character(len=200)               :: line, var, value, data_block
    character(len=20000)             :: datalist

    data_block = this_block

    do
        read (edifile,'(a200)',iostat=ios) line
        if ((ios /= 0) .or. (trim(this_block) .eq. 'END')) then
            exit
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

    read (datalist, *, iostat=ios) data
    if (ios /= 0) then
        write(*,*) 'Warning: unable to read ',nf,' data values from block ',trim(data_block)
    end if

    select case (trim(data_block))
    case ('FREQ')
        type = 'FREQ'
    case ('ZROT','RHOROT','TROT.EXP')
        type = 'ROTATION'
    case ('ZXXR','ZXXI','ZYYR','ZYYI','ZXYR','ZXYI','ZYXR','ZYXI')
        type = 'TF'
    case ('ZXXR.VAR','ZXXI.VAR','ZYYR.VAR','ZYYI.VAR','ZXYR.VAR','ZXYI.VAR','ZYXR.VAR','ZYXI.VAR')
        type = 'TFVARCMPLX'
    case ('ZXX.VAR','ZYY.VAR','ZXY.VAR','ZYX.VAR')
        type = 'TFVAR'
    case ('ZXX.COV','ZYY.COV','ZXY.COV','ZYX.COV')
        type = 'TFCOV'
    case ('TXR.EXP','TXI.EXP','TYR.EXP','TYI.EXP')
        type = 'TF'
    case ('TXVAR.EXP','TYVAR.EXP')
        type = 'TFVAR'
    case ('TIPMAG','TIPPHS')
        type = 'TIPPER'
    case ('TIPMAG.VAR','TIPPHS.VAR')
        type = 'TIPPER'
    case ('TIPMAG.ERR','TIPPHS.ERR')
    case ('RHOXX','RHOYY','RHOXY','RHOYX')
    case ('RHOXX.VAR','RHOYY.VAR','RHOXY.VAR','RHOYX.VAR')
    case ('RHOXX.ERR','RHOYY.ERR','RHOXY.ERR','RHOYX.ERR')
    case ('PHSXX','PHSYY','PHSXY','PHSYX')
    case ('PHSXX.VAR','PHSYY.VAR','PHSXY.VAR','PHSYX.VAR')
    case ('PHSXX.ERR','PHSYY.ERR','PHSXY.ERR','PHSYX.ERR')
    case ('ZSTRIKE','ZSKEW','ZELLIP')
    case ('TSTRIKE','TSKEW','TELLIP')
    case ('INDMAGR.EXP','INDMAGI.EXP','INDANGR.EXP')
    case ('COH','EPREDCOH','HPREDCOH','SIGAMP','SIGNOISE')
    case ('SPECTRA')
    case ('DUMMY')
        ! empty line
    case default
        ! but there might be something else that requires parsing
        write(0,*) 'Warning: unknown DATA block ',trim(data_block),' encountered and ignored!'
    end select


  end subroutine read_edi_data_block


  subroutine end_edi_input

    close(edifile)
  end subroutine end_edi_input

end module edi_read
