module edi_read

  use global
  use utils

  implicit none
  private

  integer                        :: edifile
  character(len=200)             :: temp, block_info_line, spectra_line
  logical                        :: new_block, new_section
  character(len=80)              :: this_block, this_section
  integer                        :: missing_value_count=0
  character(len=80)              :: dummy_data_value
  integer                        :: ios,i,j,k,maxblks=0
  integer                        :: Ex=0,Ey=0,Hx=0,Hy=0,Hz=0,Rx=0,Ry=0

  save   :: edifile
  save   :: new_block, new_section
  save   :: this_block, this_section
  save   :: missing_value_count
  save   :: dummy_data_value
  save   :: maxblks
  save   :: block_info_line, spectra_line
  save   :: Ex,Ey,Hx,Hy,Hz,Rx,Ry

  public :: initialize_edi_input
  public :: read_edi_header
  public :: read_edi_channels
  public :: read_edi_data_header
  public :: read_edi_data_block
  public :: read_edi_data
  public :: read_edi_spectra
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
            block_info_line = value
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
        write(0,*) trim(line)
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
            if (.not. (trim(var) .eq. 'DATA')) then
                write(*,*) 'Reading ',trim(var),' line: ',trim(value)
            end if
        end if

        select case (trim(var))
        case ('DATAID')
            ! do nothing: info extracted from file name more reliable
        case ('ACQBY')
            if (len_trim(Info%AcquiredBy) == 0) then ! prefer the XML config info
                Info%AcquiredBy = trim(value)
            end if
        case ('FILEBY') ! only use this if the information is missing from config
            if (len_trim(Info%ProcessedBy) == 0) then
                Info%ProcessedBy = trim(value)
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
    character(len=200)                    :: line, var, value, tmp
    character(len=4)                      :: year
    character(len=2)                      :: month,date
    character(len=8)                      :: time
    character(len=2)                      :: elevunits
    logical                               :: readlat,readlon,readelev
    logical                               :: readinfo
    integer                               :: i1,ii

    elevunits = 'M'

    read (edifile,'(a200)',iostat=ios) line !read the first line in file
    call parse_edi_line(line,var,value)
    if (.not. (trim(var) == 'MAXINFO')) then
        write(0,*) 'Error reading the first line of EDI INFO block:'
        write(0,*) trim(line)
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

            if (index(to_upper(Info%AcquiredBy),'PHOENIX')>0) then

                ! parse special Phoenix info format - for now, do nothing
                ! write(*,*) 'DEBUG PHOENIX: ',trim(line)

                    ii = index(line,'START-UP')
                    if (ii>0) then
                        read(line(ii+10:ii+20),*) year
                        read(line(ii+15:ii+20),*) month
                        read(line(ii+18:ii+20),*) date
                        read(line(ii+23:ii+32),*) time
                        Site%Start = year//'-'//month//'-'//date//'T'//time
                        write(*,*) 'Start time inferred from Phoenix INFO: ',Site%Start
                    end if

                    ii = index(line,'END-TIME')
                    if (ii>0) then
                        read(line(ii+10:ii+20),*) year
                        read(line(ii+15:ii+20),*) month
                        read(line(ii+18:ii+20),*) date
                        read(line(ii+23:ii+32),*) time
                        Site%End = year//'-'//month//'-'//date//'T'//time
                        write(*,*) 'End time inferred from Phoenix INFO: ',Site%End
                    end if

            else

                ! In the special case of "EDI file changed by program edi_dec",
                ! whatever that program is... read the declination
                if (trim(var) .eq. 'Declination for this location') then
                    read(value,*) Site%Declination
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
                        case ('AZIMUTH','Declination for this location')
                            !read(value,*) Site%Declination
                        case ('LATITUDE','SITE LATITUDE') ! assume it's decimal if found in INFO block
                            read(value,*) Site%Location%lat
                            readlat = .true.
                        case ('LONGITUDE','SITE LONGITUDE') ! assume it's decimal if found in INFO block
                            read(value,*) Site%Location%lon
                            readlon = .true.
                        case ('ELEVATION','SITE ELEVATION','Elev') ! read elevation and convert to meters if needed
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

            end if ! End the Info%ProcessedBy if statement

        end if

    end do

    if (.not.silent) then
        write(*,*) 'Before exiting the INFO block, latitude  = ',Site%Location%lat
        write(*,*) 'Before exiting the INFO block, longitude = ',Site%Location%lon
        write(*,*) 'Before exiting the INFO block, elevation = ',Site%Location%elev
        write(*,*) 'Before exiting the INFO block, declination = ',Site%Declination
    end if

  end subroutine read_edi_info


  subroutine parse_edi_channel(value, Channel, need_second_line, orientation_exists)
    character(len=*), intent(in)     :: value
    type(Channel_t), intent(inout)   :: Channel
    logical, intent(inout), optional :: need_second_line
    logical, intent(inout), optional :: orientation_exists
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
    if (present(orientation_exists)) then
        orientation_exists = .false.
    end if

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
        ! define the default azimuths, to be overwritten if present in file
        select case (edichar2)
        case ('X')
            Channel%Orientation = 0.0
        case ('Y')
            Channel%Orientation = 90.0
        end select
        ! read in the numeric ID
        i1 = index(trim(temp),'ID=')
        i2 = index(trim(temp),'ID =')
        i = max(i1,i2+1)
        read(temp(i+3:N),*,iostat=istat) Channel%NumericID
        ! read in channel azimuth
        i1 = index(trim(temp),'AZM=')
        i2 = index(trim(temp),'AZM =')
        i = max(i1,i2+1)
        if (i>1) then
            read(temp(i+4:N),*,iostat=istat) Channel%Orientation
            if (present(orientation_exists)) then
                orientation_exists = .true.
            end if
        end if
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
            edichar2 = var(2:2)
        else
            write(0,*) 'Error: unknown EDI channel type'
            return
        end if
        Channel%ID = trim(var)
        call init_channel_units(Channel)
        ! define the default azimuths, to be overwritten if present in file
        select case (edichar2)
        case ('X')
            Channel%Orientation = 0.0
        case ('Y')
            Channel%Orientation = 90.0
        case ('Z')
            Channel%Orientation = 0.0
        end select
        ! read in the numeric ID
        i1 = index(trim(temp),'ID=')
        i2 = index(trim(temp),'ID =')
        i = max(i1,i2+1)
        read(temp(i+3:N),*,iostat=istat) Channel%NumericID
        ! read in channel azimuth
        i1 = index(trim(temp),'AZM=')
        i2 = index(trim(temp),'AZM =')
        i = max(i1,i2+1)
        if (i>1) then
            read(temp(i+4:N),*,iostat=istat) Channel%Orientation
            if (present(orientation_exists)) then
                orientation_exists = .true.
            end if
        end if
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

    !write(0,*) 'DEBUG ID,AZM,X,Y,X2,Y2: ',Channel%ID,Channel%Orientation,Channel%X,Channel%Y,Channel%X2,Channel%Y2

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
    logical                          :: electric_channels_first,channel_on_two_lines,orientation_exists
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
                if (num==nch+1) then
                    Rx = num
                elseif (num==nch+2) then
                    Ry = num
                end if
                write(0,*) 'Warning: remote channel number ',num,' will not be copied to the new file format'
            else
                call parse_edi_channel(value,Channel(num),channel_on_two_lines,orientation_exists)
                if (channel_on_two_lines) then
                    read (edifile,'(a200)',iostat=ios) line
                    value = trim(value)//trim(line)
                    call parse_edi_channel(value,Channel(num))
                end if
                if (trim(Channel(num)%Type) .eq. 'H') then
                    hch = hch+1
                    if (Channel(num)%ID(2:2) .eq. 'X') then
                        Hx = num
                        if (.not. orientation_exists) then
                            write(0,*) 'Missing orientation of ', trim(Channel(num)%ID),' set to : ',Channel(1)%Orientation
                            Channel(num)%Orientation = Channel(1)%Orientation
                        end if
                    elseif (Channel(num)%ID(2:2) .eq. 'Y') then
                        Hy = num
                        if (.not. orientation_exists) then
                            write(0,*) 'Missing orientation of ', trim(Channel(num)%ID),' set to : ',Channel(2)%Orientation
                            Channel(num)%Orientation = Channel(2)%Orientation
                        end if
                    elseif (Channel(num)%ID(2:2) .eq. 'Z') then
                        Hz = num
                    end if
                elseif (trim(Channel(num)%Type) .eq. 'E') then
                    ech = ech+1
                    if (Channel(num)%ID(2:2) .eq. 'X') then
                        Ex = num
                        if (.not. orientation_exists) then
                            write(0,*) 'Missing orientation of ', trim(Channel(num)%ID),' set to : ',Channel(1)%Orientation
                            Channel(num)%Orientation = Channel(1)%Orientation
                        end if
                    elseif (Channel(num)%ID(2:2) .eq. 'Y') then
                        Ey = num
                        if (.not. orientation_exists) then
                            write(0,*) 'Missing orientation of ', trim(Channel(num)%ID),' set to : ',Channel(2)%Orientation
                            Channel(num)%Orientation = Channel(2)%Orientation
                        end if
                    end if
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

    ! if no remote channels are encountered in file, assume single station
    if (Rx==0) then
        Rx = Hx
    end if
    if (Ry==0) then
        Ry = Hy
    end if

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
    ! A.K. UPDATE as of 8 Oct 2017: turns out that this adjustment is actually
    ! unnecessary and confusing; leave the channel coordinates alone as they were
    !do i=1,nch
    !    Channel(i)%X = Channel(i)%X - Site%Coords%X
    !    Channel(i)%Y = Channel(i)%Y - Site%Coords%Y
    !    Channel(i)%Z = Channel(i)%Z - Site%Coords%Z
    !    ! for electric output channels, also update the end of the dipole
    !    if (trim(Channel(i)%Type) .eq. 'E') then
    !        Channel(i)%X2 = Channel(i)%X2 - Site%Coords%X
    !        Channel(i)%Y2 = Channel(i)%Y2 - Site%Coords%Y
    !        Channel(i)%Z2 = Channel(i)%Z2 - Site%Coords%Z
    !    end if
    !    Channel(i)%Units = Site%Coords%Units
    !end do

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
    if (UserInfo%AddDeclToSiteLayout) then
        if (.not.silent) then
            write(*,*) 'Adding declination to site layout to indicate original geomagnetic'
        end if
        do i=1,2
            Input(i)%Orientation = Input(i)%Orientation + Site%Declination
        end do
        do i=1,size(OutputE)
            OutputE(i)%Orientation = OutputE(i)%Orientation + Site%Declination
        end do
    end if

  end subroutine read_edi_channels


  subroutine read_edi_data_header(nf,nch,spectraFile)
    integer, intent(inout) :: nf
    integer, intent(inout), optional :: nch
    logical, intent(inout), optional :: spectraFile
   ! local
    character(len=200)               :: line, var, value
    integer                          :: num
    character(len=80)                :: chname

    if (present(spectraFile)) then
        spectraFile = .false.
    end if

    maxblks = 100

    do
        read (edifile,'(a200)',iostat=ios) line
        if ((ios /= 0) .or. (trim(this_block) .eq. 'END')) then
            exit
        end if
        call parse_edi_line(line,var,value)
        if (new_block) then
            ! if dealing with SPECTRA, save the full data block header
            if (trim(this_section) .eq. 'SPECTRASECT') then
                spectra_line = value
            end if
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
            if (present(spectraFile)) then
                spectraFile = .true.
            end if
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
        if (.not.silent .and. (trim(var) .ne. 'DATA')) then
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
    integer		  	    :: i,j,k,ind,istat,irot
    real(8)             :: rotval ! scalar rotation value sometimes used in block header
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
        case ('')
            ! encountered a statistical estimate that does not pertain to a specific TF, skip over for now
            ! ... these are: COH,PREDCOH,EPREDCOH,HPREDCOH,SIGAMP,SIGNOISE
        case default
            ind = find_data_type(Data,TFname)
            if (ind == 0) then
                write(0,*) 'Warning: data type for TF name ',trim(TFname),' not allocated, ignored. Please update your tags to fix this.'
                cycle
            end if
            select case (trim(stat))
            case ('EXP','')
                ! initialize data to zero (not NaN!) to make this work
                if (Data(ind)%Type%isScalar) then
                    Data(ind)%Matrix(:,1,1) = Data(ind)%Matrix(:,1,1) + cvalue
                else
                    Data(ind)%Matrix(:,row,col) = Data(ind)%Matrix(:,row,col) + cvalue
                end if
                ! if a rotation value is specified in the block header, use that in place of rotation block
                irot = index(block_info_line,'ROT=')
                if (irot>0) then
                    read(block_info_line(irot+4:len_trim(block_info_line)),*,iostat=istat) rotval
                    if (istat==0) then
                        write(0,*) 'Overwriting the rotation value for data type ',trim(TFname),' with ',rotval
                        Data(ind)%Rot(:) = rotval
                        Data(ind)%orthogonal = .true.
                    end if
                end if
            case ('ROT')
                Data(ind)%Rot(:) = dreal(cvalue)
                Data(ind)%orthogonal = .true.
            case ('VAR')
                Data(ind)%Var(:,row,col) = dreal(cvalue)
            case ('ERR')
                Data(ind)%Var(:,row,col) = dreal(cvalue)**2
            case default
                ! but there might be something else that requires parsing
                write(0,*) 'Warning: data component ',trim(this_block),' not recognized; ignored'
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
  ! (Note: SPECTRA are supported through read_edi_spectra)
  ! Exceptions that are not supported:
  ! RES1D, DEP1D
  ! & COV statistic (which doesn't make sense unless it's pairwise between TF components)
  subroutine parse_edi_data_block_name(blockdef,TFname,row,col,type,stat)
    character(*), intent(in)    :: blockdef ! full name of data block
    character(80), intent(out)  :: TFname ! ROT etc from block definition
    integer, intent(out)        :: row,col ! row & column in a matrix
    character(4),  intent(out)  :: type ! real/imag
    character(80), intent(out)  :: stat ! VAR, ERR, etc
    character(len=200)          :: line, var, data_block
    character(len=80)           :: TFcomp,info,str,str1,str2
    integer                     :: i,j,lenstr,len1,len2,idot,irot
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

    ! deal with a special case: coherences (COH,PREDCOH,EPREDCOH,HPREDCOH)
    i = index(str1,'COH')
    if (i>0) then
        stat = trim(str1)
        TFname = ''
    end if

    ! deal with a special case: signal amplitude and noise (SIGAMP,SIGNOISE)
    i = index(str1,'SIG')
    if (i>0) then
        stat = trim(str1)
        TFname = ''
    end if

    if (.not.silent) then
        if (len_trim(stat)>0 .and. len_trim(TFname)>0) then
            write(*,*) 'Reading ',trim(stat),' for variable ',trim(TFname),' from block ',trim(TFcomp)
        elseif (len_trim(stat)>0) then
            write(*,*) 'Reading statistic ',trim(stat),': parsing not implemented yet, will be ignored'
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
    case ('COH','PREDCOH','EPREDCOH','HPREDCOH','SIGAMP','SIGNOISE')
    case ('RES1D','DEP1D')
    case ('SPECTRA')
    case ('DUMMY')
        ! empty line
    case default
        ! but there might be something else that requires parsing
        write(0,*) 'Warning: unknown DATA block ',trim(TFcomp),' encountered and ignored!'
    end select


  end subroutine parse_edi_data_block_name


  subroutine read_edi_spectra_block(nch,data)
    integer, intent(in)         :: nch ! number of channels incl remote; read nch^2
    real(8), intent(out)        :: data(nch,nch) ! real data values
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
        if (.not.silent .and. (trim(var) .ne. 'DATA')) then
            write(*,*) 'Reading ',trim(var),' line: ',trim(value)
        end if
        ! if we enter a new block, save the header and exit
        if (new_block) then
            spectra_line = value
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
        write(*,*) 'Warning: unable to read ',nch*nch,' data values from block ',trim(data_block)
    end if
    data = transpose(data)

  end subroutine read_edi_spectra_block

! Wrote by Anna Kelbert, 24 Sep 2017 to calculate the full covariance matrix
! from EDI spectra files. Using Xavi Garcia's edi2edi code as the basis, but
! reading the EDI file independently, and computing the full covariances
! instead of the approximate variance. No rotation of spectra because we can
! later rotate using the covariance matrices.
! The order of channels is very important for SPECTRA files; we are recording
! this order in Ex,Ey,Hx,Hy,Hz,Rx,Ry integers. We do not "unpack" the cross-
! power spectra matrix but merely refer to the row and columns by channel.
! On input, assuming Data(1) for the tipper, Data(2) for impedances
! but checking for this anyway.
  subroutine read_edi_spectra(nf,nch,F,Data)
    type(FreqInfo_t), intent(inout) :: F(:) ! periods
    type(Data_t), intent(inout)     :: Data(:) ! one for each data type
    integer, intent(inout)          :: nf ! number of freq to read
    integer, intent(inout)          :: nch ! number of channels incl remote
    ! local variables
    !real(8), dimension(:,:,:), allocatable :: rspectra
    !real(8), dimension(:,:),   allocatable :: tspectra
    !real(8), dimension(:),     allocatable :: rotspec,bw,avgt
    !complex(8), dimension(:,:), allocatable :: cavg
    !integer                                :: ch(7)
    real(8)             :: freq
    real(8)             :: rotspec,bw,avgt
    real(8)             :: value(nch,nch) ! real data values
    complex(8)          :: cavg(nch,nch) ! complex data values
    character(4)        :: type ! real/imag
    integer             :: row,col ! row & column in a matrix
    integer             :: i,j,k,n,ind,istat,imp,tip
    character(len=80)   :: TFcomp, TFname, info, stat
    ! ... extend the definition of "E" to all output channels, incl. Hz
    ! nch-4 stands for all channels minus 2 input minus 2 remote
    real(8)      :: TFVar(nch-4,2)
    complex(8)   :: Z(nch-4,2), Zh(2,nch-4), ResidCov(nch-4,nch-4), InvSigCov(2,2)
    complex(8)   :: RhH(2,2), RhE(2,nch-4), HhE(2,nch-4), RhR(2,2), EhE(nch-4,nch-4), HhH(2,2)
    complex(8)   :: czero
    real(8)      :: delta
    logical      :: rew
    !character(len=12), dimension(7,2) :: chID

    czero = dcmplx(0.0d0,0.0d0)
    delta = 0e0
    Z = czero
    Zh = czero
    ResidCov = czero
    InvSigCov = czero
    TFVar = 0.0d0

    imp = find_data_type(Data,'Z')
    if (imp == 0) then
        write(0,*) 'Error: data storage not allocated for impedance in read_spectra'
        stop
    end if
    tip = find_data_type(Data,'T')
    if ((tip == 0) .and. (Ex.eq.0) .and. (Ey.ne.0)) then
        write(0,*) 'Warning: data storage not allocated for tipper in read_spectra, while tipper data are present'
    else if ((Ex.eq.0) .or. (Ey.eq.0)) then
        write(0,*) 'Warning: not trying to read tipper data in read_spectra, because the electric field channels are missing'
        tip = 0
    end if

    do k=1,nf
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
        temp = spectra_line
        call parse_edi_spectra_block_name(temp,freq,rotspec,bw,avgt)
        call init_freq_info(F(k))
        F(k)%value = 1.0d0/freq
        if (.not.silent) then
            write(*,'(a25,a10,a2,i4,a7,es14.3)') 'Reading DATA from block ',trim(this_block),' #',k,': FREQ=',freq
        end if
        call read_edi_spectra_block(nch,value)
        !rspectra(j,:,:) = value
        ! unpack spectra matrix but do not rotate - set delta to zero
        ! (we may rotate later using the covariance matrices)
        ! also do not switch order of rows and columns.
        !call unpackspectra(nch,value,delta,cavg,ch)
        do i=1,nch
            do j=i,nch
                if (i==j) then
                    cavg(i,j) = dcmplx(value(i,j), 0e0)
                else
                    cavg(i,j) = dcmplx(value(j,i), -value(i,j))
                    cavg(j,i) = dcmplx(value(j,i),  value(i,j))
                end if
            end do
        end do

        ! initialize all cross-spectra matrices
        ! R* H
        RhH(1,1) = cavg(Rx,Hx)
        RhH(1,2) = cavg(Rx,Hy)
        RhH(2,1) = cavg(Ry,Hx)
        RhH(2,2) = cavg(Ry,Hy)
        !RhH = matconjg2(RhH)
        ! R* R
        RhR(1,1) = cavg(Rx,Rx)
        RhR(1,2) = cavg(Rx,Ry)
        RhR(2,1) = cavg(Ry,Rx)
        RhR(2,2) = cavg(Ry,Ry)
        !RhR = matconjg2(RhR)
        ! H* H
        HhH(1,1) = cavg(Hx,Hx)
        HhH(1,2) = cavg(Hx,Hy)
        HhH(2,1) = cavg(Hy,Hx)
        HhH(2,2) = cavg(Hy,Hy)
        !HhH = matconjg2(HhH)

        if ( tip.ne.0 ) then
            ! R* E
            RhE(1,1) = cavg(Rx,Hz)
            RhE(1,2) = cavg(Rx,Ex)
            RhE(1,3) = cavg(Rx,Ey)
            RhE(2,1) = cavg(Ry,Hz)
            RhE(2,2) = cavg(Ry,Ex)
            RhE(2,3) = cavg(Ry,Ey)
            !RhE = matconjg2(RhE)
            ! H* E
            HhE(1,1) = cavg(Hx,Hz)
            HhE(1,2) = cavg(Hx,Ex)
            HhE(1,3) = cavg(Hx,Ey)
            HhE(2,1) = cavg(Hy,Hz)
            HhE(2,2) = cavg(Hy,Ex)
            HhE(2,3) = cavg(Hy,Ey)
            !HhE = matconjg2(HhE)
            ! E* E
            EhE(1,1) = cavg(Hz,Hz)
            EhE(1,2) = cavg(Hz,Ex)
            EhE(1,3) = cavg(Hz,Ey)
            EhE(2,1) = cavg(Ex,Hz)
            EhE(2,2) = cavg(Ex,Ex)
            EhE(2,3) = cavg(Ex,Ey)
            EhE(3,1) = cavg(Ey,Hz)
            EhE(3,2) = cavg(Ey,Ex)
            EhE(3,3) = cavg(Ey,Ey)
            !EhE = matconjg2(EhE)
        else
            ! R* E
            RhE(1,1) = cavg(Rx,Ex)
            RhE(1,2) = cavg(Rx,Ey)
            RhE(2,1) = cavg(Ry,Ex)
            RhE(2,2) = cavg(Ry,Ey)
            !RhE = matconjg2(RhE)
            ! H* E
            HhE(1,1) = cavg(Hx,Ex)
            HhE(1,2) = cavg(Hx,Ey)
            HhE(2,1) = cavg(Hy,Ex)
            HhE(2,2) = cavg(Hy,Ey)
            !HhE = matconjg2(HhE)
            ! E* E
            EhE(1,1) = cavg(Ex,Ex)
            EhE(1,2) = cavg(Ex,Ey)
            EhE(2,1) = cavg(Ey,Ex)
            EhE(2,2) = cavg(Ey,Ey)
            !EhE = matconjg2(EhE)
        end if

        ! calculate impedances:
        if (abs(RhH(1,1)*RhH(2,2) - RhH(1,2)*RhH(2,1)) < epsilon(dreal(RhH))) then
            write(0,'(a50,i3,a30)') 'Warning: matrix determinant too small for period #',k,'. Results could be inaccurate.'
        end if
        Zh = matmul(matinv2(RhH), RhE)
        Z = matconjg(Zh)

        ! calculate full covariances for remote reference
        InvSigCov = matmul(matinv2(RhH), matmul(RhR, matinv2(matconjg(RhH))))
        ResidCov = (EhE - matmul(matconjg(Zh),HhE) - matmul(matconjg(HhE),Zh) &
            + matmul(matconjg(Zh), matmul(HhH,Zh))) / avgt

        do i=1,nch-4
            do j=1,2
                TFVar(i,j) = (ResidCov(i,i)*InvSigCov(j,j))
            end do
        end do

        !write(0,*) 'DEBUG: ',k, TFVar

        ! save in Data
        if (tip.ne.0) then
            Data(tip)%Rot(k) = rotspec
            Data(tip)%Matrix(k,:,:) = Z(1:1,:)
            Data(tip)%Var(k,:,:) = TFVar(1:1,:)
            Data(tip)%InvSigCov(k,:,:) = InvSigCov
            Data(tip)%ResidCov(k,:,:) = ResidCov(1:1,1:1)
            Data(tip)%fullcov = .true.
            Data(imp)%Rot(k) = rotspec
            Data(imp)%Matrix(k,:,:) = Z(2:nch-4,:)
            Data(imp)%Var(k,:,:) = TFVar(2:nch-4,:)
            Data(imp)%InvSigCov(k,:,:) = InvSigCov
            Data(imp)%ResidCov(k,:,:) = ResidCov(2:nch-4,2:nch-4)
            Data(imp)%fullcov = .true.
        else
            Data(imp)%Rot(k) = rotspec
            Data(imp)%Matrix(k,:,:) = Z
            Data(imp)%Var(k,:,:) = TFVar
            Data(imp)%InvSigCov(k,:,:) = InvSigCov
            Data(imp)%ResidCov(k,:,:) = ResidCov
            Data(imp)%fullcov = .true.
        end if

    end do

    ! Find the order in which the different EM channels are:
    !call findchannels(edifile, ch, chID, rew = .TRUE.)

    !allocate(rspectra(nf,nch,nch),tspectra(nch,nch),cavg(nch,nch), STAT=istat)

    !call readspectra(edifile,nf,nch,freq,rspectra,rotspec,bw,avgt)

    !freq_loop_sp: do n=1, nf

        !tspectra = rspectra(n,:,:)

        ! unpack spectra matrix but do not rotate - set delta to zero
        ! (we may rotate later using the covariance matrices)
        !call unpackspectra(nch,tspectra,delta,cavg,ch)

    !end do freq_loop_sp


  end subroutine read_edi_spectra

! Example line to parse:
!>SPECTRA  FREQ=3.200E+02 ROTSPEC=0 BW=8.0000E+01 AVGT=9.1888E+03 // 49
  subroutine parse_edi_spectra_block_name(value, freq, rotspec, bw, avgt)
    character(len=*), intent(in)     :: value
    real(8), intent(out)             :: freq, rotspec, bw, avgt
    ! local
    integer                          :: i, i1, i2, N=200, istat

    temp = adjustl(value)
    N = len_trim(value)
    i1 = 0
    i2 = 0
    i = 0
    freq = 0.0d0
    rotspec = 0.0d0
    bw = 0.0d0
    avgt = 0.0d0

    if (.not. silent) then
        write(*,*) 'Parsing SPECTRA block: ',value
    end if

    ! read in frequency
    i1 = index(trim(temp),'FREQ=')
    i2 = index(trim(temp),'FREQ =')
    i = max(i1,i2+1)
    if (i <= 0) then
        write(*,*) 'Error: unknown frequency for SPECTRA block!!!'
        return
    end if
    read(temp(i+5:N),*,iostat=istat) freq
    if (istat .ne. 0) then
        write(*,*) 'Error: unable to read frequency for SPECTRA block!!!'
    end if
    ! read in the rotation
    i1 = index(trim(temp),'ROTSPEC=')
    i2 = index(trim(temp),'ROTSPEC =')
    i = max(i1,i2+1)
    read(temp(i+8:N),*,iostat=istat) rotspec
    ! read in bw and avgt
    i1 = index(trim(temp),'BW=')
    i2 = index(trim(temp),'BW =')
    i = max(i1,i2+1)
    read(temp(i+3:N),*,iostat=istat) bw
    i1 = index(trim(temp),'AVGT=')
    i2 = index(trim(temp),'AVGT =')
    i = max(i1,i2+1)
    read(temp(i+5:N),*,iostat=istat) avgt

  end subroutine parse_edi_spectra_block_name


  subroutine end_edi_input

    close(edifile)
  end subroutine end_edi_input

end module edi_read
