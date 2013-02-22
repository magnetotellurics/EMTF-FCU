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

        select case (trim(var))
        case ('AZIMUTH')
            read(value,*) Site%Location%lat
        case ('LATITUDE') ! assume it's decimal if found in INFO block
            read(value,*) Site%Location%lat
        case ('LONGITUDE') ! assume it's decimal if found in INFO block
            read(value,*) Site%Location%lon
        case ('ELEVATION')
            read(value,*) Site%Location%elev
        case ('INFO','DUMMY','COMMENT')
            ! these are already saved in the Notes if needed
        case default
            ! but there might be something else that requires parsing
            write(0,*) 'Warning: INFO block ',trim(var),' value ',trim(value),' ignored'
        end select
    end do

  end subroutine read_edi_info


  subroutine parse_edi_channel(value, Channel)
    character(len=200), intent(in)   :: value
    type(Channel_t), intent(out)     :: Channel
    ! local
    character(len=200)               :: line, var
    character(len=1)                 :: edichar1, edichar2
    integer                          :: i, N=200

    ! Use the first two non-space characters to determine channel type
    temp = adjustl(line)
    edichar1 = temp(1:1)
    edichar2 = temp(2:2)
    select case (edichar1)
    case ('E')
        ! electric field channel
        i = index(trim(temp),'CHTYPE=')
        if (i > 0) then
            var = trim(temp(i:i+2))
        end if
        write(*,*) 'Channel ',trim(var)
    case ('H')
        ! magnetic field channel
        i = index(trim(temp),'CHTYPE=')
        if (i > 0) then
            var = trim(temp(i:i+2))
        end if
        write(*,*) 'Channel ',trim(var)
    case default
        write(*,*) 'Warning: not a known EDI channel line: ',trim(value)
        return
    end select


!    do i=1,2
!       read (edifile,*) num, orientation, tilt, temp, chname
!       call init_channel_info(Input(i))
!       Input(i)%ID = chname
!       Input(i)%orientation = orientation + declination
!       Input(i)%tilt = tilt
!       call init_channel_units(Input(i))
!    end do
!
!    do i=1,nch-2
!       read (edifile,*) num, orientation, tilt, temp, chname
!       call init_channel_info(Output(i))
!       Output(i)%ID = chname
!       Output(i)%orientation = orientation + declination
!       Output(i)%tilt = tilt
!       call init_channel_units(Output(i))
!    end do

  end subroutine parse_edi_channel


  subroutine read_edi_channels(Input, Output, UserInfo, RemoteSite)
    type(Channel_t), dimension(:), intent(inout) :: Input
    type(Channel_t), dimension(:), intent(inout) :: Output
    type(UserInfo_t), intent(inout)              :: UserInfo
    type(Site_t), intent(inout)                  :: RemoteSite
    ! local
    type(Channel_t)                  :: Channel
    character(len=200)               :: line, var, value
    character(len=2)                 :: elevunits
    real(8)                          :: declination
    character(len=3)                 :: temp
    integer                          :: num
    character(len=80)                :: chname
    real                             :: orientation
    real                             :: tilt

    nch = 5 ! default number of channels; replaces the number preallocated on input

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
            read(value,*) nch
        case ('BLOCK')
            call parse_edi_channel(value,Channel)
        case ('REFLOC')
            UserInfo%RemoteRef = .TRUE.
            UserInfo%RemoteRefType = 'Remote Reference'
            UserInfo%RemoteSiteID = trim(value)
        case ('REFLAT')
            RemoteSite%Location%lat=dms2deg(value)
            if (.not. silent) then
                write(*,*) 'Latitude conversion: ',trim(value),' to ',RemoteSite%Location%lat
            end if
        case ('REFLONG')
            RemoteSite%Location%lon=dms2deg(value)
            if (.not. silent) then
                write(*,*) 'Longitude conversion: ',trim(value),' to ',RemoteSite%Location%lon
            end if
        case ('REFELEV')
            read(value,*) RemoteSite%Location%elev
        case ('UNITS') ! units for elevation
            elevunits = value
        case ('REFTYPE')
            ! this isn't used correctly in practical EDI files
            ! UserInfo%RemoteRefType = trim(value)
        case ('MAXRUN','MAXMEAS')
            ! do nothing - these aren't really used in practice
        case ('DUMMY')
            ! empty line
        case default
            ! but there might be something else that requires parsing
            write(0,*) 'Warning: DEFINEMEAS block ',trim(var),' value ',trim(value),' ignored'
        end select
    end do

    ! save the remote site ID
    RemoteSite%ID = UserInfo%RemoteSiteID

    ! convert elevation to meters
    if(trim(elevunits) .eq. 'FT') then
        RemoteSite%Location%elev = 0.3048 * RemoteSite%Location%elev
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
