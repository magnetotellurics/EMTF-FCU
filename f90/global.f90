module global

  implicit none
  public

  !integer, save         :: nf, ndt
  logical, save         :: dry=.false.
  logical, save         :: silent=.false.
  logical, save         :: rotate=.false.
  character(len=10)     :: date, time, zone
  !*********************************************************
  ! General data types and statistical estimate information
  ! is read from a folder called DATATYPES. The location of
  ! this folder can be hardcoded here. If the code is run
  ! from a directory that contains DATATYPES and COPYRIGHT
  ! folders, homedir is overwritten with the current directory
  character(len=80)     :: homedir='/Users/akelbert/Developer/EMTF-FCU/f90/'
  !*********************************************************
  ! IRIS requires site ID to have no more than 5 chars
  ! respectively, run ID has no more than 6 chars
  ! This restriction does not hold to data not archived
  ! with IRIS, so make this a parameter... just be careful
  ! that the computed 5-digit IRIS ID is always unique
  integer, parameter    :: nid=16
  !*********************************************************
  ! WGS84 - common standard global datum
  ! NAD83 - used in North America
  ! ETRS89 - used in Europe
  ! For example, in Sydney there is a 200 m  difference
  ! between GPS coordinates configured in GDA (based on
  ! global standard WGS84) and AGD (used for most local maps)
  character(len=10)     :: datum='WGS84'
  !*********************************************************
  ! The version only changes if the XML schema changes
  character(len=3)      :: version='3.0'
  !*********************************************************
  ! The sign convention is always going to be +1
  ! except when not! - e.g., if Larsen's code was used,
  ! in which case this should be set in configuration file
  character(len=16)     :: sign_convention='exp(+ i\omega t)'
  !*********************************************************
  ! The units are always going to be non-SI
  ! character(len=12)     :: units='[mV/km]/[nT]'
  character(len=12)     :: magnetic_field_units='[nT]'
  character(len=12)     :: electric_field_units='[mV/km]'
  !*********************************************************


  !************************************************************************
  ! MT TF info block
  ! In general, site names are unique within the 2-char Network.
  ! However, all MT stuff has been given a single network name ('EM').
  ! Recently, that has been changed, and new MT surveys will have unique
  ! networks. Still, most of the historic MT is in 'EM'.
  character(len=2)      :: network='EM'
  character(len=20)     :: subType='MT_TF'
  character(len=200)    :: subTypeInfo='Magnetotelluric Transfer Functions'
  character(len=200)    :: tags='impedance,tipper'


!  type :: Dimensions_t
!    integer           :: f ! number of frequencies/periods
!    integer           :: dt ! number of data types (from tags)
!    integer           :: ch ! total number of channels = nchin + nchout
!    integer           :: chin ! number of input channels
!    integer           :: chout ! number of output channels
!    integer           :: choutE ! number of output electric channels
!    integer           :: choutH ! number of output magnetic channels
!  end type Dimensions_t

  type :: Person_t
    character(len=80) :: Name
    character(len=80) :: Email
    character(len=80) :: Org
    character(len=80) :: OrgUrl
    character(len=80) :: OrgLogoUrl
  end type Person_t

  type :: Copyright_t
    character(len=800):: Title
    character(len=800):: Authors
    character(len=80) :: Year ! or year range
    character(len=80) :: DOI ! for this site; assigned automatically by IRIS DMC
    character(len=80) :: SurveyDOI ! for the survey; assigned once in the config.xml file
    character(len=80) :: PAPERS ! selected publications file name
    character(len=800):: SelectedPublications(100) ! necessary for ReleaseStatus = "Paper Citation Required"
    character(len=80) :: THANKS ! acknowledgements file name
    character(len=800):: Acknowledgement(100)
    character(len=80) :: ReleaseStatus ! string; determines which fixed conditions of use to apply
    character(len=800):: ConditionsOfUse(100)
    character(len=80) :: README ! additional info file name
    character(len=800):: AdditionalInfo(100)
  end type Copyright_t

  type :: UserInfo_t
    logical           :: TimeSeriesArchived
    character(len=2)  :: Network
    character(len=80) :: SubType
    character(len=400):: Description
    character(len=400):: Tags
    character(len=400):: ExternalUrl
    character(len=400):: ExternalUrlInfo
    type(Copyright_t) :: Copyright
    character(len=80) :: Project
    character(len=400):: Survey
    character(len=80) :: Country
    character(len=80) :: YearCollected    
    character(len=400):: AcquiredBy
    type(Person_t)	  :: Creator
    type(Person_t)	  :: Submitter
    character(len=80) :: SignConvention
    logical           :: RemoteRef
    character(len=80) :: RemoteRefType
    character(len=nid):: RemoteSiteID
    character(len=80) :: ProcessedBy
    character(len=80) :: ProcessDate
    logical           :: IgnoreProcessDateInFile ! for EDI input to use when FILEDATA is meaningless
    character(len=80) :: ProcessingSoftware
    character(len=80) :: ProcessingSoftwareLastMod
    character(len=80) :: ProcessingSoftwareAuthor
    character(len=80) :: ProcessingTag ! for Z-file input/output
    character(len=80) :: Estimate(8) ! statistical estimate types
    character(len=80) :: DateFormat ! for EDI input/output
    character(len=80) :: DummyDataValue ! for EDI input/output
    character(len=80) :: DefaultSiteName ! for EDI input/output
    logical           :: IgnoreSiteNameInFile ! for EDI input to use when site name is meaningless
    integer           :: DefaultDataQuality ! a priori data quality from 1 to 5 (0 = unrated)
    character(len=400):: DataQualityComment ! information about survey data quality
    logical           :: ComputeSiteCoords ! for EDI input/output
    logical           :: ParseEDIInfo ! for EDI input/output
    logical           :: WriteEDIInfo ! for EDI input/output
    logical           :: MetadataOnly ! true to produce XML file with no data
    integer           :: OrthogonalGeographic
    character(len=80) :: Basename ! base name of the original file to be submitted
    character(len=10) :: Image ! extension of the image file, if present
    character(len=10) :: Original ! extension of the original file to be submitted
    character(len=80) :: Attachment ! full name for optional survey attachment file
    character(len=400):: AttachmentInfo ! description of the survey attachment file
    character(len=80) :: RunList
    character(len=80) :: SiteList
    character(len=80) :: ChannelList
  end type UserInfo_t

  type :: Location_t
    character(10)     :: datum
	real(8)           :: lat
	real(8)           :: lon
	real(8)           :: elev
    character(80)     :: ID
  end type Location_t

  type :: XYZ_t
    character(10)     :: Type
    character(10)     :: Units
    real(8)           :: X
    real(8)           :: Y
    real(8)           :: Z
    type(Location_t)  :: Origin
  end type XYZ_t

  type :: TimePeriod_t
	character(len=nid+1) :: RunID
	character(len=19) :: StartTime
	character(len=19) :: EndTime
  end type TimePeriod_t

  type :: Site_t
  	character(len=nid) :: ID
    character(len=5)   :: IRIS_ID
	character(len=80)  :: Description
	type(XYZ_t)        :: Coords
  	type(Location_t)   :: Location
	real(8)            :: Declination
	integer			   :: QualityRating ! 1-5
	real(8)			   :: GoodFromPeriod
	real(8)			   :: GoodToPeriod
	character(len=200) :: QualityComments
    integer            :: WarningFlag ! 0/1
    character(len=200) :: WarningComments
  	character(len=19)  :: Start
	character(len=19)  :: End
	character(len=200) :: RunList
  end type Site_t


  type :: Run_t
  	character(len=nid+1) :: ID
  	character(len=nid)   :: SiteID
   	character(len=80)  :: Instrument
	character(len=80)  :: InstrumentName
 	character(len=80)  :: InstrumentID
	character(len=200) :: Manufacturer
  	type(Location_t)   :: Location
	real(8)            :: Declination
	type(TimePeriod_t) :: TimePeriod
	real(8)            :: Ex_wire_length
	real(8)            :: Ey_wire_length
	real(8)            :: SamplingRate
	real(8)            :: SamplingInterval
	character(len=80)  :: SiteInstalledBy
	character(len=80)  :: MetaDataCheckedBy
	character(len=400) :: FieldComments
	character(len=400) :: Comments
	character(len=400) :: Errors
  end type Run_t


  type :: Channel_t
	character(len=10)  :: ID
	real               :: NumericID ! from the EDI files
	character(len=10)  :: Type ! E/H
    character(len=10)  :: Intention ! Input/Output
	real               :: DipoleLength
	real               :: DipoleAzimuth ! instrument orientation
	real               :: Orientation   ! orientation in final TF
	real               :: Tilt
	real               :: X,Y,Z,X2,Y2,Z2  ! location relative to site
	type(Location_t)   :: Location
	character(len=12)  :: Units
	character(len=80)  :: Instrument
	character(len=200) :: InstrumentType
	character(len=200) :: Manufacturer
	character(len=80)  :: InstrumentName
	character(len=80)  :: InstrumentID
	character(len=80)  :: InstrumentConfig
    character(len=80)  :: Sensor ! specific run details if known (e.g., from EDI)
    character(len=80)  :: Filter
    character(len=80)  :: Gain
    character(len=19)  :: MeasuredDate
  end type Channel_t


  type :: FreqInfo_t
	character(len=10) :: info_type
	real(8)           :: value
	integer           :: num_points
	character(len=10) :: units
	integer           :: dec_level
  end type FreqInfo_t


  type :: DataType_t
     character(len=80) :: Intention ! primary/derived data type
     character(len=800):: Description
     character(len=200):: ExternalUrl
     character(len=80) :: Tag   ! e.g., apparent_resisitivity
     character(len=80) :: Name  ! e.g., RHO
     character(len=1)  :: Input ! E/H
     character(len=1)  :: Output ! E/H
     character(len=80) :: Units ! always get them from file! - for derived types, can't compute
     character(len=80) :: DerivedFrom ! primary data tag
     character(len=80) :: SeeAlso ! interpret with (tag list)
     !character(len=80), pointer :: Component(:) ! conceptual components stored in "Names"
     !integer           :: nComp ! number of conceptual components
     logical           :: derivedType = .false.
     logical           :: isScalar = .false.
     logical           :: isComplex = .false.
     logical           :: allocated = .false.
  end type DataType_t


  type :: Data_t
    ! this stores the data type, dimensions, data and all allowed
    ! statistical estimates (not all are used simultaneously)
    ! we are also storing the rotation angles, but these won't be used
    ! except to correct the "channel orientations" - so in fact, I do not
    ! at this point intend to support varying rotation angles in a file.
    ! note that scalar data types are supported by setting nchin & nchout to 1.
    type(DataType_t)        :: Type
    integer                 :: nf, nchin, nchout ! number of input and output channels
    real(8),   dimension(:),     pointer :: Rot  ! rotation angles (FOR ALL FREQUENCIES!)
    complex(8),dimension(:,:,:), pointer :: Matrix ! (nf,nchout,nchin)
    real(8),   dimension(:,:,:), pointer :: Var ! (nf,nchout,nchin)
    complex(8),dimension(:,:,:), pointer :: Cov ! (nf,nchin*nchout,nchin*nchout)
    complex(8),dimension(:,:,:), pointer :: InvSigCov ! (nf,nchin,nchin)
    complex(8),dimension(:,:,:), pointer :: ResidCov ! (nf,nchout,nchout)
    complex(8),dimension(:,:,:), pointer :: Coh ! (nf,nchout,nchin)
    complex(8),dimension(:,:),   pointer :: MultCoh ! (nf,nchout)
    complex(8),dimension(:,:),   pointer :: SigAmp ! (nf,nchout)
    complex(8),dimension(:,:),   pointer :: SigNoise ! (nf,nchout)
    logical          :: allocated = .false.
  end type Data_t

contains

  !************************************************************
  ! Total number of INPUT channels is determined by the
  ! method through which the data are obtained.
  ! The MT assumption requires two polarizations, and there
  ! are always 2 input channels. Other types of data that
  ! would potentially be stored in the format may have
  ! 1 (e.g., C responses) or zero (e.g., fields) input channels.
  ! We store this number as a global parameter;
  ! number of OUTPUT channels is much more variable and
  ! is dynamically allocated but initialized to 3
  ! However, rather than using global variables (not safe)
  ! we store all these in a derived data type, which we
  ! initialize and update as needed

!  subroutine init_dimensions(N)
!    type(Dimensions_t), intent(out)  ::N
!
!    N%f = 0
!    N%dt = 0
!
!    N%chin = 2
!    N%choutH = 1
!    N%choutE = 2
!
!    N%chout = N%choutE + N%choutH
!    N%ch = N%chin + N%chout
!
!  end subroutine init_dimensions

    subroutine init_homedir()

        ! local
        logical copyright_dir_exists, datatype_dir_exists

        ! check that the COPYRIGHT and DATATYPES folders exist in current directory;
        ! using a workaround to avoid compiler dependence
        inquire (file='COPYRIGHT/UnrestrictedRelease.copyright',exist=copyright_dir_exists)
        inquire (file='DATATYPES/impedance.xml',exist=datatype_dir_exists)

        ! if they exist, use the current directory to get them; otherwise use homedir above
        if (copyright_dir_exists .and. datatype_dir_exists) then
            homedir = './'
        end if

    end subroutine init_homedir

	subroutine init_copyright(Info)
		type(Copyright_t), intent(out)  :: Info
		! local
		integer i

		Info%Title = 'USArray TA Magnetotelluric Transfer Functions'
		Info%Authors = ''
		Info%Year = ''
		Info%DOI = ''
		Info%SurveyDOI = ''
        Info%THANKS = ''
        do i=1,size(Info%Acknowledgement)
            Info%Acknowledgement = ''
        end do
        Info%PAPERS = ''
        do i=1,size(Info%SelectedPublications)
            Info%SelectedPublications = ''
        end do
		Info%ReleaseStatus = 'UNKNOWN'
		do i=1,size(Info%ConditionsOfUse)
		    Info%ConditionsOfUse = ''
		end do
        Info%README = ''
        do i=1,size(Info%AdditionalInfo)
            Info%AdditionalInfo = ''
        end do

	end subroutine init_copyright

    subroutine init_user_info(Info)
        type(UserInfo_t), intent(out)  :: Info

        Info%TimeSeriesArchived = .FALSE.
        Info%Network = network
        Info%SubType = subType
        Info%Description = subTypeInfo
        Info%Tags = tags
        Info%ExternalUrl = ''
        Info%ExternalUrlInfo = ''
        call init_copyright(Info%Copyright)
        Info%Project = '' ! 'USArray'
        Info%Survey = '' ! 'TA'
        Info%Country = '' ! 'USA'
        Info%YearCollected = ''
        Info%AcquiredBy = ' '
        call init_person(Info%Creator)
        call init_person(Info%Submitter)
        Info%SignConvention = ''
        Info%RemoteRef = .FALSE.
        Info%RemoteRefType = ' '
        Info%RemoteSiteID = ' '
        Info%ProcessedBy = ' '
        Info%ProcessDate = ' '
        Info%IgnoreProcessDateInFile = .FALSE.
        Info%ProcessingSoftware = ' '
        Info%ProcessingSoftwareLastMod = ' '
        Info%ProcessingSoftwareAuthor = ' '
        Info%ProcessingTag = ' '
        Info%DateFormat = 'MM/DD/YY'
        Info%DummyDataValue = ''
        Info%DefaultSiteName = 'UNKNOWN SITE NAME'
        Info%IgnoreSiteNameInFile = .FALSE.
        Info%DefaultDataQuality = 0
        Info%DataQualityComment = ''
        Info%ComputeSiteCoords = .FALSE.
        Info%ParseEDIInfo = .TRUE.
        Info%WriteEDIInfo = .TRUE.
        Info%MetadataOnly = .FALSE.
        Info%OrthogonalGeographic = 0
        Info%Basename = ' '
        Info%Image = ' '
        Info%Original = ' '
        Info%Attachment = ' '
        Info%AttachmentInfo = ' '
        Info%RunList = 'Runs.xml'
        Info%SiteList = 'Sites.xml'
        Info%ChannelList = 'Channels.xml'

        ! hardcode statistical estimates here, but read them from files
        Info%Estimate(1) = 'variance'
        Info%Estimate(2) = 'covariance'
        Info%Estimate(3) = 'inverse_signal_covariance'
        Info%Estimate(4) = 'residual_covariance'
        Info%Estimate(5) = 'coherence'
        Info%Estimate(6) = 'multiple_coherence'
        Info%Estimate(7) = 'signal_amplitude'
        Info%Estimate(8) = 'signal_noise'

    end subroutine init_user_info

	subroutine init_person(Person)
		type(Person_t), intent(out)  :: Person

		Person%Name = 'UNKNOWN'
		Person%Email = 'UNKNOWN'
		Person%Org = 'UNKNOWN'
		Person%OrgUrl = 'UNKNOWN'
		Person%OrgLogoUrl = 'UNKNOWN'

	end subroutine init_person

    subroutine init_location(Location)
        type(Location_t), intent(out)  :: Location

        Location%datum = datum
        Location%lat = 0.0d0
        Location%lon = 0.0d0
        Location%elev = 0.0d0
        Location%ID = ' '

    end subroutine init_location

	subroutine init_site_info(Site)
		type(Site_t), intent(out)  :: Site

		Site%ID = ' '
        Site%IRIS_ID = ' '
		Site%Description = ' '
		Site%Coords%Type = ' '
		Site%Coords%Units = ' '
		Site%Coords%X = 0.0d0
        Site%Coords%Y = 0.0d0
        Site%Coords%Z = 0.0d0
        call init_location(Site%Coords%Origin)
		call init_location(Site%Location)
		Site%Declination = 0.0d0
		Site%QualityRating = 0
		Site%GoodFromPeriod = 0.0d0
		Site%GoodToPeriod = 0.0d0
		Site%QualityComments = ' '
        Site%WarningFlag = -1 ! unknown
        Site%WarningComments = ' '
		Site%Start = ' '
		Site%End = ' '
		Site%RunList = ' '

	end subroutine init_site_info


	subroutine init_run_info(Run)
		type(Run_t), intent(out)  :: Run

		Run%ID = ' '
		Run%SiteID = ' '
		Run%Instrument = ' '
		Run%InstrumentName = ' '
		Run%InstrumentID = ' '
		Run%Manufacturer = ' '
        call init_location(Run%Location)
		Run%Declination = 0.0d0
		Run%TimePeriod%RunID = ' '
		Run%TimePeriod%StartTime = ' '
		Run%TimePeriod%EndTime = ' '
		Run%Ex_wire_length = 0.0d0
		Run%Ey_wire_length = 0.0d0
		Run%SamplingRate = 0.0d0
		Run%SamplingInterval = 0.0d0
		Run%SiteInstalledBy = ' '
		Run%MetaDataCheckedBy = ' '
		Run%FieldComments = ' '
		Run%Comments = ' '
		Run%Errors = ' '

	end subroutine init_run_info


	subroutine init_freq_info(Freq)
		type(FreqInfo_t), intent(inout) :: Freq

		Freq%info_type = 'period'
		Freq%value = 0.0d0
		Freq%num_points = 0
		Freq%units = 'secs'
		Freq%dec_level = 0

	end subroutine init_freq_info

    ! optional argument tag ('impedance'/'tipper') makes it possible
    ! to allocate a base data type without referring to XML read
    ! (convenient for converters such as z2edi which do not use FoX)
	subroutine init_data_type(DataType, tag)
        type(DataType_t), intent(inout) :: DataType
        character(*), intent(in), optional :: tag

	    DataType%Intention = ' '
	    DataType%Description = ' '
        DataType%ExternalUrl = ' '
	    DataType%Tag = ' '
	    DataType%Name = 'NULL'
        DataType%Input = ' '
	    DataType%Output = ' '
        DataType%Units = ' '
        DataType%DerivedFrom = ' '
        DataType%SeeAlso = ' '
        DataType%derivedType = .false.
        DataType%isScalar = .false.
	    DataType%isComplex = .false.
	    DataType%allocated = .false.

	    if (present(tag)) then
	        select case (trim(tag))
	        case ('impedance')
                DataType%Intention = 'primary data type'
                DataType%Description = 'Magnetotelluric Impedance'
                DataType%Tag = 'impedance'
                DataType%Name = 'Z'
                DataType%Input = 'H'
                DataType%Output = 'E'
                DataType%Units = '[mV/km]/[nT]'
                DataType%isComplex = .true.
                DataType%allocated = .true.
	        case ('tipper')
                DataType%Intention = 'primary data type'
                DataType%Description = 'Vertical Field Transfer Functions (Tipper)'
                DataType%Tag = 'tipper'
                DataType%Name = 'T'
                DataType%Input = 'H'
                DataType%Output = 'H'
                DataType%Units = '[]'
                DataType%isComplex = .true.
                DataType%allocated = .true.
	        case default
	            write(0,*) 'Data type not initialized: tag ',trim(tag),' not recognised. Use an XML definition instead.'
            end select
        end if

	end subroutine init_data_type

    ! NOTE: the number of input and output channels here
    !       will be variable depending on the data type
    subroutine init_data(Data, dataType, nf, nchin, nchout)
        type(Data_t), intent(inout)     :: Data
        type(DataType_t), intent(in)    :: dataType
        integer, intent(in)             :: nf, nchout, nchin
        ! local
        real(8) ZERO,NaN
        integer istat

        call deall_data(Data)

        write(*,*) 'Creating ',trim(dataType%Name),' (input=',dataType%Input,'; output=',dataType%Output,')'

        Data%Type = dataType
        Data%nf = nf
        Data%nchin = nchin
        Data%nchout = nchout

        allocate(Data%Rot(nf), stat=istat)
        allocate(Data%Matrix(nf,nchout,nchin), stat=istat)
        allocate(Data%Var(nf,nchout,nchin), stat=istat)
        allocate(Data%Cov(nf,nchin*nchout,nchin*nchout), stat=istat)
        allocate(Data%InvSigCov(nf,nchin,nchin), stat=istat)
        allocate(Data%ResidCov(nf,nchout,nchout), stat=istat)
        allocate(Data%Coh(nf,nchout,nchin), stat=istat)
        allocate(Data%MultCoh(nf,nchout), stat=istat)
        allocate(Data%SigAmp(nf,nchout), stat=istat)
        allocate(Data%SigNoise(nf,nchout), stat=istat)
        Data%allocated = .true.

        ! NaNs are not currently universally supported in Fortran
        ZERO = 0.0d0
        NaN = ZERO/ZERO
!        if (.not. isnan(NaN)) then
!            write(0,*) 'Warning: using NaNs for missing data will not work with this compiler'
!        end if

        ! Reading of complex components is done by addition
        ! so data have to be initialized to zero (non NaNs);
        ! instead, use NaNs to replace the missing data
        Data%Rot = 0.0d0
        Data%Matrix = dcmplx(0.0d0,0.0d0)
        Data%Var = 0.0d0
        Data%Cov = dcmplx(0.0d0,0.0d0)
        Data%InvSigCov = dcmplx(0.0d0,0.0d0)
        Data%ResidCov = dcmplx(0.0d0,0.0d0)
        Data%Coh = dcmplx(0.0d0,0.0d0)
        Data%MultCoh = dcmplx(0.0d0,0.0d0)
        Data%SigAmp = dcmplx(0.0d0,0.0d0)
        Data%SigNoise = dcmplx(0.0d0,0.0d0)

!        Data%Rot = NaN
!        Data%Matrix = dcmplx(NaN,NaN)
!        Data%Var = NaN
!        Data%Cov = dcmplx(NaN,NaN)
!        Data%InvSigCov = dcmplx(NaN,NaN)
!        Data%ResidCov = dcmplx(NaN,NaN)
!        Data%Coh = dcmplx(NaN,NaN)
!        Data%MultCoh = dcmplx(NaN,NaN)
!        Data%SigAmp = dcmplx(NaN,NaN)
!        Data%SigNoise = dcmplx(NaN,NaN)

    end subroutine init_data


    subroutine deall_data(Data)
        type(Data_t), intent(inout)     :: Data
        ! local
        integer istat

        if (Data%allocated) then
            deallocate(Data%Rot, stat=istat)
            deallocate(Data%Matrix, stat=istat)
            deallocate(Data%Var, stat=istat)
            deallocate(Data%Cov, stat=istat)
            deallocate(Data%InvSigCov, stat=istat)
            deallocate(Data%ResidCov, stat=istat)
            deallocate(Data%Coh, stat=istat)
            deallocate(Data%MultCoh, stat=istat)
            deallocate(Data%SigAmp, stat=istat)
            deallocate(Data%SigNoise, stat=istat)
        end if

        Data%allocated = .false.

    end subroutine deall_data


    subroutine init_channel_info(Channel)
         type(Channel_t), intent(inout) :: Channel

		Channel%ID = ' '
		Channel%NumericID = 0
        Channel%Type = ' '
        Channel%Intention = ' '
		Channel%DipoleLength = 0
		Channel%DipoleAzimuth = 0
		Channel%Orientation = 0
		Channel%Tilt = 0
		Channel%X = 0
		Channel%Y = 0
		Channel%Z = 0
		Channel%X2 = 0
		Channel%Y2 = 0
		Channel%Z2 = 0
		Channel%Units = ' '
		Channel%Instrument = ' '
		Channel%InstrumentType = ' '
		Channel%Manufacturer = ' '
		Channel%InstrumentName = ' '
		Channel%InstrumentID = ' '
		Channel%InstrumentConfig = ' '
        Channel%Sensor = ' '
        Channel%Filter = ' '
        Channel%Gain = ' '
        Channel%MeasuredDate = ' '

    end subroutine init_channel_info

    subroutine init_channel_units(Channel)
         type(Channel_t), intent(inout) :: Channel

         if ((index(Channel%ID,'H')==1) .or. (index(Channel%ID,'R')==1)) then
            Channel%Units = magnetic_field_units
         else if (index(Channel%ID,'E')==1) then
            Channel%Units = electric_field_units
         end if

         if (.not. silent) then
             write(*,*) trim(Channel%ID)//' '//trim(Channel%Units)
         end if

    end subroutine init_channel_units
    

    function channel_type(id) result (type)
         character(*), intent(in)   :: id
         character(1)               :: type

         if ((index(id,'H')==1) .or. (index(id,'R')==1)) then
            type = 'H'
         else if (index(id,'E')==1) then
            type = 'E'
         else
            type = ' '
            write(0,*) 'Warning: unable to determine channel type from ',trim(id)
         end if

    end function channel_type


    function find_channel(Channels,name) result (ind)
         type(Channel_t), intent(in)    :: Channels(:)
         character(*), intent(in)       :: name
         integer                        :: ind
         ! local
         integer i

         ind = 0

         do i = 1,size(Channels)
            if (trim(Channels(i)%ID) .eq. trim(name)) then
                ind = i
                return
            end if
         end do

    end function find_channel


    function find_data_type(Data,name) result (ind)
         type(Data_t), intent(in)   	:: Data(:)
         character(*), intent(in)       :: name
         integer                        :: ind
         ! local
         integer i

         ind = 0

         do i = 1,size(Data)
            if (trim(Data(i)%Type%Name) .eq. trim(name)) then
                ind = i
                return
            end if
         end do

    end function find_data_type


    function TF_name(DataType,InputChannel,OutputChannel) result (tfname)
          type(DataType_t), intent(in)   :: DataType
		  type(Channel_t), intent(in), optional    :: InputChannel, OutputChannel
		  character(10)					 :: tfname

          if (present(InputChannel)) then
              select case (DataType%Name)
              case ('T')
                tfname = trim(DataType%Name)//trim(InputChannel%ID(2:10))
              case default
                tfname = trim(DataType%Name)//trim(OutputChannel%ID(2:10))//trim(InputChannel%ID(2:10))
              end select
          else
              tfname = trim(DataType%Name)
          end if

	end function TF_name


    ! cleaning up the EDI units flexibility (use to_upper if needed)
    subroutine convert_channel_to_meters(Channel)
         type(Channel_t), intent(inout) :: Channel

         if (trim(Channel%Units) .eq. 'FT') then
            Channel%X = 0.3048 * Channel%X
            Channel%Y = 0.3048 * Channel%Y
            Channel%Z = 0.3048 * Channel%Z
            if (index(Channel%ID,'E')>0) then
                Channel%X2 = 0.3048 * Channel%X2
                Channel%Y2 = 0.3048 * Channel%Y2
                Channel%Z2 = 0.3048 * Channel%Z2
            end if
            Channel%Units = 'm'
         end if

    end subroutine convert_channel_to_meters



end module global
