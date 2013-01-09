module global

  implicit none
  public

  integer, save         :: nch, nf
  logical, save         :: silent=.false.
  logical, save         :: rotate=.false.
  character(len=10)     :: date, time, zone
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
  character(len=16)     :: sign_convention='exp(+ i\omega t)'
  !*********************************************************
  ! The units are always going to be non-SI
  ! character(len=12)     :: units='[mV/km]/[nT]'
  character(len=12)     :: magnetic_field_units='[nT]'
  character(len=12)     :: electric_field_units='[mV/km]'
  !*********************************************************
  ! Double precision constants
  real(8), parameter    :: PI  = 3.14159265357898
  real(8), parameter	:: D2R = PI/180.d0
  real(8), parameter	:: R2D = 180.d0/PI
  real(8), parameter    :: EPS = 1.0e-8
  !*********************************************************


  !************************************************************************
  ! MT TF info block
  character(len=2)      :: network='EM'
  character(len=20)     :: subType='MT_TF'
  character(len=200)    :: subTypeInfo='Magnetotelluric Transfer Functions'
  character(len=20)     :: tags='impedance,tipper'


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
    character(len=80) :: Year
    character(len=80) :: DOI
    character(len=800):: RelatedPublication ! optionally replaces data DOI
    character(len=80) :: ReleaseStatus
    character(len=100):: ConditionsOfUse(100)
  end type Copyright_t

  type :: UserInfo_t
    character(len=2)  :: Network
    character(len=80) :: SubType
    character(len=80) :: Description
    character(len=80) :: Tags
    type(Copyright_t) :: Copyright
    character(len=80) :: Project
    character(len=80) :: Survey
    character(len=80) :: YearCollected    
    character(len=80) :: AcquiredBy
    type(Person_t)	  :: Creator
    type(Person_t)	  :: Submitter
    character(len=80) :: ProcessedBy
    character(len=80) :: ProcessingSoftware
    character(len=80) :: ProcessingSoftwareLastMod
    character(len=80) :: ProcessingSoftwareAuthor
    integer           :: OrthogonalGeographic
    character(len=80) :: RunList
    character(len=80) :: SiteList
    character(len=80) :: ChannelList
  end type UserInfo_t

  type :: Location_t
    character(10)     :: datum
	real(8)           :: lat
	real(8)           :: lon
	real(8)           :: elev
  end type Location_t


  type :: TimePeriod_t
	character(len=6)  :: RunID
	character(len=19) :: StartTime
	character(len=19) :: EndTime
  end type TimePeriod_t


  type :: Site_t
  	character(len=5)   :: ID
	character(len=80)  :: Description
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
  	character(len=6)   :: ID
  	character(len=5)   :: SiteID
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
  end type Channel_t


  type :: FreqInfo_t
	character(len=10) :: info_type
	real(8)           :: value
	integer           :: num_points
	character(len=10) :: units
	integer           :: dec_level
  end type FreqInfo_t


  type :: RemoteRef_t
  	!character(len=80) :: processed_by
  	character(len=80) :: processing_tag
  	character(len=80) :: sign_convention
  	!character(len=80) :: software
  	!character(len=80) :: software_lastmod
  	!character(len=80) :: software_author
	character(len=80) :: remote_ref_type
	logical           :: remote_ref
	character(len=5)  :: remote_site_id
  	!type(Site_t)     :: remote_site
	!character(len=2) :: remote_ref_abbrev
  end type RemoteRef_t

contains

	subroutine init_copyright(Info)
		type(Copyright_t), intent(out)  :: Info
		! local
		integer i

		Info%Title = 'USArray TA Magnetotelluric Transfer Functions'
		Info%Authors = ''
		Info%Year = ''
		Info%DOI = 'Unassigned'
        Info%RelatedPublication = ''
		Info%ReleaseStatus = 'UNKNOWN'
		do i=1,size(Info%ConditionsOfUse)
		    Info%ConditionsOfUse = ''
		end do
		
	end subroutine init_copyright

    subroutine init_user_info(Info)
        type(UserInfo_t), intent(out)  :: Info

        Info%Network = network
        Info%SubType = subType
        Info%Description = subTypeInfo
        Info%Tags = tags
        call init_copyright(Info%Copyright)
        Info%Project = 'USArray'
        Info%Survey = 'TA'
        Info%YearCollected = ''
        Info%AcquiredBy = 'UNKNOWN'
        call init_person(Info%Creator)
        call init_person(Info%Submitter)
        Info%ProcessedBy = 'UNKNOWN'
        Info%ProcessingSoftware = 'UNKNOWN'
        Info%ProcessingSoftwareLastMod = 'UNKNOWN'
        Info%ProcessingSoftwareAuthor = 'UNKNOWN'
        Info%OrthogonalGeographic = 0
        Info%RunList = 'Runs.xml'
        Info%SiteList = 'Sites.xml'
        Info%ChannelList = 'Channels.xml'

    end subroutine init_user_info

	subroutine init_person(Person)
		type(Person_t), intent(out)  :: Person

		Person%Name = 'UNKNOWN'
		Person%Email = 'UNKNOWN'
		Person%Org = 'UNKNOWN'
		Person%OrgUrl = 'UNKNOWN'
		Person%OrgLogoUrl = 'UNKNOWN'

	end subroutine init_person

	subroutine init_site_info(Site)
		type(Site_t), intent(out)  :: Site

		Site%ID = ' '
		Site%Description = ' '
        Site%Location%datum = datum
		Site%Location%lon = 0.0d0
		Site%Location%lat = 0.0d0
		Site%Location%elev = 0.0d0
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
		Run%Location%datum = datum
		Run%Location%lon = 0.0d0
		Run%Location%lat = 0.0d0
		Run%Location%elev = 0.0d0
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


	subroutine init_remote_ref(Info)
		type(RemoteRef_t), intent(out)  :: Info

		!Info%processed_by = ' '
		Info%processing_tag = ' '
		Info%sign_convention = sign_convention
		!Info%software = ' '
		!Info%software_lastmod = ' '
		!Info%software_author = ' '
  		Info%remote_ref_type = ' '
		Info%remote_ref = .FALSE.
		Info%remote_site_id = ' '

		!call init_site_info(Info%remote_site)

	end subroutine init_remote_ref


	subroutine init_freq_info(Freq)
		type(FreqInfo_t), intent(inout) :: Freq

		Freq%info_type = 'period'
		Freq%value = 0.0d0
		Freq%num_points = 0
		Freq%units = 'secs'
		Freq%dec_level = 0

	end subroutine init_freq_info

    subroutine init_channel_info(Channel)
         type(Channel_t), intent(inout) :: Channel

		Channel%ID = ' '
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

    end subroutine init_channel_info

    subroutine init_channel_units(Channel)
         type(Channel_t), intent(inout) :: Channel

         if (index(Channel%ID,'H')==1) then
            Channel%Units = magnetic_field_units
         else if (index(Channel%ID,'E')==1) then
            Channel%Units = electric_field_units
         end if

         if (.not. silent) then
             write(*,*) trim(Channel%ID)//' '//trim(Channel%Units)
         end if

    end subroutine init_channel_units
    
    
    function TF_name(InputChannel,OutputChannel) result (tfname)
		  type(Channel_t), intent(in)    :: InputChannel, OutputChannel
		  character(10)					 :: tfname
		  
		  if (index(OutputChannel%ID,'H')==1) then
		     tfname = 'T'//trim(InputChannel%ID(2:10))
		  else if (index(OutputChannel%ID,'E')==1) then
		     tfname = 'Z'//trim(OutputChannel%ID(2:10))//trim(InputChannel%ID(2:10))
		  else
		     tfname = 'UNKNOWN'
		  end if
		  
	end function TF_name

end module global
