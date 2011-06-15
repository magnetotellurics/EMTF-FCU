module global

  implicit none
  public

  integer, save         :: nch, nf
  logical, save         :: silent=.false.
  logical, save         :: rotate=.false.
  character(len=10)     :: date, time, zone
  !*********************************************************
  ! The version only changes if the XML schema changes
  character(len=3)      :: version='0.5'
  !*********************************************************
  ! The network is always 'EM' !!!
  character(len=2)      :: network='EM'
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


  type :: UserInfo_t
    character(len=80) :: Source
    character(len=80) :: Project
    character(len=80) :: Experiment
    integer           :: YearCollected
    integer           :: OrthogonalGeographic
    character(len=80) :: ProcessedBy
    character(len=80) :: ProcessingSoftware
    character(len=80) :: ProcessingTag
    character(len=80) :: RunList
    character(len=80) :: SiteList
  end type UserInfo_t


  type :: Location_t
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
	character(len=80)  :: RunList
  end type Site_t


  type :: Run_t
  	character(len=6)   :: ID
  	character(len=5)   :: SiteID
  	character(len=80)  :: Instrument
  	type(Location_t)   :: Location
	real(8)            :: Declination
	type(TimePeriod_t) :: TimePeriod
	real(8)            :: Ex_wire_length
	real(8)            :: Ey_wire_length
	real(8)            :: SamplingFreq
	real(8)            :: SamplingInterval
	character(len=80)  :: Comments
  end type Run_t


  type :: Channel_t
	character(len=80) :: ID
	real              :: Orientation
	real              :: Tilt
	type(Location_t)  :: Location
	character(len=12) :: Units
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
  	character(len=80) :: processing_id
  	character(len=80) :: sign_convention
  	character(len=80) :: software
  	character(len=80) :: software_version
	character(len=80) :: remote_ref_type
	logical           :: remote_ref
	character(len=5)  :: remote_site_id
  	!type(Site_t)     :: remote_site
	!character(len=2) :: remote_ref_abbrev
  end type RemoteRef_t


contains

	subroutine init_user_info(Info)
		type(UserInfo_t), intent(out)  :: Info

		Info%Source = 'OSU'
		Info%Project = 'USArray'
		Info%Experiment = 'UNKNOWN'
		Info%YearCollected = 2007
		Info%OrthogonalGeographic = 0;
		Info%ProcessedBy = 'UNKNOWN'
		Info%ProcessingSoftware = 'EMTF'
		Info%ProcessingTag = ''
		Info%RunList = 'USArray_2007_Runs.xml'
		Info%SiteList = 'USArray_2007_Sites.xml'

	end subroutine init_user_info


	subroutine init_site_info(Site)
		type(Site_t), intent(out)  :: Site

		Site%ID = ' '
		Site%Description = ' '
		Site%Location%lon = 0.0d0
		Site%Location%lat = 0.0d0
		Site%Location%elev = 0.0d0
		Site%Declination = 0.0d0
		Site%RunList = ' '

	end subroutine init_site_info


	subroutine init_run_info(Run)
		type(Run_t), intent(out)  :: Run

		Run%ID = ' '
		Run%SiteID = ' '
		Run%Instrument = ' '
		Run%Location%lon = 0.0d0
		Run%Location%lat = 0.0d0
		Run%Location%elev = 0.0d0
		Run%Declination = 0.0d0
		Run%TimePeriod%RunID = ' '
		Run%TimePeriod%StartTime = ' '
		Run%TimePeriod%EndTime = ' '
		Run%Ex_wire_length = 0.0d0
		Run%Ey_wire_length = 0.0d0
		Run%SamplingFreq = 0.0d0
		Run%SamplingInterval = 0.0d0
		Run%Comments = ' '

	end subroutine init_run_info


	subroutine init_remote_ref(Info)
		type(RemoteRef_t), intent(out)  :: Info

		!Info%processed_by = ' '
		Info%processing_id = ' '
		Info%sign_convention = sign_convention
		Info%software = ' '
		Info%software_version = ' '
  		Info%remote_ref_type = ' '
		Info%remote_ref = .FALSE.
		Info%remote_site_id = ' '

		!call init_site_info(Info%remote_site)

	end subroutine init_remote_ref


	subroutine init_freq_info(Freq)
		type(FreqInfo_t), intent(out)   :: Freq

		Freq%info_type = 'frequency'
		Freq%value = 0.0d0
		Freq%num_points = 0
		Freq%units = 'Hz'
		Freq%dec_level = 0

	end subroutine init_freq_info


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

end module global
