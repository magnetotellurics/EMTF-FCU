! *****************************************************************************
module dataTypes
  ! This module is adapted from the data type dictionary (typeDict) for ModEM.
  ! Many of these data types are not yet implemented in ModEM 3D_MT code;
  ! however, we choose to maintain naming compatibility for convenience.
  !
  ! This stand-alone module could be used to replace the on in ModEM to ensure
  ! better consistency between ModEM and EMTF FCU codes. It is NOT used by EMTF FCU.
  ! It is also outdated so should only serve as a reference when you start
  ! getting this code to work. *This isn't expected to work out-of-the-box.*
  ! A. Kelbert, July 19, 2024

  use math_constants
  use utilities

  implicit none

  public			:: setup_typeDict, deall_typeDict

  type :: dataType

     logical           :: isComplex = .false.
     character(80)     :: name = ''
     integer	       :: tfType
     character(80)     :: units
     integer           :: nComp ! number of REAL data type components

     ! the (real or complex) data type components in a fixed order as given;
     ! the file can have a different component order on input
     character(15), pointer, dimension(:) :: id

     ! these lists contain the indices into the data vector for each data type;
     ! they make it possible to sort the data by receiver for output
     ! no data denoted by zero index; dimensions (nTx) and (nTx,nRx)
     integer, pointer, dimension(:)   :: tx_index
     integer, pointer, dimension(:)   :: dt_index
     integer, pointer, dimension(:,:) :: rx_index

  end type dataType

  ! data type dictionary must be public
  type (dataType), pointer, save, public, dimension(:) :: typeDict

  ! primary data types
  integer, parameter   :: impedance = 1
  integer, parameter   :: off_diagonal_impedance = 2
  integer, parameter   :: tipper = 3
  integer, parameter   :: interstation_impedance = 4
  integer, parameter   :: interstation_transfer_functions = 5

  ! derived data types
  integer, parameter   :: phase_tensor = 6
  integer, parameter   :: apparent_resistivity_and_phase = 7
  integer, parameter   :: off_diagonal_resistivity_and_phase = 8
  integer, parameter   :: tipper_magnitude_and_phase = 9
  integer, parameter   :: impedance_strike_skew_ellipticity = 10
  integer, parameter   :: tipper_strike_skew_ellipticity = 11
  integer, parameter   :: induction_arrows = 12
  integer, parameter   :: impedance_determinant = 13
  integer, parameter   :: effective_impedance = 14

  ! other data representations
  integer, parameter   :: inversion_1d = 13
  integer, parameter   :: spectra = 14

  ! statistical estimates
  integer, parameter   :: variance = 1
  integer, parameter   :: covariance = 2
  integer, parameter   :: inverse_signal_covariance = 3
  integer, parameter   :: residual_covariance = 4
  integer, parameter   :: coherence = 5
  integer, parameter   :: multiple_coherence = 6
  integer, parameter   :: signal_amplitude = 7
  integer, parameter   :: signal_noise = 8


Contains


!**************************************************************************
! Initializes and sets up data type dictionary
  subroutine setup_typeDict()

  	 integer     :: istat

     allocate(typeDict(6),STAT=istat)

     typeDict(impedance)%name = 'impedance'
     typeDict(impedance)%isComplex = .true.
     typeDict(impedance)%tfType    = impedance
     typeDict(impedance)%units     = '[V/m]/[T]'
     typeDict(impedance)%nComp     = 8
     allocate(typeDict(impedance)%id(4),STAT=istat)
     typeDict(impedance)%id(1)    = 'ZXX'
     typeDict(impedance)%id(2)    = 'ZXY'
     typeDict(impedance)%id(3)    = 'ZYX'
     typeDict(impedance)%id(4)    = 'ZYY'

     typeDict(off_diagonal_impedance)%name = 'off_diagonal_impedance'
     typeDict(off_diagonal_impedance)%isComplex = .true.
     typeDict(off_diagonal_impedance)%tfType    = off_diagonal_impedance
     typeDict(off_diagonal_impedance)%units     = '[V/m]/[T]'
     typeDict(off_diagonal_impedance)%nComp     = 4
     allocate(typeDict(off_diagonal_impedance)%id(2),STAT=istat)
     typeDict(off_diagonal_impedance)%id(1)    = 'ZXY'
     typeDict(off_diagonal_impedance)%id(2)    = 'ZYX'

     typeDict(tipper)%name = 'tipper'
     typeDict(tipper)%isComplex = .true.
     typeDict(tipper)%tfType    = tipper
     typeDict(tipper)%units     = '[]'
     typeDict(tipper)%nComp     = 4
     allocate(typeDict(tipper)%id(2),STAT=istat)
     typeDict(tipper)%id(1)    = 'TX '
     typeDict(tipper)%id(2)    = 'TY '

     typeDict(interstation_transfer_functions)%name = 'interstation_transfer_functions'
     typeDict(interstation_transfer_functions)%isComplex = .true.
     typeDict(interstation_transfer_functions)%tfType    = interstation_transfer_functions
     typeDict(interstation_transfer_functions)%units     = '[]'
     typeDict(interstation_transfer_functions)%nComp     = 8
     allocate(typeDict(interstation_transfer_functions)%id(4),STAT=istat)
     typeDict(interstation_transfer_functions)%id(1)    = 'MXX'
     typeDict(interstation_transfer_functions)%id(2)    = 'MXY'
     typeDict(interstation_transfer_functions)%id(3)    = 'MYX'
     typeDict(interstation_transfer_functions)%id(4)    = 'MYY'

 	 typeDict(off_diagonal_resistivity_and_phase)%name = 'off_diagonal_resistivity_and_phase'
     typeDict(off_diagonal_resistivity_and_phase)%isComplex = .false.
     typeDict(off_diagonal_resistivity_and_phase)%tfType    = off_diagonal_resistivity_and_phase
     typeDict(off_diagonal_resistivity_and_phase)%units     = '[]'
     typeDict(off_diagonal_resistivity_and_phase)%nComp     = 4
     allocate(typeDict(off_diagonal_resistivity_and_phase)%id(4),STAT=istat)
     typeDict(off_diagonal_resistivity_and_phase)%id(1) = 'RHOXY'
     typeDict(off_diagonal_resistivity_and_phase)%id(2) = 'PHSXY'
     typeDict(off_diagonal_resistivity_and_phase)%id(3) = 'RHOYX'
     typeDict(off_diagonal_resistivity_and_phase)%id(4) = 'PHSYX'
	 	 
	 typeDict(phase_tensor)%name = 'phase_tensor'
     typeDict(phase_tensor)%isComplex = .false.
     typeDict(phase_tensor)%tfType    = phase_tensor
     typeDict(phase_tensor)%units     = '[]'
     typeDict(phase_tensor)%nComp     = 4
     allocate(typeDict(phase_tensor)%id(4),STAT=istat)
     typeDict(phase_tensor)%id(1) = 'PTXX'
     typeDict(phase_tensor)%id(2) = 'PTXY'
     typeDict(phase_tensor)%id(3) = 'PTYX'
     typeDict(phase_tensor)%id(4) = 'PTYY'
	 

  end subroutine setup_typeDict

! **************************************************************************
! Cleans up and deletes type dictionary at end of program execution
  subroutine deall_typeDict()

	integer     :: j, istat

	if (associated(typeDict)) then

	   do j = 1,size(typeDict)
	      if (associated(typeDict(j)%id)) then
	         deallocate(typeDict(j)%id,STAT=istat)
	      end if
          if (associated(typeDict(j)%tx_index)) then
             deallocate(typeDict(j)%tx_index,STAT=istat)
          end if
          if (associated(typeDict(j)%dt_index)) then
             deallocate(typeDict(j)%dt_index,STAT=istat)
          end if
          if (associated(typeDict(j)%rx_index)) then
             deallocate(typeDict(j)%rx_index,STAT=istat)
          end if
	   end do

       deallocate(typeDict,STAT=istat)

    end if

  end subroutine deall_typeDict

!**********************************************************************
! Computes the value by which the data must be multiplied to convert
! from the old units to the new units.
! The units may be any of the following.
! 1) SI units for E/B: [V/m]/[T] (used in ModEM code)
! 2) practical units for E/B: [mV/km]/[nT]
! 3) SI units for E/H: [V/m]/[A/m] = Ohm

  function ImpUnits(oldUnits,newUnits) result (SI_factor)
	character(*), intent(in)    :: oldUnits, newUnits
	real(kind=prec)             :: SI_factor
	! local
	real(kind=prec)             :: factor1, factor2

	! if the quantity is dimensionless, do nothing
	if ((index(oldUnits,'[]')>0) .or. (index(newUnits,'[]')>0)) then
	   SI_factor = ONE
	   return
	end if

		! first convert the old units to [V/m]/[T]
		if (index(oldUnits,'[V/m]/[T]')>0) then
		   ! SI units for E/B
		   factor1 = ONE
		else if (index(oldUnits,'[mV/km]/[nT]')>0) then
		   ! practical units for E/B
		   factor1 = ONE * 1000.0
		else if ((index(oldUnits,'[V/m]/[A/m]')>0) .or. (index(oldUnits,'Ohm')>0)) then
		   ! SI units for E/H
		   factor1 = ONE * 1000.0 * 10000.0/(4*PI) ! approx. 796000.0
		else
		   call errStop('Unknown input units in ImpUnits: '//trim(oldUnits))
		end if

		! now convert [V/m]/[T] to the new units
		if (index(newUnits,'[V/m]/[T]')>0) then
		   ! SI units for E/B
		   factor2 = ONE
		else if (index(newUnits,'[mV/km]/[nT]')>0) then
		   ! practical units for E/B
		   factor2 = ONE / (1000.0)
		else if ((index(newUnits,'[V/m]/[A/m]')>0) .or. (index(newUnits,'Ohm')>0)) then
		   ! SI units for E/H
		   factor2 = ONE / (1000.0 * 10000.0/(4*PI))
		else
		   call errStop('Unknown output units in ImpUnits: '//trim(newUnits))
		end if

		SI_factor = factor1 * factor2


  end function ImpUnits

!**********************************************************************
! Figures out the data type from its name

  function ImpType(typeName) result (dataType)

    character(*), intent(in)    :: typeName
	integer	             	 	:: dataType

    select case (trim(typeName))

       case('impedance')
          dataType = impedance

       case('off_diagonal_impedance')
          dataType = off_diagonal_impedance

       case('tipper')
          dataType = tipper

       case('interstation_transfer_functions')
          dataType = interstation_transfer_functions

       case('off_diagonal_resistivity_and_phase')
          dataType = off_diagonal_resistivity_and_phase
		  		  
      case('phase_tensor')
          dataType = phase_tensor
		  
       case default
          call errStop('Unknown data type:'//trim(typeName))

    end select

  end function ImpType

!**********************************************************************
! Sorts out the data type header

  function ImpHeader(dataType) result (header)

    integer, intent(in)         :: dataType
    character(200)              :: header

    select case (dataType)

       case(impedance,off_diagonal_impedance,tipper)
          header = '# Period(s) Code GG_Lat GG_Lon X(m) Y(m) Z(m) Component Real Imag Error'

       case(interstation_transfer_functions)
          header = '# Period(s) Code GG_Lat GG_Lon X(m) Y(m) Z(m) Ref_Code Ref_Lat Ref_Lon Ref_X(m) Ref_Y(m) Ref_Z(m) Component Real Imag Error'

       case(off_diagonal_resistivity_and_phase,phase_tensor)
          header = '# Period(s) Code GG_Lat GG_Lon X(m) Y(m) Z(m) Component Value Error'

    end select

  end function ImpHeader

!**********************************************************************
! Figures out the component index from its name for any data type

  function ImpComp(compid,dataType) result (icomp)

    character(*), intent(in)    :: compid
    integer,      intent(in)    :: dataType
    integer                     :: icomp
    ! local
    integer                     :: i, ncomp

    ncomp = typeDict(dataType)%ncomp
    if (typeDict(dataType)%isComplex) then
        ncomp = ncomp/2
    end if
    icomp = 0

    do i = 1,ncomp
        if (index(typeDict(dataType)%id(i),trim(compid))>0) then
            icomp = i
            exit
        end if
    end do

    if (icomp == 0) then
        call errStop('Problem locating the component '//trim(compid)//' in data type '//trim(typeDict(dataType)%name))
    end if

  end function ImpComp

!*************************************************************************
! Supports the old data format file: figures out the data type from header

  function ImpTypeFromHeader(nComp,header) result (dataType)

    integer, intent(in)         :: nComp
    character(*), intent(in)    :: header
    integer                     :: dataType
    ! local
    character(15), allocatable  :: compids(:)
    integer                     :: j,istat

    allocate(compids(nComp),STAT=istat)

    read(header,*) compids

    select case (nComp)
       case(8)
          if (index(compids(1),'Mxx')>0) then
             dataType =  interstation_transfer_functions
          else
             dataType =  impedance
          end if
       case(4)
          if (index(compids(1),'Tx')>0) then
             dataType =  tipper
          elseif (index(compids(1),'Zxy')>0) then
             dataType =  off_diagonal_impedance
          elseif (index(compids(1),'Rhoxy')>0) then
             dataType =  off_diagonal_resistivity_and_phase
          end if
    end select

    do j = 1,nComp
        if (compids(j) .ne. typeDict(dataType)%id(j)) then
            call errStop('Wrong order of impedance components in data header')
        end if
    end do

    deallocate(compids,STAT=istat)

  end function ImpTypeFromHeader


!*************************************************************************
! Supports the old data format file: checks the order of components

 subroutine check_header_order(nComp,dataType,header)
 	 integer, intent(in)   	 	 :: nComp
 	 integer, intent(in)   	 	 :: dataType
     character(*), intent(in)    :: header
 	 !Local
 	 character(15), allocatable  :: compids(:)
     integer                     :: j,istat

     allocate(compids(nComp),STAT=istat)
     read(header,*) compids


      do j = 1,nComp
    	if (compids(j) .ne. typeDict(dataType)%id(j)) then
    		call errStop('Wrong order of impedance components in data header')
    	end if
    end do

    deallocate(compids,STAT=istat)

 end subroutine check_header_order


end module dataTypes
