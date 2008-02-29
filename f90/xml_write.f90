module xml_write

  use FoX_wxml
  use global
  use utils

  implicit none
  private

  type(xmlf_t)                 :: xmlfile
  character(len=19)            :: xml_time
  integer                      :: i, j

  save   :: xmlfile

  public :: initialize_xml_output
  public :: add_USArray_MT_header
  public :: add_ProcessingInfo
  public :: end_xml_output
  public :: new_element, end_element
  public :: add_Location, add_Channel
  public :: add_TimePeriod, add_Info
  public :: initialize_xml_freq_block_output
  public :: end_xml_freq_block_output
  public :: add_FrequencyRange, new_Frequency
  public :: add_TF, add_TFVar
  public :: add_InvSigCov, add_ResidCov

contains

  subroutine initialize_xml_output(fname,root)
    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: root
    character(len=100)           :: schemaLocation

	schemaLocation = "http://www.earthscope.org/mt http://www.iris.edu/schema/mt/MT_TF_"//version//".xsd"

    call xml_OpenFile(fname, xmlfile)
    
    call xml_DeclareNamespace(xmlfile, 'http://www.w3.org/2001/XMLSchema-instance','xsi')
    call xml_DeclareNamespace(xmlfile, 'http://www.earthscope.org/mt')
    
    call xml_NewElement(xmlfile, root)
    call xml_AddAttribute(xmlfile, "xsi:schemaLocation",trim(schemaLocation))
        
  end subroutine initialize_xml_output


  subroutine new_element(ElementName)
    character(len=*), intent(in) :: ElementName    

    call xml_NewElement(xmlfile, ElementName)

  end subroutine new_element

  
  subroutine end_element(ElementName)
    character(len=*), intent(in) :: ElementName

    call xml_EndElement(xmlfile, ElementName)

  end subroutine end_element

  
  subroutine initialize_xml_freq_block_output(nf)
    integer, intent(in)          :: nf

    call xml_NewElement(xmlfile, 'Frequencies')
    call xml_AddAttribute(xmlfile, 'count', nf)

  end subroutine initialize_xml_freq_block_output



  subroutine add_USArray_MT_header(zsitename, Site, UserInfo, Info, Notes)
    character(len=*), intent(in) :: zsitename
    type(Site_t), intent(in)     :: Site
    type(UserInfo_t), intent(in) :: UserInfo
    type(RemoteRef_t), intent(in):: Info
    character(len=*), dimension(:), pointer, optional :: Notes

    call xml_NewElement(xmlfile, 'ProductID')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Source)//'.'//Site%ID)
    if (len_trim(UserInfo%ID)>0) call xml_AddCharacters(xmlfile, '.'//trim(UserInfo%ID))
    call xml_EndElement(xmlfile, 'ProductID')
    
    call xml_NewElement(xmlfile, 'Source')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Source))
    call xml_EndElement(xmlfile, 'Source')

    call xml_NewElement(xmlfile, 'YearCollected')
    call xml_AddCharacters(xmlfile, UserInfo%Year)
    call xml_EndElement(xmlfile, 'YearCollected')

    call xml_NewElement(xmlfile, 'USArrayNet')
    call xml_AddCharacters(xmlfile, '_US-MT')
    call xml_EndElement(xmlfile, 'USArrayNet')

    call xml_NewElement(xmlfile, 'Network')
    call xml_AddCharacters(xmlfile, trim(network))
    call xml_EndElement(xmlfile, 'Network')
    
    call xml_NewElement(xmlfile, 'SiteName')
    call xml_AddCharacters(xmlfile, trim(Site%Description))
    call xml_EndElement(xmlfile, 'SiteName')
    
	call date_and_time(date, time, zone)

    xml_time = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//&
         'T'//time(1:2)//':'//time(3:4)//':'//time(5:6)

    call xml_NewElement(xmlfile, 'CreateTime')
    call xml_AddCharacters(xmlfile, xml_time)
    call xml_EndElement(xmlfile, 'CreateTime')
 
	call add_Location(Site%Location,Site%Declination)
 
    call xml_NewElement(xmlfile, 'SiteID')
    call xml_AddAttribute(xmlfile, 'network', trim(UserInfo%Source))
    call xml_AddCharacters(xmlfile, trim(Site%ID))
    call xml_EndElement(xmlfile, 'SiteID')
 
    call xml_NewElement(xmlfile, 'SiteID')
    call xml_AddAttribute(xmlfile, 'network', 'EMTF')
    call xml_AddCharacters(xmlfile, trim(zsitename))
    call xml_EndElement(xmlfile, 'SiteID')
    
    call xml_NewElement(xmlfile, 'RunList')
    call xml_AddCharacters(xmlfile, trim(Site%RunList))
    call xml_EndElement(xmlfile, 'RunList')
    
    if (present(Notes)) then
    	if (associated(Notes)) then
    		    call xml_NewElement(xmlfile, 'Notes')
    		    call xml_AddCharacters(xmlfile, trim(Notes(1)))
    		    do i=2,size(Notes)
    				call xml_AddCharacters(xmlfile, achar(ascii_cr))
    				call xml_AddCharacters(xmlfile, trim(Notes(i)))
    			end do
    			call xml_EndElement(xmlfile, 'Notes')	
    	end if
    end if
    
  end subroutine add_USArray_MT_header

	
	subroutine add_ProcessingInfo(UserInfo, Info, RemoteSite, RemoteRun)
		type(UserInfo_t), intent(in)                    :: UserInfo
 		type(RemoteRef_t), intent(in)                   :: Info
    	type(Site_t), optional, intent(in)              :: RemoteSite
    	type(Run_t), optional, dimension(:), intent(in) :: RemoteRun 

		call xml_NewElement(xmlfile, 'ProcessingInfo')
		call xml_NewElement(xmlfile, 'RemoteRef')
		call xml_AddAttribute(xmlfile, 'type', trim(Info%remote_ref_type))
		call xml_EndElement(xmlfile, 'RemoteRef')
		
		if (present(RemoteSite)) then
			if (len_trim(RemoteSite%ID)>0) then
				call xml_NewElement(xmlfile, 'RemoteSite')
			
				call xml_NewElement(xmlfile, 'SiteName')
				call xml_AddCharacters(xmlfile, trim(RemoteSite%Description))
				call xml_EndElement(xmlfile, 'SiteName')

				call xml_NewElement(xmlfile, 'SiteID')
				call xml_AddAttribute(xmlfile, 'network', trim(UserInfo%Source))
 				call xml_AddCharacters(xmlfile, trim(RemoteSite%ID))
				call xml_EndElement(xmlfile, 'SiteID')
			
				call add_Location(RemoteSite%Location)
				
				if (present(RemoteRun)) then
  					do i=1,size(RemoteRun)
						call add_TimePeriod(RemoteRun(i))
  					end do
  				end if	
			
				call xml_EndElement(xmlfile, 'RemoteSite')
			end if
		end if	
		
		call xml_NewElement(xmlfile, 'ProcessedBy')
		call xml_AddCharacters(xmlfile, trim(UserInfo%ProcessedBy))
		call xml_EndElement(xmlfile, 'ProcessedBy')

		call xml_NewElement(xmlfile, 'ProcessingSoftware')
		if (len_trim(Info%software_version)>0) then
			call xml_AddAttribute(xmlfile, 'version', trim(Info%software_version))
		end if
		call xml_AddCharacters(xmlfile, trim(Info%software))
		call xml_EndElement(xmlfile, 'ProcessingSoftware')
		call xml_EndElement(xmlfile, 'ProcessingInfo')
    	
	end subroutine add_ProcessingInfo
	
		
	subroutine add_Info(Run)
 		type(Run_t), intent(in)      :: Run

		call xml_NewElement(xmlfile, 'Info')
		call xml_AddAttribute(xmlfile, 'run', trim(Run%ID))
		
		call xml_NewElement(xmlfile, 'Instrument')
		call xml_AddCharacters(xmlfile, trim(Run%Instrument))
		call xml_EndElement(xmlfile, 'Instrument')

		if (Run%SamplingFreq > 0.0d0) then
    		call xml_NewElement(xmlfile, 'SamplingFreq')
    		call xml_AddCharacters(xmlfile, Run%SamplingFreq, fmt="r3")
    		call xml_EndElement(xmlfile, 'SamplingFreq')
    	end if

		call xml_NewElement(xmlfile, 'Ex_wire_length')
		call xml_AddCharacters(xmlfile, Run%Ex_wire_length, fmt="r3")
		call xml_EndElement(xmlfile, 'Ex_wire_length')

		call xml_NewElement(xmlfile, 'Ey_wire_length')
		call xml_AddCharacters(xmlfile, Run%Ey_wire_length, fmt="r3")
		call xml_EndElement(xmlfile, 'Ey_wire_length')

		call xml_NewElement(xmlfile, 'Comments')
		call xml_AddCharacters(xmlfile, trim(Run%Comments))
		call xml_EndElement(xmlfile, 'Comments')
		
		call xml_EndElement(xmlfile, 'Info')   	
    	
	end subroutine add_Info

	subroutine add_TimePeriod(Run)
 		type(Run_t), intent(in)      :: Run

		call xml_NewElement(xmlfile, 'TimePeriod')
		call xml_AddAttribute(xmlfile, 'run', trim(Run%ID))
		
		call xml_NewElement(xmlfile, 'Start')
		call xml_AddCharacters(xmlfile, Run%TimePeriod%StartTime)
		call xml_EndElement(xmlfile, 'Start')

		call xml_NewElement(xmlfile, 'End')
		call xml_AddCharacters(xmlfile, Run%TimePeriod%EndTime)
		call xml_EndElement(xmlfile, 'End')

		call xml_EndElement(xmlfile, 'TimePeriod')   	
    	
	end subroutine add_TimePeriod

  subroutine add_Location(L,decl)
    type(Location_t), intent(in)    :: L
	real(8), optional, intent(in)   :: decl

    call xml_NewElement(xmlfile, 'Location')
    
    call xml_NewElement(xmlfile, 'Latitude')
    call xml_AddCharacters(xmlfile, L%lat, fmt="r3")
    call xml_EndElement(xmlfile, 'Latitude')
    
    call xml_NewElement(xmlfile, 'Longitude')
    call xml_AddCharacters(xmlfile, L%lon, fmt="r3")
    call xml_EndElement(xmlfile, 'Longitude')
    
    call xml_NewElement(xmlfile, 'Elevation')
    call xml_AddCharacters(xmlfile, L%elev,  fmt="r3")
    call xml_EndElement(xmlfile, 'Elevation')

    call xml_EndElement(xmlfile, 'Location')
    
    if (present(decl)) then
    	call xml_NewElement(xmlfile, 'Declination')
    	call xml_AddCharacters(xmlfile, decl,  fmt="r3")
    	call xml_EndElement(xmlfile, 'Declination')
    end if
    	

  end subroutine add_Location


  subroutine add_Channel(C,location)
    type(Channel_t), intent(in)     :: C
    logical, intent(in)           :: location

    call xml_NewElement(xmlfile, 'Channel')
    call xml_AddAttribute(xmlfile, 'name', trim(C%ID))
    call xml_AddAttribute(xmlfile, 'orientation', C%Orientation, fmt="r1")
    if (location) then
       call add_Location(C%Location)
    end if
    call xml_EndElement(xmlfile, 'Channel')

  end subroutine add_Channel


  subroutine add_FrequencyRange(F)
    type(FreqInfo_t), dimension(:), intent(in)  :: F
    real(8), dimension(:), allocatable          :: period
    real(8)                                     :: minFreq, minPeriod
    real(8)                                     :: maxFreq, maxPeriod
	integer                                     :: i

	allocate(period(nf))

	do i=1,nf
		if (index(F(i)%info_type,'period')>0) then
			period(i) = F(i)%value
		else
			period(i) = 1.0d0/F(i)%value
		end if
	end do
	
	maxPeriod = maxval(period)
	minPeriod = minval(period)
	
	deallocate(period)
	
	maxFreq = 1.0d0/minPeriod
	minFreq = 1.0d0/maxPeriod
	
	if (.not.(silent)) then
		print *,'maxPeriod = ',maxPeriod
		print *,'minPeriod = ',minPeriod
	end if

    call xml_NewElement(xmlfile, 'PeriodRange')
    call xml_AddAttribute(xmlfile, 'min', minPeriod, fmt="r5")
    call xml_AddAttribute(xmlfile, 'max', maxPeriod, fmt="r5")
    call xml_EndElement(xmlfile, 'PeriodRange')

    call xml_NewElement(xmlfile, 'FrequencyRange')
    call xml_AddAttribute(xmlfile, 'min', minFreq, fmt="s5")
    call xml_AddAttribute(xmlfile, 'max', maxFreq, fmt="s5")
    call xml_EndElement(xmlfile, 'FrequencyRange')

  end subroutine add_FrequencyRange


  subroutine new_Frequency(F)
    type(FreqInfo_t), intent(in)    :: F

    call xml_NewElement(xmlfile, 'Frequency')
    call xml_AddAttribute(xmlfile, 'type', trim(F%info_type))
    call xml_AddAttribute(xmlfile, 'value', F%value, fmt="r5")
    call xml_NewElement(xmlfile, 'NumData')
    call xml_AddCharacters(xmlfile, F%num_points)
    call xml_EndElement(xmlfile, 'NumData')

  end subroutine new_Frequency


  subroutine add_TF(TF, Input, Output)
    complex(8), dimension(:,:), intent(in)    :: TF
    type(Channel_t), dimension(:), intent(in) :: Input, Output

    call xml_NewElement(xmlfile, 'TF')
    do i=1,nch-2
       do j=1,2
          call xml_NewElement(xmlfile, 'component')
          call xml_AddAttribute(xmlfile, 'input', trim(Input(j)%ID))
          call xml_AddAttribute(xmlfile, 'output', trim(Output(i)%ID))
          call xml_NewElement(xmlfile, 'real')
          call xml_AddCharacters(xmlfile, dreal(TF(i,j)), fmt="s5")
          call xml_EndElement(xmlfile, 'real')
          call xml_NewElement(xmlfile, 'imag')
          call xml_AddCharacters(xmlfile, dimag(TF(i,j)), fmt="s5")
          call xml_EndElement(xmlfile, 'imag')
          call xml_EndElement(xmlfile, 'component')
       end do
    end do
    call xml_EndElement(xmlfile, 'TF')

  end subroutine add_TF


  subroutine add_TFVar(TFVar, Input, Output)
    real(8), dimension(:,:), intent(in)       :: TFVar
    type(Channel_t), dimension(:), intent(in) :: Input, Output

    call xml_NewElement(xmlfile, 'TFVar')
    do i=1,nch-2
       do j=1,2
          call xml_NewElement(xmlfile, 'value')
          call xml_AddAttribute(xmlfile, 'input', trim(Input(j)%ID))
          call xml_AddAttribute(xmlfile, 'output', trim(Output(i)%ID))
          call xml_AddCharacters(xmlfile, TFVar(i,j), fmt="s5")
          call xml_EndElement(xmlfile, 'value')
       end do
    end do
    call xml_EndElement(xmlfile, 'TFVar')

  end subroutine add_TFVar


  subroutine add_InvSigCov(InvSigCov, Input)
    complex(8), dimension(:,:), intent(in)    :: InvSigCov
    type(Channel_t), dimension(:), intent(in) :: Input

    call xml_NewElement(xmlfile, 'InvSigCov')
    call xml_NewElement(xmlfile, 'comment')
    call xml_AddCharacters(xmlfile, 'Inverse Coherent Signal Power Matrix')
    call xml_EndElement(xmlfile, 'comment')
    call xml_NewElement(xmlfile, 'size')
    call xml_AddCharacters(xmlfile, 2)
    call xml_EndElement(xmlfile, 'size')

    do i=1,2
       do j=1,i
          call xml_NewElement(xmlfile, 'value')
          call xml_AddAttribute(xmlfile, 'row', trim(Input(i)%ID))
          call xml_AddAttribute(xmlfile, 'col', trim(Input(j)%ID))
          call xml_NewElement(xmlfile, 'real')
          call xml_AddCharacters(xmlfile, dreal(InvSigCov(i,j)), fmt="s5")
          call xml_EndElement(xmlfile, 'real')
          call xml_NewElement(xmlfile, 'imag')
          call xml_AddCharacters(xmlfile, dimag(InvSigCov(i,j)), fmt="s5")
          call xml_EndElement(xmlfile, 'imag')
          call xml_EndElement(xmlfile, 'value')
       end do
    end do
    call xml_EndElement(xmlfile, 'InvSigCov')

  end subroutine add_InvSigCov


  subroutine add_ResidCov(ResidCov, Output)
    complex(8), dimension(:,:), intent(in)    :: ResidCov
    type(Channel_t), dimension(:), intent(in) :: Output

    call xml_NewElement(xmlfile, 'ResidCov')
    call xml_NewElement(xmlfile, 'comment')
    call xml_AddCharacters(xmlfile, 'Residual Covariance')
    call xml_EndElement(xmlfile, 'comment')
    call xml_NewElement(xmlfile, 'size')
    call xml_AddCharacters(xmlfile, nch-2)
    call xml_EndElement(xmlfile, 'size')

    do i=1,nch-2
       do j=1,i
          call xml_NewElement(xmlfile, 'value')
          call xml_AddAttribute(xmlfile, 'row', trim(Output(i)%ID))
          call xml_AddAttribute(xmlfile, 'col', trim(Output(j)%ID))
          call xml_NewElement(xmlfile, 'real')
          call xml_AddCharacters(xmlfile, dreal(ResidCov(i,j)), fmt="s5")
          call xml_EndElement(xmlfile, 'real')
          call xml_NewElement(xmlfile, 'imag')
          call xml_AddCharacters(xmlfile, dimag(ResidCov(i,j)), fmt="s5")
          call xml_EndElement(xmlfile, 'imag')
          call xml_EndElement(xmlfile, 'value')
       end do
    end do
    call xml_EndElement(xmlfile, 'ResidCov')

  end subroutine add_ResidCov


  subroutine end_xml_freq_block_output

     call xml_EndElement(xmlfile, 'Frequencies')

  end subroutine end_xml_freq_block_output


  subroutine end_xml_output(product)
    character(len=*), intent(in) :: product

     call xml_EndElement(xmlfile, product)

     call xml_Close(xmlfile)
  end subroutine end_xml_output

end module xml_write
