module xml_write

  use FoX_wxml
  use global
  use utils

  implicit none
  private

  type(xmlf_t)                 :: xmlfile
  character(len=19)            :: xml_time
  integer                      :: i, j, N

  save   :: xmlfile

  public :: initialize_xml_output
  public :: add_xml_header
  public :: add_ProcessingInfo
  public :: end_xml_output
  public :: new_element, end_element
  public :: new_channel_block, new_data_block, end_block
  public :: add_Location, add_Channel
  public :: add_DataType, add_Estimate
  public :: add_Data, add_Var
  public :: add_InvSigCov, add_ResidCov
  public :: add_FieldNotes
  public :: add_GridOrigin
  public :: initialize_xml_freq_block_output
  public :: end_xml_freq_block_output
  public :: add_PeriodRange

contains

  subroutine initialize_xml_output(fname,root)
    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: root
    character(len=100)           :: schemaLocation

	!schemaLocation = "http://www.earthscope.org/mt http://www.iris.edu/schema/mt/MT_TF_"//version//".xsd"

    call xml_OpenFile(fname, xmlfile)
    
    !call xml_DeclareNamespace(xmlfile, 'http://www.w3.org/2001/XMLSchema-instance','xsi')
    !call xml_DeclareNamespace(xmlfile, 'http://www.earthscope.org/mt')
    
    call xml_NewElement(xmlfile, root)
    !call xml_AddAttribute(xmlfile, "xsi:schemaLocation",trim(schemaLocation))
        
  end subroutine initialize_xml_output


  subroutine new_element(ElementName)
    character(len=*), intent(in) :: ElementName    

    call xml_NewElement(xmlfile, ElementName)

  end subroutine new_element

  
  subroutine end_element(ElementName)
    character(len=*), intent(in) :: ElementName

    call xml_EndElement(xmlfile, ElementName)

  end subroutine end_element


  subroutine new_channel_block(ElementName)
    character(len=*), intent(in) :: ElementName

	call xml_NewElement(xmlfile, ElementName)
    call xml_AddAttribute(xmlfile, 'ref', 'site')
    call xml_AddAttribute(xmlfile, 'units', 'm')

  end subroutine new_channel_block


  subroutine new_data_block(ElementName,F)
    character(len=*), intent(in) :: ElementName 
    type(FreqInfo_t), intent(in) :: F   

	call xml_NewElement(xmlfile, ElementName)
    call xml_AddAttribute(xmlfile, 'value', F%value, fmt="r5")
    call xml_AddAttribute(xmlfile, 'units', trim(F%units))

  end subroutine new_data_block


   subroutine end_block(ElementName)
    character(len=*), intent(in) :: ElementName

    call xml_EndElement(xmlfile, ElementName)

  end subroutine end_block

 
  subroutine initialize_xml_freq_block_output(nf)
    integer, intent(in)          :: nf

    call xml_NewElement(xmlfile, 'Data')
    call xml_AddAttribute(xmlfile, 'count', nf)

  end subroutine initialize_xml_freq_block_output



  subroutine add_xml_header(Site, UserInfo, Notes, NotesLength)
    type(Site_t), intent(in)     :: Site
    type(UserInfo_t), intent(in) :: UserInfo
    character(len=*), dimension(:), pointer, optional :: Notes
    integer, optional            :: NotesLength

    call xml_NewElement(xmlfile, 'Description')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Description))
    call xml_EndElement(xmlfile, 'Description')
    
    call xml_NewElement(xmlfile, 'ProductId')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Project)//'.'//Site%ID)
    if (len_trim(UserInfo%YearCollected)>0) call xml_AddCharacters(xmlfile, '.'//trim(UserInfo%YearCollected))
    call xml_EndElement(xmlfile, 'ProductId')

    call xml_NewElement(xmlfile, 'SubType')
    call xml_AddCharacters(xmlfile, trim(UserInfo%SubType))
    call xml_EndElement(xmlfile, 'SubType')

    call xml_NewElement(xmlfile, 'Notes')
    ! empty element for now
    call xml_EndElement(xmlfile, 'Notes')
    
    call xml_NewElement(xmlfile, 'Tags')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Tags))
    call xml_EndElement(xmlfile, 'Tags')

    if (UserInfo%TimeSeriesArchived) then
        call xml_NewElement(xmlfile, 'ExternalUrl')
        call xml_NewElement(xmlfile, 'Description')
        call xml_AddCharacters(xmlfile, 'IRIS DMC MetaData')
        call xml_EndElement(xmlfile, 'Description')
        call xml_NewElement(xmlfile, 'Url')
        call xml_AddCharacters(xmlfile, 'http://www.iris.edu/mda/'//UserInfo%Network//'/'//Site%ID)
        call xml_EndElement(xmlfile, 'Url')
        call xml_EndElement(xmlfile, 'ExternalUrl')
    end if

	call date_and_time(date, time, zone)

    xml_time = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//&
         'T'//time(1:2)//':'//time(3:4)//':'//time(5:6)

    call xml_NewElement(xmlfile, 'Provenance')

    call xml_NewElement(xmlfile, 'CreateTime')
    call xml_AddCharacters(xmlfile, xml_time)
    call xml_EndElement(xmlfile, 'CreateTime')
    call xml_NewElement(xmlfile, 'CreatingApplication')
    call xml_AddCharacters(xmlfile, 'EMTF File Conversion Utilities '//trim(version))
    call xml_EndElement(xmlfile, 'CreatingApplication')

    call xml_NewElement(xmlfile, 'Creator')
    call xml_NewElement(xmlfile, 'Name')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Creator%Name))
    call xml_EndElement(xmlfile, 'Name')
    call xml_NewElement(xmlfile, 'Email')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Creator%Email))
    call xml_EndElement(xmlfile, 'Email')
    call xml_NewElement(xmlfile, 'Org')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Creator%Org))
    call xml_EndElement(xmlfile, 'Org')
    call xml_NewElement(xmlfile, 'OrgUrl')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Creator%OrgUrl))
    call xml_EndElement(xmlfile, 'OrgUrl')
    call xml_EndElement(xmlfile, 'Creator')

    call xml_NewElement(xmlfile, 'Submitter')
    call xml_NewElement(xmlfile, 'Name')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Submitter%Name))
    call xml_EndElement(xmlfile, 'Name')
    call xml_NewElement(xmlfile, 'Email')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Submitter%Email))
    call xml_EndElement(xmlfile, 'Email')
    call xml_NewElement(xmlfile, 'Org')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Submitter%Org))
    call xml_EndElement(xmlfile, 'Org')
    call xml_NewElement(xmlfile, 'OrgUrl')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Submitter%OrgUrl))
    call xml_EndElement(xmlfile, 'OrgUrl')
    call xml_EndElement(xmlfile, 'Submitter')

    call xml_EndElement(xmlfile, 'Provenance')
    
    call xml_NewElement(xmlfile, 'Copyright')

    call xml_NewElement(xmlfile, 'Citation')
    call xml_NewElement(xmlfile, 'Title')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Copyright%Title))
    call xml_EndElement(xmlfile, 'Title')
    call xml_NewElement(xmlfile, 'Authors')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Copyright%Authors))
    call xml_EndElement(xmlfile, 'Authors')
    call xml_NewElement(xmlfile, 'Year')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Copyright%Year))
    call xml_EndElement(xmlfile, 'Year')
    call xml_NewElement(xmlfile, 'DOI')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Copyright%DOI))
    call xml_EndElement(xmlfile, 'DOI')
    call xml_EndElement(xmlfile, 'Citation')
    call xml_NewElement(xmlfile, 'ReleaseStatus')
    call xml_AddCharacters(xmlfile, trim(UserInfo%Copyright%ReleaseStatus))
    call xml_EndElement(xmlfile, 'ReleaseStatus')
    if (.not. isempty(UserInfo%Copyright%ConditionsOfUse(1))) then
        call xml_NewElement(xmlfile, 'ConditionsOfUse')
        call xml_AddCharacters(xmlfile, trim(UserInfo%Copyright%ConditionsOfUse(1)))
        do i=2,size(UserInfo%Copyright%ConditionsOfUse)
            if (isempty(UserInfo%Copyright%ConditionsOfUse(i))) then
                exit
            end if
            call xml_AddCharacters(xmlfile, achar(ascii_cr))
            call xml_AddCharacters(xmlfile, trim(UserInfo%Copyright%ConditionsOfUse(i)))
        end do
        call xml_EndElement(xmlfile, 'ConditionsOfUse')
    end if

    call xml_EndElement(xmlfile, 'Copyright')

    call xml_NewElement(xmlfile, 'Site')

	call add_Site_header(UserInfo, Site)
 
	call add_Location(Site%Location,Site%Declination)
 
    call xml_NewElement(xmlfile, 'AcquiredBy')
    call xml_AddCharacters(xmlfile, trim(UserInfo%AcquiredBy))
    call xml_EndElement(xmlfile, 'AcquiredBy')

    call xml_NewElement(xmlfile, 'Start')
    call xml_AddCharacters(xmlfile, trim(Site%Start))
    call xml_EndElement(xmlfile, 'Start')

    call xml_NewElement(xmlfile, 'End')
    call xml_AddCharacters(xmlfile, trim(Site%End))
    call xml_EndElement(xmlfile, 'End')

    call xml_NewElement(xmlfile, 'RunList')
    call xml_AddCharacters(xmlfile, trim(Site%RunList))
    call xml_EndElement(xmlfile, 'RunList')

    if (Site%QualityRating > 0) then
        call xml_NewElement(xmlfile, 'DataQualityNotes')
        call xml_NewElement(xmlfile, 'Rating')
        call xml_AddCharacters(xmlfile, Site%QualityRating)
        call xml_EndElement(xmlfile, 'Rating')
        call xml_NewElement(xmlfile, 'GoodFromPeriod')
        call xml_AddCharacters(xmlfile, Site%GoodFromPeriod, fmt="r3")
        call xml_EndElement(xmlfile, 'GoodFromPeriod')
        call xml_NewElement(xmlfile, 'GoodToPeriod')
        call xml_AddCharacters(xmlfile, Site%GoodToPeriod, fmt="r3")
        call xml_EndElement(xmlfile, 'GoodToPeriod')
        call xml_NewElement(xmlfile, 'Comments')
        call xml_AddAttribute(xmlfile, 'author', trim(UserInfo%Creator%Name))
        call xml_AddCharacters(xmlfile, trim(Site%QualityComments))
        call xml_EndElement(xmlfile, 'Comments')
        call xml_EndElement(xmlfile, 'DataQualityNotes')
    end if

    if (Site%WarningFlag >= 0) then
        call xml_NewElement(xmlfile, 'DataQualityWarnings')
        call xml_NewElement(xmlfile, 'Flag')
        call xml_AddCharacters(xmlfile, Site%WarningFlag)
        call xml_EndElement(xmlfile, 'Flag')
        call xml_NewElement(xmlfile, 'Comments')
        call xml_AddAttribute(xmlfile, 'author', trim(UserInfo%Creator%Name))
        call xml_AddCharacters(xmlfile, trim(Site%WarningComments))
        call xml_EndElement(xmlfile, 'Comments')
        call xml_EndElement(xmlfile, 'DataQualityWarnings')
    end if

    if (present(Notes)) then
    	if (associated(Notes)) then
                if (present(NotesLength)) then
                    N = NotesLength
                else
                    N = size(Notes)
                end if
    		    call xml_NewElement(xmlfile, 'Comments')
				call xml_AddAttribute(xmlfile, 'author', trim(UserInfo%ProcessedBy))
    		    call xml_AddCharacters(xmlfile, trim(Notes(1)))
    		    do i=2,N
    				call xml_AddCharacters(xmlfile, achar(ascii_cr))
    				call xml_AddCharacters(xmlfile, trim(Notes(i)))
    			end do
    			call xml_EndElement(xmlfile, 'Comments')	
    	end if
    end if
    
    call xml_EndElement(xmlfile, 'Site')
    
  end subroutine add_xml_header

  
	subroutine add_Site_header(UserInfo, Site)
		type(UserInfo_t), intent(in)                    :: UserInfo
    	type(Site_t), optional, intent(in)              :: Site

		call xml_NewElement(xmlfile, 'Project')
		call xml_AddCharacters(xmlfile, trim(UserInfo%Project))
		call xml_EndElement(xmlfile, 'Project')
		
		call xml_NewElement(xmlfile, 'Survey')
		call xml_AddCharacters(xmlfile, trim(UserInfo%Survey))
		call xml_EndElement(xmlfile, 'Survey')
		
		call xml_NewElement(xmlfile, 'YearCollected')
		call xml_AddCharacters(xmlfile, trim(UserInfo%YearCollected))
		call xml_EndElement(xmlfile, 'YearCollected')
		
		call xml_NewElement(xmlfile, 'Id')
		call xml_AddCharacters(xmlfile, trim(Site%ID))
		call xml_EndElement(xmlfile, 'Id')
		
		call xml_NewElement(xmlfile, 'Name')
		call xml_AddCharacters(xmlfile, trim(Site%Description))
		call xml_EndElement(xmlfile, 'Name')

	end subroutine add_Site_header
	
	
	subroutine add_ProcessingInfo(UserInfo, RemoteSite, RemoteRun)
		type(UserInfo_t), intent(in)                    :: UserInfo
    	type(Site_t), optional, intent(in)              :: RemoteSite
    	type(Run_t), optional, dimension(:), intent(in) :: RemoteRun 

		call xml_NewElement(xmlfile, 'ProcessingInfo')
		call xml_NewElement(xmlfile, 'SignConvention')
		call xml_AddCharacters(xmlfile, trim(UserInfo%SignConvention))
		call xml_EndElement(xmlfile, 'SignConvention')
		
		call xml_NewElement(xmlfile, 'RemoteRef')
		call xml_AddAttribute(xmlfile, 'type', trim(UserInfo%RemoteRefType))
		call xml_EndElement(xmlfile, 'RemoteRef')
		
		if (present(RemoteSite)) then
			if (len_trim(RemoteSite%ID)>0) then
				call xml_NewElement(xmlfile, 'RemoteInfo')
			
				call xml_NewElement(xmlfile, 'Site')
				call add_Site_header(UserInfo,RemoteSite)				
				call add_Location(RemoteSite%Location)				
				call xml_EndElement(xmlfile, 'Site')
								
				if (present(RemoteRun)) then
  					do i=1,size(RemoteRun)
						call add_FieldNotes(RemoteRun(i))
  					end do
   				end if	
			
				call xml_EndElement(xmlfile, 'RemoteInfo')
			end if
		end if	
		
		call xml_NewElement(xmlfile, 'ProcessedBy')
		call xml_AddCharacters(xmlfile, trim(UserInfo%ProcessedBy))
		call xml_EndElement(xmlfile, 'ProcessedBy')

        if (.not. isempty(UserInfo%ProcessDate)) then
            call xml_NewElement(xmlfile, 'ProcessDate')
            call xml_AddCharacters(xmlfile, trim(UserInfo%ProcessDate))
            call xml_EndElement(xmlfile, 'ProcessDate')
        end if

		call xml_NewElement(xmlfile, 'ProcessingSoftware')
		call xml_NewElement(xmlfile, 'Name')
		call xml_AddCharacters(xmlfile, trim(UserInfo%ProcessingSoftware))
		call xml_EndElement(xmlfile, 'Name')
		call xml_NewElement(xmlfile, 'LastMod')
		call xml_AddCharacters(xmlfile, trim(UserInfo%ProcessingSoftwareLastMod))
		call xml_EndElement(xmlfile, 'LastMod')
		call xml_NewElement(xmlfile, 'Author')
		call xml_AddCharacters(xmlfile, trim(UserInfo%ProcessingSoftwareAuthor))
		call xml_EndElement(xmlfile, 'Author')
		call xml_EndElement(xmlfile, 'ProcessingSoftware')
		
		call xml_NewElement(xmlfile, 'ProcessingTag')
        call xml_AddCharacters(xmlfile, trim(UserInfo%ProcessingTag))
        call xml_EndElement(xmlfile, 'ProcessingTag')
		call xml_EndElement(xmlfile, 'ProcessingInfo')
    	
	end subroutine add_ProcessingInfo
	

	subroutine add_FieldNotes(Run, InputChannel, OutputChannel)
 		type(Run_t), intent(in)      :: Run
 		type(Channel_t), intent(in), optional	 :: InputChannel(:)
 		type(Channel_t), intent(in), optional	 :: OutputChannel(:) ! electric only
 		! local
 		integer						 :: nchoutE
 		character(len=1)			 :: location(2)
 		character(len=10)			 :: number(2)

		call xml_NewElement(xmlfile, 'FieldNotes')
		call xml_AddAttribute(xmlfile, 'run', trim(Run%ID))
		
		call xml_NewElement(xmlfile, 'Instrument')
		call xml_NewElement(xmlfile, 'Manufacturer')
		call xml_AddCharacters(xmlfile, trim(Run%Manufacturer))
		call xml_EndElement(xmlfile, 'Manufacturer')
		call xml_NewElement(xmlfile, 'Name')
		call xml_AddCharacters(xmlfile, trim(Run%InstrumentName))
		call xml_EndElement(xmlfile, 'Name')
		call xml_NewElement(xmlfile, 'Id')
		call xml_AddCharacters(xmlfile, trim(Run%InstrumentID))
		call xml_EndElement(xmlfile, 'Id')
		call xml_NewElement(xmlfile, 'Settings')
		call xml_EndElement(xmlfile, 'Settings')
		call xml_EndElement(xmlfile, 'Instrument')

		if (.not. present(InputChannel)) then
			! skip magnetometer information
		else
			! add full information on magnetometer
			call xml_NewElement(xmlfile, 'Magnetometer')
			call xml_AddAttribute(xmlfile, 'type', trim(InputChannel(1)%InstrumentType))
			call xml_NewElement(xmlfile, 'Manufacturer')
			call xml_AddCharacters(xmlfile, trim(InputChannel(1)%Manufacturer))
			call xml_EndElement(xmlfile, 'Manufacturer')
			call xml_NewElement(xmlfile, 'Name')
			call xml_AddCharacters(xmlfile, trim(InputChannel(1)%InstrumentName))
			call xml_EndElement(xmlfile, 'Name')
			call xml_NewElement(xmlfile, 'Id')
			call xml_AddCharacters(xmlfile, trim(InputChannel(1)%InstrumentID))
			call xml_EndElement(xmlfile, 'Id')
			call xml_NewElement(xmlfile, 'Settings')
			call xml_EndElement(xmlfile, 'Settings')
			call xml_EndElement(xmlfile, 'Magnetometer')
		end if
					
		if (.not. present(OutputChannel)) then		
			! add minimal information on Ex: wire length
			call xml_NewElement(xmlfile, 'Dipole')
			call xml_AddAttribute(xmlfile, 'name', 'Ex')
			call xml_NewElement(xmlfile, 'Length')
			call xml_AddAttribute(xmlfile, 'units', 'meters')
			call xml_AddCharacters(xmlfile, Run%Ex_wire_length, fmt="r3")
			call xml_EndElement(xmlfile, 'Length')
			call xml_EndElement(xmlfile, 'Dipole')
			! add minimal information on Ey: wire length
			call xml_NewElement(xmlfile, 'Dipole')
			call xml_AddAttribute(xmlfile, 'name', 'Ey')
			call xml_NewElement(xmlfile, 'Length')
			call xml_AddAttribute(xmlfile, 'units', 'meters')
			call xml_AddCharacters(xmlfile, Run%Ey_wire_length, fmt="r3")
			call xml_EndElement(xmlfile, 'Length')
			call xml_EndElement(xmlfile, 'Dipole')
		else
			! add full information on electric field channels
			nchoutE = size(OutputChannel)
			do i = 1,nchoutE
				call xml_NewElement(xmlfile, 'Dipole')
				call xml_AddAttribute(xmlfile, 'name', trim(OutputChannel(i)%ID))
				call xml_AddAttribute(xmlfile, 'type', trim(OutputChannel(i)%InstrumentConfig))
				call xml_NewElement(xmlfile, 'Manufacturer')
				call xml_AddCharacters(xmlfile, trim(OutputChannel(i)%Manufacturer))
				call xml_EndElement(xmlfile, 'Manufacturer')
				call xml_NewElement(xmlfile, 'Length')
				call xml_AddAttribute(xmlfile, 'units', 'meters')
				call xml_AddCharacters(xmlfile, OutputChannel(i)%DipoleLength, fmt="r3")
				call xml_EndElement(xmlfile, 'Length')
				call xml_NewElement(xmlfile, 'Azimuth')
				call xml_AddAttribute(xmlfile, 'units', 'degrees')
				call xml_AddCharacters(xmlfile, OutputChannel(i)%DipoleAzimuth, fmt="r3")
				call xml_EndElement(xmlfile, 'Azimuth')
				if (index(OutputChannel(i)%ID,'x')>0) then
					location(1) = 'N'
					location(2) = 'S'
				else if (index(OutputChannel(i)%ID,'y')>0) then
					location(1) = 'E'
					location(2) = 'W'
				end if
				j = index(OutputChannel(i)%InstrumentID,';')
				number(1) = OutputChannel(i)%InstrumentID(1:j-1)
				number(2) = OutputChannel(i)%InstrumentID(j+2:80)
				call xml_NewElement(xmlfile, 'Electrode')
				call xml_AddAttribute(xmlfile, 'location', location(1))
				call xml_AddAttribute(xmlfile, 'number', trim(number(1)))
				call xml_AddCharacters(xmlfile, trim(OutputChannel(i)%InstrumentType))
				call xml_EndElement(xmlfile, 'Electrode')
				call xml_NewElement(xmlfile, 'Electrode')
				call xml_AddAttribute(xmlfile, 'location', location(2))
				call xml_AddAttribute(xmlfile, 'number', trim(number(2)))
				call xml_AddCharacters(xmlfile, trim(OutputChannel(i)%InstrumentType))
				call xml_EndElement(xmlfile, 'Electrode')
				call xml_EndElement(xmlfile, 'Dipole')
			end do	
		end if
		
		if (len_trim(Run%FieldComments)>0) then
			call xml_NewElement(xmlfile, 'Comments')
			call xml_AddAttribute(xmlfile, 'author', trim(Run%SiteInstalledBy))
			call xml_AddCharacters(xmlfile, trim(Run%FieldComments))
			call xml_EndElement(xmlfile, 'Comments')
		end if
		
		if (len_trim(Run%Comments)>0) then
			call xml_NewElement(xmlfile, 'Comments')
			call xml_AddAttribute(xmlfile, 'author', trim(Run%MetaDataCheckedBy))
			call xml_AddCharacters(xmlfile, trim(Run%Comments))
			call xml_EndElement(xmlfile, 'Comments')
		end if

		call xml_NewElement(xmlfile, 'Errors')
		call xml_AddCharacters(xmlfile, trim(Run%Errors))
		call xml_EndElement(xmlfile, 'Errors')

		if (Run%SamplingRate > 0.0d0) then
    		call xml_NewElement(xmlfile, 'SamplingRate')
    		call xml_AddAttribute(xmlfile, 'units', 'Hz')
    		call xml_AddCharacters(xmlfile, Run%SamplingRate, fmt="r3")
    		call xml_EndElement(xmlfile, 'SamplingRate')
    	end if
				
		call xml_NewElement(xmlfile, 'Start')
		call xml_AddCharacters(xmlfile, Run%TimePeriod%StartTime)
		call xml_EndElement(xmlfile, 'Start')

		call xml_NewElement(xmlfile, 'End')
		call xml_AddCharacters(xmlfile, Run%TimePeriod%EndTime)
		call xml_EndElement(xmlfile, 'End')

		call xml_EndElement(xmlfile, 'FieldNotes')   	
    	
	end subroutine add_FieldNotes

  subroutine add_Location(L,decl)
    type(Location_t), intent(in)    :: L
	real(8), optional, intent(in)   :: decl

    call xml_NewElement(xmlfile, 'Location')
    call xml_AddAttribute(xmlfile, 'datum', trim(L%datum))
    
    call xml_NewElement(xmlfile, 'Latitude')
    call xml_AddCharacters(xmlfile, L%lat, fmt="r6")
    call xml_EndElement(xmlfile, 'Latitude')
    
    call xml_NewElement(xmlfile, 'Longitude')
    call xml_AddCharacters(xmlfile, L%lon, fmt="r6")
    call xml_EndElement(xmlfile, 'Longitude')
    
    call xml_NewElement(xmlfile, 'Elevation')
    call xml_AddAttribute(xmlfile, 'units', 'meters')
    call xml_AddCharacters(xmlfile, L%elev,  fmt="r3")
    call xml_EndElement(xmlfile, 'Elevation')
    
    if (present(decl)) then
    	call xml_NewElement(xmlfile, 'Declination')
    	call xml_AddAttribute(xmlfile, 'epoch', '1995.0')
    	call xml_AddCharacters(xmlfile, decl,  fmt="r3")
    	call xml_EndElement(xmlfile, 'Declination')
    end if

    call xml_EndElement(xmlfile, 'Location')

  end subroutine add_Location


  subroutine add_SiteCoords(C)
    type(XYZ_t), intent(in)    :: C

    call xml_NewElement(xmlfile, 'SiteCoords')
    call xml_AddAttribute(xmlfile, 'type', trim(C%type))
    call xml_AddAttribute(xmlfile, 'units', trim(C%units))

    call xml_NewElement(xmlfile, 'X')
    call xml_AddCharacters(xmlfile, C%X, fmt="r6")
    call xml_EndElement(xmlfile, 'X')

    call xml_NewElement(xmlfile, 'Y')
    call xml_AddCharacters(xmlfile, C%Y, fmt="r6")
    call xml_EndElement(xmlfile, 'Y')

    call xml_NewElement(xmlfile, 'Z')
    call xml_AddCharacters(xmlfile, C%Z,  fmt="r3")
    call xml_EndElement(xmlfile, 'Z')

    call xml_EndElement(xmlfile, 'SiteCoords')

  end subroutine add_SiteCoords


  subroutine add_GridOrigin(Site)
    type(Site_t), intent(in)         :: Site

    call xml_NewElement(xmlfile, 'GridOrigin')
    call xml_AddAttribute(xmlfile, 'name', trim(Site%Coords%Origin%ID))
    call add_Location(Site%Coords%Origin)

    call xml_NewElement(xmlfile, 'SiteCoords')
    call xml_AddAttribute(xmlfile, 'type', trim(Site%Coords%Type))
    call xml_AddAttribute(xmlfile, 'units', trim(Site%Coords%Units))

    call xml_NewElement(xmlfile, 'X')
    call xml_AddCharacters(xmlfile, Site%Coords%X, fmt="r6")
    call xml_EndElement(xmlfile, 'X')

    call xml_NewElement(xmlfile, 'Y')
    call xml_AddCharacters(xmlfile, Site%Coords%Y, fmt="r6")
    call xml_EndElement(xmlfile, 'Y')

    call xml_NewElement(xmlfile, 'Z')
    call xml_AddCharacters(xmlfile, Site%Coords%Z,  fmt="r3")
    call xml_EndElement(xmlfile, 'Z')

    call xml_EndElement(xmlfile, 'SiteCoords')
    call xml_EndElement(xmlfile, 'GridOrigin')

  end subroutine add_GridOrigin


  subroutine add_DataType(D)
    type(DataType_t), intent(in)     :: D

    call xml_NewElement(xmlfile, 'DataType')
    call xml_AddAttribute(xmlfile, 'name', trim(D%Name))
    if (D%isComplex) then
        call xml_AddAttribute(xmlfile, 'type', 'complex')
    else
        call xml_AddAttribute(xmlfile, 'type', 'real')
    end if
    call xml_AddAttribute(xmlfile, 'output', trim(D%Output))
    call xml_AddAttribute(xmlfile, 'input', trim(D%Input))
    call xml_AddAttribute(xmlfile, 'units', trim(D%Units))

    call xml_NewElement(xmlfile, 'Description')
    call xml_AddCharacters(xmlfile, trim(D%Description))
    call xml_EndElement(xmlfile, 'Description')

    call xml_NewElement(xmlfile, 'ExternalUrl')
    call xml_AddCharacters(xmlfile, 'http://www.iris.edu/dms/products/emtf/'//trim(D%Tag)//'.html')
    call xml_EndElement(xmlfile, 'ExternalUrl')

    call xml_NewElement(xmlfile, 'Intention')
    call xml_AddCharacters(xmlfile, trim(D%Intention))
    call xml_EndElement(xmlfile, 'Intention')

    call xml_NewElement(xmlfile, 'Tag')
    call xml_AddCharacters(xmlfile, trim(D%Tag))
    call xml_EndElement(xmlfile, 'Tag')

    if (index(D%Intention,'derived')>0) then
        call xml_NewElement(xmlfile, 'DerivedFrom')
        call xml_AddCharacters(xmlfile, trim(D%DerivedFrom))
        call xml_EndElement(xmlfile, 'DerivedFrom')
    end if

    if (len_trim(D%SeeAlso) > 0) then
        call xml_NewElement(xmlfile, 'SeeAlso')
        call xml_AddCharacters(xmlfile, trim(D%SeeAlso))
        call xml_EndElement(xmlfile, 'SeeAlso')
    end if

    call xml_EndElement(xmlfile, 'DataType')

  end subroutine add_DataType


  subroutine add_Estimate(D)
    type(DataType_t), intent(in)     :: D ! statistical estimate

    call xml_NewElement(xmlfile, 'Estimate')
    call xml_AddAttribute(xmlfile, 'name', trim(D%Name))
    if (D%isComplex) then
        call xml_AddAttribute(xmlfile, 'type', 'complex')
    else
        call xml_AddAttribute(xmlfile, 'type', 'real')
    end if

    call xml_NewElement(xmlfile, 'Description')
    call xml_AddCharacters(xmlfile, trim(D%Description))
    call xml_EndElement(xmlfile, 'Description')

    call xml_NewElement(xmlfile, 'ExternalUrl')
    call xml_AddCharacters(xmlfile, 'http://www.iris.edu/dms/products/emtf/'//trim(D%Tag)//'.html')
    call xml_EndElement(xmlfile, 'ExternalUrl')

    call xml_NewElement(xmlfile, 'Intention')
    call xml_AddCharacters(xmlfile, trim(D%Intention))
    call xml_EndElement(xmlfile, 'Intention')

    call xml_NewElement(xmlfile, 'Tag')
    call xml_AddCharacters(xmlfile, trim(D%Tag))
    call xml_EndElement(xmlfile, 'Tag')

    call xml_EndElement(xmlfile, 'Estimate')

  end subroutine add_Estimate


  subroutine add_Channel(C,location)
    type(Channel_t), intent(in)     :: C
    logical, intent(in)           :: location
    ! local
    character(10)   :: type

    if (C%Type .eq. 'E') then
        type = 'Electric'
    elseif (C%Type .eq. 'H') then
        type = 'Magnetic'
    else
        write(0,*) 'Unknown channel type ',trim(C%Type),' on output to XML'
        stop
    end if

    call xml_NewElement(xmlfile, trim(type))
    call xml_AddAttribute(xmlfile, 'name', trim(C%ID))
    call xml_AddAttribute(xmlfile, 'orientation', C%Orientation, fmt="r1")
    call xml_AddAttribute(xmlfile, 'x', C%X, fmt="r1")
    call xml_AddAttribute(xmlfile, 'y', C%Y, fmt="r1")
    call xml_AddAttribute(xmlfile, 'z', C%Z, fmt="r1")
    if (index(C%ID,'E')>0) then
		call xml_AddAttribute(xmlfile, 'x2', C%X2, fmt="r1")
		call xml_AddAttribute(xmlfile, 'y2', C%Y2, fmt="r1")
		call xml_AddAttribute(xmlfile, 'z2', C%Z2, fmt="r1")
	end if
    if (location) then
       call add_Location(C%Location)
    end if
    call xml_EndElement(xmlfile, trim(type))

  end subroutine add_Channel


  subroutine add_PeriodRange(F)
    type(FreqInfo_t), dimension(:), intent(in)  :: F
    real(8), dimension(:), allocatable          :: period
    real(8)                                     :: minFreq, minPeriod
    real(8)                                     :: maxFreq, maxPeriod
	integer                                     :: i, nf

    nf = size(F)
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

    !call xml_NewElement(xmlfile, 'FrequencyRange')
    !call xml_AddAttribute(xmlfile, 'min', minFreq, fmt="s5")
    !call xml_AddAttribute(xmlfile, 'max', maxFreq, fmt="s5")
    !call xml_EndElement(xmlfile, 'FrequencyRange')

  end subroutine add_PeriodRange
  

  subroutine add_Data(Data, Input, Output, iPer)
    type(Data_t), intent(in)                  :: Data
    type(Channel_t), dimension(:), intent(in) :: Input, Output
    integer, intent(in)                       :: iPer
    ! local
    character(20)                             :: str

    call xml_NewElement(xmlfile, trim(Data%Type%Name))
    if (Data%Type%isComplex) then
        call xml_AddAttribute(xmlfile, 'type', 'complex')
    else
        call xml_AddAttribute(xmlfile, 'type', 'real')
    end if
    write(str,'(i3 i3)') Data%nchout, Data%nchin
    call xml_AddAttribute(xmlfile, 'size', trim(adjustl(str)))
    if (len_trim(Data%Type%Units) > 0) then
        call xml_AddAttribute(xmlfile, 'units', trim(Data%Type%Units))
    end if

    do i=1,Data%nchout
       do j=1,Data%nchin
          if (isnan(dreal(Data%Matrix(iPer,i,j))) .or. isnan(dimag(Data%Matrix(iPer,i,j)))) then
            write(0,*) 'Warning: skipping NaN output for ',trim(TF_name(Data%Type,Input(j),Output(i))),' period ',iPer
            cycle
          end if
          call xml_NewElement(xmlfile, 'value')
          call xml_AddAttribute(xmlfile, 'name', trim(TF_name(Data%Type,Input(j),Output(i))))
          call xml_AddAttribute(xmlfile, 'output', trim(Output(i)%ID))
          call xml_AddAttribute(xmlfile, 'input', trim(Input(j)%ID))
          call xml_AddCharacters(xmlfile, dreal(Data%Matrix(iPer,i,j)), fmt="s7")
          if (Data%Type%isComplex) then
            call xml_AddCharacters(xmlfile, ' ')
            call xml_AddCharacters(xmlfile, dimag(Data%Matrix(iPer,i,j)), fmt="s7")
          end if
          call xml_EndElement(xmlfile, 'value')
       end do
    end do

    call xml_EndElement(xmlfile, trim(Data%Type%Name))

  end subroutine add_Data


  subroutine add_Var(Data, Input, Output, iPer)
    type(Data_t), intent(in)                  :: Data
    type(Channel_t), dimension(:), intent(in) :: Input, Output
    integer, intent(in)                       :: iPer
    ! local
    character(20)                             :: str

    call xml_NewElement(xmlfile, trim(Data%Type%Name)//'.VAR')
    call xml_AddAttribute(xmlfile, 'type', 'real')
    write(str,'(i3 i3)') Data%nchout, Data%nchin
    call xml_AddAttribute(xmlfile, 'size', trim(adjustl(str)))

    do i=1,Data%nchout
       do j=1,Data%nchin
          if (isnan(Data%Var(iPer,i,j))) then
            write(0,*) 'Warning: skipping NaN output for ',trim(TF_name(Data%Type,Input(j),Output(i))),' VAR period ',iPer
            cycle
          end if
          call xml_NewElement(xmlfile, 'value')
          call xml_AddAttribute(xmlfile, 'name', trim(TF_name(Data%Type,Input(j),Output(i))))
          call xml_AddAttribute(xmlfile, 'output', trim(Output(i)%ID))
          call xml_AddAttribute(xmlfile, 'input', trim(Input(j)%ID))
          call xml_AddCharacters(xmlfile, Data%Var(iPer,i,j), fmt="s7")
          call xml_EndElement(xmlfile, 'value')
       end do
    end do

    call xml_EndElement(xmlfile, trim(Data%Type%Name)//'.VAR')

  end subroutine add_Var


  subroutine add_InvSigCov(Data, Input, iPer)
    type(Data_t), intent(in)                  :: Data
    type(Channel_t), dimension(:), intent(in) :: Input
    integer, intent(in)                       :: iPer
    ! local
    character(20)                             :: str

    call xml_NewElement(xmlfile, trim(Data%Type%Name)//'.INVSIGCOV')
    call xml_AddAttribute(xmlfile, 'type', 'complex')
    write(str,'(i3 i3)') Data%nchout, Data%nchin
    call xml_AddAttribute(xmlfile, 'size', trim(adjustl(str)))

    do i=1,Data%nchin
       do j=1,Data%nchin
          call xml_NewElement(xmlfile, 'value')
          call xml_AddAttribute(xmlfile, 'output', trim(Input(i)%ID))
          call xml_AddAttribute(xmlfile, 'input', trim(Input(j)%ID))
          call xml_AddCharacters(xmlfile, dreal(Data%InvSigCov(iPer,i,j)), fmt="s7")
          call xml_AddCharacters(xmlfile, ' ')
          call xml_AddCharacters(xmlfile, dimag(Data%InvSigCov(iPer,i,j)), fmt="s7")
          call xml_EndElement(xmlfile, 'value')
       end do
    end do

    call xml_EndElement(xmlfile, trim(Data%Type%Name)//'.INVSIGCOV')

  end subroutine add_InvSigCov


  subroutine add_ResidCov(Data, Output, iPer)
    type(Data_t), intent(in)                  :: Data
    type(Channel_t), dimension(:), intent(in) :: Output
    integer, intent(in)                       :: iPer
    ! local
    character(20)                             :: str

    call xml_NewElement(xmlfile, trim(Data%Type%Name)//'.RESIDCOV')
    call xml_AddAttribute(xmlfile, 'type', 'complex')
    write(str,'(i3 i3)') Data%nchout, Data%nchin
    call xml_AddAttribute(xmlfile, 'size', trim(adjustl(str)))

    do i=1,Data%nchout
       do j=1,Data%nchout
          call xml_NewElement(xmlfile, 'value')
          call xml_AddAttribute(xmlfile, 'output', trim(Output(i)%ID))
          call xml_AddAttribute(xmlfile, 'input', trim(Output(j)%ID))
          call xml_AddCharacters(xmlfile, dreal(Data%ResidCov(iPer,i,j)), fmt="s7")
          call xml_AddCharacters(xmlfile, ' ')
          call xml_AddCharacters(xmlfile, dimag(Data%ResidCov(iPer,i,j)), fmt="s7")
          call xml_EndElement(xmlfile, 'value')
       end do
    end do

    call xml_EndElement(xmlfile, trim(Data%Type%Name)//'.RESIDCOV')

  end subroutine add_ResidCov


  subroutine end_xml_freq_block_output

     call xml_EndElement(xmlfile, 'Data')

  end subroutine end_xml_freq_block_output


  subroutine end_xml_output(product)
    character(len=*), intent(in) :: product

     call xml_EndElement(xmlfile, product)

     call xml_Close(xmlfile)
  end subroutine end_xml_output

end module xml_write
