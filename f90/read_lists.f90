module read_lists

	use FoX_dom
	use parse_dom
	use global
	implicit none
	private

	integer :: i,j

	public  :: read_site_list
	public  :: read_run_list
	public  :: read_channel_list

contains

! ----------------------------------------------------------------------
! Read XML file that consists of a list of 'Site' elements in the format
!   <Site>
!      <ID>ORF08</ID>
!      <Description>Helix,OR,USA</Description>
!      <Location>
!         <Latitude>45.7935</Latitude>
!         <Longitude>-118.742</Longitude>
!         <Elevation>510.0333</Elevation>
!      </Location>
!      <Declination>16.7</Declination>    
!	   <TimePeriod>
!         <Start>2011-06-10T19:09:51</Start>
!         <End>2011-06-10T20:01:07</End>
!      </TimePeriod>
!      <QC>
!         <Rating>4</Rating>
!         <GoodFromPeriod>10</GoodFromPeriod>
!         <GoodToPeriod>10000</GoodToPeriod>
!         <BestTF/> 
!      </QC>
!      <RunList>ORF08a ORF08b ORF08c</RunList>
!   </Site>
  
  subroutine read_site_list(xmlFile, id, Site, exists)
    character(len=*), intent(in)    :: xmlFile
    character(len=*), intent(in)    :: id
    type(Site_t), intent(out)       :: Site
	character(len=80)               :: IDstring
	type(Node), pointer             :: doc, thisSite
	type(NodeList), pointer         :: sites
 	logical, intent(out)            :: exists
  	
	! Initialize site information
	call init_site_info(Site)

	inquire (file=xmlFile,exist=exists)
	
	if (.not.(exists)) then
		write(0,*) 'Unable to read data from XML file ',trim(xmlFile),': File does not exist'
		return
    end if
    
    if (len_trim(xmlFile)==0) then
    	exists = .false.
    	return
	end if
	    
  	! Load in the document
  	doc => parseFile(xmlFile)

	! Loop over the site list. Note that the DOM counts from zero, not from one.
	sites => getElementsByTagName(doc, "Site")
	do i = 0, getLength(sites)-1
  		thisSite => item(sites, i)
  		IDstring = getString(thisSite,"ID")
   		! if this is the correct ID, then pull out all the information from the site tag.
  		if (IDstring(3:5)==id(3:5)) then
  			Site%ID = IDstring
  			Site%Description = getString(thisSite,"Description")
    		Site%Location%lat = getReal(thisSite,"Latitude")
    		Site%Location%lon = getReal(thisSite,"Longitude")
    		Site%Location%elev = getReal(thisSite,"Elevation")
    		Site%Declination = getReal(thisSite,"Declination")
    		Site%QualityRating = getInteger(thisSite,"QualityRating")
    		Site%GoodFromPeriod = getReal(thisSite,"GoodFromPeriod")
    		Site%GoodToPeriod = getReal(thisSite,"GoodToPeriod")
    		Site%QualityComments = getString(thisSite,"QualityComments")
            Site%WarningFlag = getInteger(thisSite,"WarningFlag")
            Site%WarningComments = getString(thisSite,"WarningComments")
    		Site%Start = getString(thisSite,"Start")
    		Site%End = getString(thisSite,"End")
    		Site%RunList = getString(thisSite,"RunList")
     		exit
  		end if
	end do 
	
	! Clear up all allocated memory
  	call destroy(doc)
	
    if (len_trim(Site%Description)==0) then
	    write(0,*) 'Site description has not been obtained from file ',trim(xmlFile)
		exists = .false.
	else if (.not.silent) then
    	write(*,*) 'Site ',Site%ID,' description is ',Site%Description
    	write(*,*) 'Site quality is ',Site%QualityRating,' out of 5'
    	write(*,*) 'Location is ', &
    		Site%Location%lon, Site%Location%lat, Site%Location%elev, Site%Declination
		write(*,*) 'Run list is ', Site%RunList		
    end if
    
  end subroutine read_site_list

! ----------------------------------------------------------------------
! Read XML file that consists of a list of 'Run' elements in the format
!    <Run>
!      <ID>ORF08a</ID>
!      <siteID>ORF08</siteID>
!	   <Manufacturer>Barry Narod</Manufacturer>
!      <Instrument>NIMS MT1</Instrument>
!      <Location>
!         <Latitude>45.7935</Latitude>
!         <Longitude>-118.742</Longitude>
!         <Elevation>509.9</Elevation>
!      </Location>
!      <Declination>16.7</Declination>
!      <TimePeriod>
!         <Start>2006-09-04T17:43:59</Start>
!         <End>2006-09-04T19:04:19</End>
!      </TimePeriod>
!      <Ex_wire_length>100</Ex_wire_length>
!      <Ey_wire_length>100</Ey_wire_length>
!      <SamplingInterval>1</SamplingInterval>
!      <SiteInstalledBy>the author of field comments</SiteInstalledBy>
!      <FieldComments>Cows in area. small dirt road 100 m west</FieldComments>
!      <MetaDataCheckedBy>the author of the comments</MetaDataCheckedBy>
!      <Comments>anything you have to say</Comments>
!      <Errors>problems found in data file </Errors>
!   </Run>
   
  subroutine read_run_list(xmlFile, runlist, Run, exists)
    character(len=*), intent(in)           :: xmlFile
    character(len=*), intent(in)           :: runlist
	type(Run_t), dimension(:), pointer     :: Run
	character(len=80)                      :: IDstring
	type(Node), pointer                    :: doc, thisRun
	type(NodeList), pointer                :: runs
	integer                                :: k, n
	logical, intent(out)                   :: exists

	! this interesting formula for the number of runs stems from the fact
	! that there are 6 letters in a run ID, and n-1 blanks in between,
	! and possibly a leading blank by construction (6n+n-1 or 6n+n letters)
	n = floor((len_trim(runlist)+1)/7.0) 
	allocate(Run(1:n))
	do i = 1,n
		call init_run_info(Run(i))
	end do
	
	inquire (file=xmlFile,exist=exists)
	
	if (.not.(exists)) then
		write(0,*) 'Unable to read data from XML file ',trim(xmlFile),': File does not exist'
		return
    end if

    if (len_trim(xmlFile)==0) then
    	exists = .false.
    	return
	end if
	
 	! Load in the document
  	doc => parseFile(xmlFile)

	! Loop over the site list. Note that the DOM counts from zero, not from one.
	k = 1
	runs => getElementsByTagName(doc, "Run")
	do i = 0, getLength(runs)-1
  		thisRun => item(runs, i)
  		IDstring = getString(thisRun,"ID")
  		! write(*,*) 'Reading run: ',trim(IDstring)
   		! if this is one of the runs in the list, pull out all the information.
  		if (index(runlist,trim(IDstring))>0) then
 			Run(k)%ID = IDstring
			Run(k)%SiteID = getString(thisRun,"siteID")
			Run(k)%Manufacturer = getString(thisRun,"Manufacturer")
			Run(k)%Instrument = getString(thisRun,"Instrument")
			j = index(Run(k)%Instrument,' ')
			Run(k)%InstrumentName = Run(k)%Instrument(1:j-1)
			Run(k)%InstrumentID = Run(k)%Instrument(j+1:80)
			Run(k)%Location%lon = getReal(thisRun,"Longitude")
			Run(k)%Location%lat = getReal(thisRun,"Latitude")
			Run(k)%Location%elev = getReal(thisRun,"Elevation")
			Run(k)%Declination = getReal(thisRun,"Declination")
			Run(k)%TimePeriod%RunID = IDstring
			Run(k)%TimePeriod%StartTime = getString(thisRun,"Start")
			Run(k)%TimePeriod%EndTime = getString(thisRun,"End")
			Run(k)%Ex_wire_length = getReal(thisRun,"Ex_wire_length")
			Run(k)%Ey_wire_length = getReal(thisRun,"Ey_wire_length")
			Run(k)%SamplingInterval = getReal(thisRun,"SamplingInterval")
			Run(k)%SamplingRate = 1.0d0/Run(k)%SamplingInterval
			Run(k)%SiteInstalledBy = getString(thisRun,"SiteInstalledBy")
			Run(k)%MetaDataCheckedBy = getString(thisRun,"MetaDataCheckedBy")
			Run(k)%FieldComments = getString(thisRun,"FieldComments")
			Run(k)%Comments = getString(thisRun,"Comments")
			Run(k)%Errors = getString(thisRun,"Errors")
			if (.not.silent) then
    			if (len_trim(Run(k)%TimePeriod%RunID)==0) then
					write(0,*) 'Start and end times have not been obtained from file ',trim(xmlFile)
				else
					write(*,*) Run(k)%ID
    				write(*,*) 'Start time is ', Run(k)%TimePeriod%StartTime
    				write(*,*) '  End time is ', Run(k)%TimePeriod%EndTime
 				end if			
    		end if
			k = k+1
  		end if
	end do 
	
	! Clear up all allocated memory
  	call destroy(doc)

  end subroutine read_run_list
  
! -------------------------------------------------------------------------
! Read XML file that consists of a list of 'Channel' elements in the format
!  <Channel>
!    <ID>LQN</ID>
!    <Name>Ex</Name>
!    <siteID>NVM07</siteID>
!    <runID>NVM07a</runID>
!    <Instrument>Dipole</Instrument>
!    <InstrumentType>Pb-PbCl2 kaolin gel Petiau 2 chamber type OSU V2.0</InstrumentType>
!    <Manufacturer>Oregon State University/Adam Schultz and Tristan Peery</Manufacturer>
!    <InstrumentName/> 
!    <InstrumentID>10021; 10059</InstrumentID>
!    <InstrumentConfig>wire</InstrumentConfig>
!    <TimePeriod>
!      <Start>2011-05-14T20:20:48</Start>
!      <End>2011-05-14T21:19:21</End>
!    </TimePeriod>
!    <SamplingRate>
!      <Value>1</Value>
!      <Units>Hz</Units>
!    </SamplingRate>
!    <DipoleLength>100</DipoleLength>
!    <Orientation>15.6</Orientation>
!    <LowPassCutoff>0.5</LowPassCutoff>
!    <HighPassCutoff>NaN</HighPassCutoff>
!    <TimeOffset>-0.285</TimeOffset>
!    <Correction>
!      <Value>1</Value>
!      <Type>gain</Type>
!    </Correction>
!    <Conversion>2.441412210479e-06</Conversion>
!    <Comments>x 2.4414e-06 to get mv, Gain 1</Comments>
!  </Channel>
! Most of this is necessary for archiving in SEED format. 
! For our purposes, we only need the instrument information.
! In this routine, we update existing Channel information with
! the additional meta data from the list.

  subroutine read_channel_list(xmlFile, myRun, Channel, exists)
    character(len=*), intent(in)           :: xmlFile
    character(len=*), intent(in)           :: myRun
	character(len=80)                      :: myChannel
	type(Channel_t), dimension(:), pointer :: Channel
	character(len=80)                      :: thisRun, thisID
	type(Node), pointer                    :: doc, thisChannel
	type(NodeList), pointer                :: channels
	integer                                :: k, n, istat
	logical, intent(out)                   :: exists

	n = size(Channel)
	
	inquire (file=xmlFile,exist=exists)
	
	if (.not.(exists)) then
		write(0,*) 'Unable to read data from XML file ',trim(xmlFile),': File does not exist'
		return
    end if

    if (len_trim(xmlFile)==0) then
    	exists = .false.
    	return
	end if
	
 	! Load in the document
  	doc => parseFile(xmlFile)

	! Loop over the channels list. Note that the DOM counts from zero, not from one.
	channels => getElementsByTagName(doc, "Channel")
	CHANNELS_LOOP: do k = 1,n
		myChannel = Channel(k)%ID
		do i = 0, getLength(channels)-1
	  		thisChannel => item(channels, i)
	  		thisRun = getString(thisChannel,"runID")
	   		! if this has correct run and channel IDs, pull out all the information.
	  		if (trim(myRun) .eq. trim(thisRun)) then
				if (trim(myChannel) .eq. trim(getString(thisChannel,"Name"))) then
					Channel(k)%DipoleLength = getReal(thisChannel,"DipoleLength")
					Channel(k)%DipoleAzimuth = getReal(thisChannel,"Orientation")
					Channel(k)%Instrument = getString(thisChannel,"Instrument")
					Channel(k)%InstrumentType = getString(thisChannel,"InstrumentType")
					Channel(k)%Manufacturer = getString(thisChannel,"Manufacturer")
					Channel(k)%InstrumentName = getString(thisChannel,"InstrumentName")
					Channel(k)%InstrumentID = getString(thisChannel,"InstrumentID")
					Channel(k)%InstrumentConfig = getString(thisChannel,"InstrumentConfig")
					if (Channel(k)%DipoleLength > 0) then
						if (index(Channel(k)%ID,'x') > 0) then
							Channel(k)%X  = - Channel(k)%DipoleLength/2;
							Channel(k)%X2 = + Channel(k)%DipoleLength/2;
						elseif (index(Channel(k)%ID,'y') > 0) then
							Channel(k)%Y  = - Channel(k)%DipoleLength/2;
							Channel(k)%Y2 = + Channel(k)%DipoleLength/2;
						end if
					end if				
					if (.not.silent) then
		    			if (len_trim(Channel(k)%Instrument)==0) then
							write(0,*) 'Channel instrument information has not been obtained from file ',trim(xmlFile)
						else
							write(*,*) trim(myRun),' ',trim(myChannel), &
							' collected with ',trim(Channel(k)%InstrumentName),' ',trim(Channel(k)%InstrumentID)
						end if			
		    		end if
		    		cycle CHANNELS_LOOP ! go to next channel element
	    		end if
	  		end if
  		end do
	end do CHANNELS_LOOP
	
	! Clear up all allocated memory
  	call destroy(doc)

  end subroutine read_channel_list
  
end module read_lists
