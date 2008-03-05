module read_lists

	use FoX_dom
	use parse_dom
	use global
	implicit none
	private

	integer :: i,j

	public  :: read_site_list
	public  :: read_run_list

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
!      <SamplingFreq>8</SamplingFreq>
!      <Comments>anything you have to say</Comments>
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
   		! if this is one of the runs in the list, pull out all the information.
  		if (index(runlist,trim(IDstring))>0) then
 			Run(k)%ID = IDstring
			Run(k)%SiteID = getString(thisRun,"siteID")
			Run(k)%Instrument = getString(thisRun,"Instrument")
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
			Run(k)%Comments = getString(thisRun,"Comments")
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

end module read_lists
