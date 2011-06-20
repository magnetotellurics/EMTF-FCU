module xml_read

	use FoX_dom
	use parse_dom
	use global
	implicit none
	private

	type(Node), pointer             :: doc
	type(NodeList), pointer         :: periods
	type(NodeList), pointer         :: channels
	integer                         :: i,j

	public  :: initialize_xml_input, end_xml_input
	public  :: read_xml_header, read_xml_channels, read_xml_period

contains
  
  subroutine initialize_xml_input(xmlFile, xmlTime)
    character(len=*), intent(in)    :: xmlFile
	character(len=19), optional, intent(out)  :: xmlTime
  	
  	! Load in the document
  	doc => parseFile(xmlFile)

	! Get all frequencies / periods and count them
	periods => getElementsByTagName(doc, "Period")
	nf = getLength(periods)

	! Get all channels and count them
	channels => getElementsByTagName(doc, "Channel")
	nch = getLength(channels)
	
	if (present(xmlTime)) then
		xmlTime = getString(doc, "CreateTime")
	end if
	
	if (.not.silent) then
		write(*,*) 'Reading from file ',xmlFile
	end if
	
  end subroutine initialize_xml_input


  subroutine read_xml_header(id, Site, UserInfo, Info)
    character(len=80), intent(out)  :: id
    type(Site_t), intent(out)       :: Site
    type(UserInfo_t), intent(out)   :: UserInfo
    type(RemoteRef_t), intent(out)  :: Info
	type(Node), pointer             :: infoNode
	character(len=80)               :: project

	! Initialize site information
	call init_site_info(Site)
	call init_user_info(UserInfo)
	call init_remote_ref(Info)

	UserInfo%Project = getString(doc,"Project")
	UserInfo%Survey = getString(doc,"Survey")
	UserInfo%YearCollected = getString(doc,"YearCollected")
	UserInfo%YearCollected = getString(doc,"YearCollected")
	UserInfo%AcquiredBy = getString(doc,"AcquiredBy")
	UserInfo%ProcessedBy = getString(doc,"ProcessedBy")

	infoNode => item(getElementsByTagName(doc, "ProcessingSoftware"),0)
	UserInfo%ProcessingSoftware = getString(infoNode,"Name")

	! Need to create this node since tags like ID and Location
	! are encountered several times throughout the document
	infoNode => item(getElementsByTagName(doc, "Site"),0)
	
	Site%ID = getString(infoNode,"ID")
	Site%Description = getString(infoNode,"Name")
	Site%Location%lat = getReal(infoNode,"Latitude")
	Site%Location%lon = getReal(infoNode,"Longitude")
	Site%Location%elev = getReal(infoNode,"Elevation")
	Site%Declination = getReal(infoNode,"Declination")
	Site%RunList = getString(infoNode,"RunList")

	id = getString(doc,"ProcessingTag")

	infoNode => item(getElementsByTagName(doc, "ProcessingInfo"),0)

	call init_remote_ref(Info)
  	Info%remote_ref_type = getStringAttr(infoNode,"RemoteRef","type")
  	if (index(Info%remote_ref_type,'Remote Reference')>0) then
		Info%remote_ref = .true.
	end if
	Info%sign_convention = getString(infoNode,"SignConvention")
	Info%remote_site_id = getString(infoNode,"ID")
	Info%processing_tag = id

  end subroutine read_xml_header
  

  subroutine read_xml_channels(Input, Output)
	type(Channel_t), dimension(:), intent(inout) :: Input
    type(Channel_t), dimension(:), intent(inout) :: Output
	type(Node), pointer                          :: this

    do i=1,2
       this => item(channels, i-1)
       Input(i)%ID = getAttribute(this,"name")
       Input(i)%orientation = getRealAttr(this,"Channel","orientation")
       Input(i)%tilt = 0.0
       !Input(i)%units = getAttribute(this,"units")
    end do

    do i=1,nch-2
       this => item(channels, i+1)
       Output(i)%ID = getAttribute(this,"name")
       Output(i)%orientation = getRealAttr(this,"Channel","orientation")
       Output(i)%tilt = 0.0
       !Output(i)%units = getAttribute(this,"units")
    end do

  end subroutine read_xml_channels
  
  
  subroutine read_xml_period(k,F,TF,TFVar,InvSigCov,ResidCov)
  	integer,                    intent(in)    :: k
    type(FreqInfo_t),           intent(out)   :: F
    complex(8), dimension(:,:), intent(inout) :: TF
    real(8),    dimension(:,:), intent(inout) :: TFVar
    complex(8), dimension(:,:), intent(inout), optional :: InvSigCov
    complex(8), dimension(:,:), intent(inout), optional :: ResidCov
    type(Node), pointer                       :: thisFreq
    type(Node), pointer                       :: thisNode
	type(NodeList), pointer                   :: list
	type(Node), pointer                       :: comp
	character(4)                              :: input, output
	character(12)                             :: tag
	character(800)							  :: str
	real(8)                                   :: vreal, vimag
	real(8), dimension(:), allocatable        :: v
	integer									  :: i,j,ind,istat
	
	call init_freq_info(F)
	TF = dcmplx(0.0d0,0.0d0)
	TFVar = 0.0d0
	if (present(InvSigCov)) InvSigCov = dcmplx(0.0d0,0.0d0)
	if (present(ResidCov)) ResidCov = dcmplx(0.0d0,0.0d0)

	thisFreq => item(periods, k-1)
	
	F%value = getRealAttr(thisFreq,"Period","value")
	F%units = 'secs'
   	F%info_type = 'period'
	if (hasAttribute(thisFreq, "units")) then
      if (getAttribute(thisFreq, "units")=='Hz') then
      	F%info_type = 'frequency'
      end if
    end if
    
	F%num_points = 0
	F%dec_level = 0
	
	if (.not.silent) then
		write(*,*) 'Reading ', trim(F%info_type),' ',k,': ',F%value
	end if
	
	thisNode => item(getElementsByTagName(thisFreq, "TF"),0)
	list => getElementsByTagName(thisNode,"value")
	do i=0,getLength(list)-1
		comp => item(list,i)
		str = getString(comp,"value")
		read(str,*) vreal,vimag
		input = getAttribute(comp,"input")
		output = getAttribute(comp,"output")
		tag = trim(input)//' -> '//trim(output)
		select case ( tag )
		case ('Hx -> Hz') !TX
			TF(1,1) = dcmplx(vreal,vimag)
		case ('Hy -> Hz') !TY
			TF(1,2) = dcmplx(vreal,vimag)
		case ('Hx -> Ex') !ZXX
			TF(2,1) = dcmplx(vreal,vimag)
		case ('Hy -> Ex') !ZXY
			TF(2,2) = dcmplx(vreal,vimag)
		case ('Hx -> Ey') !ZYX
			TF(3,1) = dcmplx(vreal,vimag)
		case ('Hy -> Ey') !ZYY
			TF(3,2) = dcmplx(vreal,vimag)
		case default
			write(*,*) 'Unknown input/output pair ',trim(tag),': TF value not stored!'
		end select		
	end do
	
	thisNode => item(getElementsByTagName(thisFreq, "TFVAR"),0)
	list => getElementsByTagName(thisNode,"value")
	do i=0,getLength(list)-1
		comp => item(list,i)
		vreal = getReal(comp,"value")
		input = getAttribute(comp,"input")
		output = getAttribute(comp,"output")
		tag = trim(input)//' -> '//trim(output)
		select case ( tag )
		case ('Hx -> Hz') !TX
			TFVar(1,1) = vreal
		case ('Hy -> Hz') !TY
			TFVar(1,2) = vreal
		case ('Hx -> Ex') !ZXX
			TFVar(2,1) = vreal
		case ('Hy -> Ex') !ZXY
			TFVar(2,2) = vreal
		case ('Hx -> Ey') !ZYX
			TFVar(3,1) = vreal
		case ('Hy -> Ey') !ZYY
			TFVar(3,2) = vreal
		case default
			write(*,*) 'Unknown input/output pair ',trim(tag),': TFVar value not stored!'
		end select		
	end do

	if (.not.(present(InvSigCov))) return

	allocate(v(2*(2*2)), stat=istat)
	thisNode => item(getElementsByTagName(thisFreq, "INVSIGCOV"),0)
	str = getString(thisNode,"value")
	if (.not.silent) then
		write(*,*) 'InvSigCov: ', trim(str)
	end if
	read(str,*) v
	ind = 1
	do i=1,2
		do j=1,2
			InvSigCov(i,j) = dcmplx(v(ind),v(ind+1))
			ind = ind+2
		end do
	end do
	deallocate(v, stat=istat)
			
	if (.not.(present(ResidCov))) return
	
	allocate(v(2*(nch-2)*(nch-2)), stat=istat)
	thisNode => item(getElementsByTagName(thisFreq, "RESIDCOV"),0)
	str = getString(thisNode,"value")
	if (.not.silent) then
		write(*,*) 'ResidCov: ', trim(str)
	end if
	read(str,*) v
	ind = 1
	do i=1,nch-2
		do j=1,nch-2
			ResidCov(i,j) = dcmplx(v(ind),v(ind+1))
			ind = ind+2
		end do
	end do
	deallocate(v, stat=istat)

  end subroutine read_xml_period
  
  
  subroutine end_xml_input

	! Clear up all allocated memory
  	call destroy(doc)

  end subroutine end_xml_input

end module xml_read
