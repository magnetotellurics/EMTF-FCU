module config

	use FoX_dom
	use parse_dom
	use global
	use utils
	implicit none
	private

	type(Node), pointer     :: doc
	character(200), pointer :: strarray(:)

	public  :: read_xml_config

contains

  subroutine read_xml_config(xmlFile, Info, DataType, Estimate, listDir)
    character(len=*), intent(in)    :: xmlFile
	type(UserInfo_t), intent(out)   :: Info
    type(DataType_t), pointer, intent(inout) :: DataType(:)
    type(DataType_t), pointer, intent(inout) :: Estimate(:)
	character(len=*), intent(in), optional :: listDir
	type(Node), pointer             :: creator,submitter,software
	type(Node), pointer             :: copyright,author
	type(NodeList), pointer         :: authors
    ! local
    character(len=200) copyright_file, datatype_file
    logical copyright_exists, datatype_exists
    integer i,ios,istat,fileid,ediparse,ediwrite,tsinfo,ntags

  	call init_user_info(Info)

  	! Load in the document
  	doc => parseFile(xmlFile)

    tsinfo = getInteger(doc,"TimeSeriesArchived")
    if (tsinfo>0) then
        Info%TimeSeriesArchived = .TRUE.
    end if
    Info%Network = getString(doc,"Network")
	Info%Project = getString(doc,"Project")
	Info%Survey = getString(doc,"Survey")
	Info%YearCollected = getString(doc,"YearCollected")
    Info%Country = getString(doc,"Country")
	Info%Tags = getString(doc,"Tags")

    copyright => item(getElementsByTagName(doc, "Citation"),0)
    Info%Copyright%Authors = getString(copyright,"Authors")
    Info%Copyright%Title = getString(copyright,"Title")
    Info%Copyright%Year = getString(copyright,"Year")
    Info%Copyright%DOI = getString(copyright,"DOI")
	Info%Copyright%ReleaseStatus = getString(doc,"ReleaseStatus")
	
	if (index(Info%Copyright%ReleaseStatus,'Unrestricted Release')>0) then
	    copyright_file = 'COPYRIGHT/UnrestrictedRelease.copyright'
        inquire (file=copyright_file,exist=copyright_exists)
	elseif (index(Info%Copyright%ReleaseStatus,'Academic Use Only')>0) then
        copyright_file = 'COPYRIGHT/AcademicUseOnly.copyright'
        inquire (file=copyright_file,exist=copyright_exists)
    elseif (index(Info%Copyright%ReleaseStatus,'Conditions Apply')>0) then
        copyright_file = 'COPYRIGHT/ConditionsApply.copyright'
        inquire (file=copyright_file,exist=copyright_exists)
    else
        copyright_exists = .false.
    end if

	if (copyright_exists) then
        fileid=105
        write(6,*) 'Reading from copyright file: ',trim(copyright_file)
        write(6,*) 'Conditions of use: '
        open (unit=fileid,file=copyright_file,status='old',iostat=ios)
        do i=1,size(Info%Copyright%ConditionsOfUse)
            read (fileid,'(a100)',iostat=ios) Info%Copyright%ConditionsOfUse(i)
            if (.not. isempty(Info%Copyright%ConditionsOfUse(i))) then
                write(6,*) len_trim(Info%Copyright%ConditionsOfUse(i)),trim(Info%Copyright%ConditionsOfUse(i))
            end if
        end do
        close (fileid)
    end if

	creator => item(getElementsByTagName(doc, "Creator"),0)
	Info%Creator%Name = getString(creator,"Name")
	Info%Creator%Email = getString(creator,"Email")
	Info%Creator%Org = getString(creator,"Org")
	Info%Creator%OrgUrl = getString(creator,"OrgUrl")

	submitter => item(getElementsByTagName(doc, "Submitter"),0)
	Info%Submitter%Name = getString(submitter,"Name")
	Info%Submitter%Email = getString(submitter,"Email")
	Info%Submitter%Org = getString(submitter,"Org")
	Info%Submitter%OrgUrl = getString(submitter,"OrgUrl")

	Info%AcquiredBy = getString(doc,"AcquiredBy")
	Info%ProcessedBy = getString(doc,"ProcessedBy")
	
	software => item(getElementsByTagName(doc, "ProcessingSoftware"),0)
	Info%ProcessingSoftware = getString(software,"Name")
	Info%ProcessingSoftwareLastMod = getString(software,"LastMod")
	Info%ProcessingSoftwareAuthor = getString(software,"Author")

    Info%DateFormat = getString(doc,"DateFormat")
    ediparse = -1
    ediparse = getInteger(doc,"ParseEDIInfo")
    if (ediparse==1) then
        Info%ParseEDIInfo = .TRUE.
    elseif (ediparse==0) then
        Info%ParseEDIInfo = .FALSE.
    end if
    ediwrite = -1
    ediwrite = getInteger(doc,"WriteEDIInfo")
    if (ediwrite==1) then
        Info%WriteEDIInfo = .TRUE.
    elseif (ediwrite==0) then
        Info%WriteEDIInfo = .FALSE.
    end if
    Info%Image = getString(doc,'Image')
    Info%Original = getString(doc,'Original')
	Info%OrthogonalGeographic = getInteger(doc,"OrthogonalGeographic")
	Info%RunList = getString(doc,"RunList")
	Info%SiteList = getString(doc,"SiteList")
	Info%ChannelList = getString(doc,"ChannelList")

	! Clear up all allocated memory
  	call destroy(doc)

    ! Read the tags and initialize data types from files
    call parse_str(Info%Tags,',',strarray,ntags)
    if (associated(DataType)) then
       deallocate(DataType, stat=istat)
    end if
    allocate(DataType(ntags), stat=istat)
    do i = 1,ntags
        datatype_file = 'DATATYPES/'//trim(strarray(i))//'.xml'
        inquire (file=datatype_file,exist=datatype_exists)
        if (datatype_exists) then
            write(0,*) 'Reading from the data type definition file ',trim(datatype_file)
            call read_xml_data_type(datatype_file, DataType(i))
        else
            write(0,*) 'Unable to find the data type definition file ',trim(datatype_file)
            write(0,*) 'Please correct your tags or create a new data type definition.'
            write(0,*) 'Cannot proceed with file conversion. Exiting...'
            stop
        end if
    end do

    ! Read all statistical estimate info from files
    if (associated(Estimate)) then
       deallocate(Estimate, stat=istat)
    end if
    allocate(Estimate(size(Info%Estimate)), stat=istat)
    do i = 1,size(Info%Estimate)
        datatype_file = 'DATATYPES/'//trim(Info%Estimate(i))//'.xml'
        inquire (file=datatype_file,exist=datatype_exists)
        if (datatype_exists) then
            write(0,*) 'Reading statistical estimate definition from file ',trim(datatype_file)
            call read_xml_data_type(datatype_file, Estimate(i))
        else
            write(0,*) 'Unable to find statistical estimate definition file ',trim(datatype_file)
            write(0,*) 'Warning: Skipping ',trim(Info%Estimate(i)),': not defined.'
        end if
    end do

  	if (present(listDir)) then
  	  Info%RunList = trim(listDir)//'/'//trim(Info%RunList)
  	  Info%SiteList = trim(listDir)//'/'//trim(Info%SiteList)
  	  Info%ChannelList = trim(listDir)//'/'//trim(Info%ChannelList)
    end if

    ! Project name is used to create the ID tags
    if (index(trim(Info%Project),' ')>0) then
        write(0,*) 'Project field in ',trim(xmlFile),' should not contain spaces'
        stop
    end if

    ! Otherwise, exit successfully
    if (.not.silent) then
		write(*,*) 'Processing survey ',trim(Info%Project),' ',trim(Info%Survey),' (',trim(Info%YearCollected),')'
	end if

  end subroutine read_xml_config


  subroutine read_xml_data_type(xmlFile, DataType)
    character(len=*), intent(in)    :: xmlFile
    type(DataType_t), intent(inout) :: DataType
    type(Node), pointer             :: dt
    character(80)                   :: str

    ! Load in the document
    doc => parseFile(xmlFile)

    call init_data_type(DataType)
    DataType%Intention = getString(doc,"Intention")
    DataType%Description = getString(doc,"Description")
    DataType%Tag = getString(doc,"Tag")
    dt = getFirstChild(doc)
    DataType%Name = getAttribute(dt,"name")
    if (hasAttribute(dt,"units")) then
        DataType%Units = getAttribute(dt,"units")
    end if
    if (hasAttribute(dt,"input")) then
        DataType%Input = getAttribute(dt,"input")
    end if
    if (hasAttribute(dt,"output")) then
        DataType%Output = getAttribute(dt,"output")
    end if
    str = getAttribute(dt,"type")
    if (index(str,'complex')>0) then
        DataType%isComplex = .true.
    else
        DataType%isComplex = .false.
    end if
    DataType%allocated = .true.

    ! Clear up all allocated memory
    call destroy(doc)

    ! Parse the "Names" and save into Components
!    call parse_str(DataType%Names,',',strarray,DataType%nComp)
!    allocate(DataType%Component(len(strarray)), stat=istat)
!    do i = 1,len(strarray)
!        DataType%Component(i) = trim(strarray(i))
!    end do

  end subroutine read_xml_data_type

end module config
