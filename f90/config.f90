module config

	use FoX_dom
	use parse_dom
	use global
	use utils
	implicit none
	private

	type(Node), pointer  :: doc

	public  :: read_xml_config

contains

  subroutine read_xml_config(xmlFile, Info, listDir)
    character(len=*), intent(in)    :: xmlFile
	type(UserInfo_t), intent(out)   :: Info
	character(len=*), intent(in), optional :: listDir
	type(Node), pointer             :: creator,submitter,software
	type(Node), pointer             :: copyright,author
	type(NodeList), pointer         :: authors
    ! local
    character(len=100) copyright_file
    logical copyright_exists
    integer i,ios,fileid

  	call init_user_info(Info)

  	! Load in the document
  	doc => parseFile(xmlFile)

	Info%Project = getString(doc,"Project")
	Info%Survey = getString(doc,"Survey")
	Info%YearCollected = getString(doc,"YearCollected")
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

	Info%OrthogonalGeographic = getInteger(doc,"OrthogonalGeographic")
	Info%RunList = getString(doc,"RunList")
	Info%SiteList = getString(doc,"SiteList")
	Info%ChannelList = getString(doc,"ChannelList")

	! Clear up all allocated memory
  	call destroy(doc)

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

end module config
