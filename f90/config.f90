module config

	use FoX_dom
	use parse_dom
	use global
	implicit none
	private

	type(Node), pointer  :: doc

	public  :: read_xml_config
	
contains
  
  subroutine read_xml_config(xmlFile, Info, listDir)
    character(len=*), intent(in)    :: xmlFile
	type(UserInfo_t), intent(out)   :: Info
	character(len=*), intent(in), optional :: listDir
  	  	
  	call init_user_info(Info)
  	  	
  	! Load in the document
  	doc => parseFile(xmlFile)

	Info%Source = getString(doc,"Source")
	Info%Project = getString(doc,"Project")
	Info%Experiment = getString(doc,"Experiment")
	Info%YearCollected = getInteger(doc,"YearCollected")
	Info%ProcessedBy = getString(doc,"ProcessedBy")
	Info%ProcessingSoftware = getString(doc,"ProcessingSoftware")
	Info%ProcessingTag = getString(doc,"ProcessingTag")
	Info%RunList = getString(doc,"RunList")
	Info%SiteList = getString(doc,"SiteList")

	! Clear up all allocated memory
  	call destroy(doc)
  	
  	if (present(listDir)) then
  	  Info%RunList = trim(listDir)//'/'//trim(Info%RunList)
  	  Info%SiteList = trim(listDir)//'/'//trim(Info%SiteList)
    end if

    ! Source, Project and ProcessingSoftware are used to create the ID tags
    if (index(trim(Info%Source),' ')>0) then
        write(5,*) 'Source field in ',trim(xmlFile),' should not contain spaces'
        stop
    end if
    if (index(trim(Info%Project),' ')>0) then
        write(5,*) 'Project field in ',trim(xmlFile),' should not contain spaces'
        stop
    end if
    if (index(trim(Info%ProcessingSoftware),' ')>0) then
        write(5,*) 'ProcessingSoftware field in ',trim(xmlFile),' should not contain spaces'
        stop
    end if
    
    ! ProcessingTag is added to the end of ProductID. Optional.
    if ((len_trim(Info%ProcessingTag)>0) .and. (index(trim(Info%ProcessingTag),' ')>0)) then
        write(5,*) 'ProcessingTag field in ',trim(xmlFile),', if present, should not contain spaces'
        stop
    end if
    
    ! Otherwise, exit successfully
    if (.not.silent) then
		write(*,*) 'Processing experiment ',trim(Info%Experiment),' (',Info%YearCollected,')'
	end if     
	
  end subroutine read_xml_config

end module config
