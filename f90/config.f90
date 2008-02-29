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
	Info%ProcessedBy = getString(doc,"ProcessedBy")
	Info%Year = getInteger(doc,"Year")
	Info%ID = getString(doc,"ID")
	Info%RunList = getString(doc,"RunList")
	Info%SiteList = getString(doc,"SiteList")

	! Clear up all allocated memory
  	call destroy(doc)
  	
  	if (present(listDir)) then
  	  Info%RunList = trim(listDir)//'/'//trim(Info%RunList)
  	  Info%SiteList = trim(listDir)//'/'//trim(Info%SiteList)
    end if

	if (.not.silent) then
		write(*,*) 'Processing experiment ',trim(Info%Source),' (',Info%Year,')'
	end if
	
  end subroutine read_xml_config

end module config
