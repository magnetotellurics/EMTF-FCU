program debug_xml_read

    use FoX_dom
    implicit none

    type(Node), pointer             :: textNode,doc
    type(NodeList), pointer         :: list
    integer                         :: i
    character(len=80)   :: xmlFile = 'impedance.xml'
    type(Node), pointer             :: dt
    character(len=80)                   :: temp='something'

    ! Load in the document
    doc => parseFile(xmlFile)
write(*,*) 'DEBUG: ', 'xml reading initialized'
    dt => getDocumentElement(doc)
write(*,*) 'DEBUG: 2'
    temp = getAttribute(dt,"name")
write(*,*) 'DEBUG: 3 ', temp

    ! Clear up all allocated memory
    call destroy(doc)
write(*,*) 'DEBUG: ', 'xml reading completed'


end program
