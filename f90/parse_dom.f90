!---------------------------------------------------------------------------------!
! parse_dom: Fortran 90 module that contains functions, similar in nature to DOM  !
! functions, to extract a string, a real value or an integer from a simpleType    !
! element text node, or the text node of any child of that element, or from an    !
! attribute of that element or any of its children. The only requirement is that  !
! the element you parse does not contain mixed content, i.e. both text and child  !
! elements.                                                                       !
!                                                                                 !
! parse_dom is free software; you can redistribute it and/or modify it under the  !
! terms of the GNU General Public License (GPL) version 3 or above, as published  !
! by the Free Software Foundation.                                                !
!                                                                                 !
! This software is distributed in the hope that it will be useful, but WITHOUT    !
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS   !
! FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.  !
!                                                                                 !
! Copyright (C) Anna Kelbert, 2007                                                !
!               College of Oceanic and Atmospheric Sciences                       !
!               Oregon State University                                           !
!               104 COAS Administration Building                                  !
!               Corvallis, OR 97331-5503                                          !
!                                                                                 !
! Please contact the author with questions, comments or corrections               !
!                              at the e-mail address:  anya@coas.oregonstate.edu  !
!                                        or by phone: +1-541-737-4113             !
!                                                                                 !
! parse_dom uses Fortran XML library (FoX),(C) 2005-2007 Toby White               !
! This library is freely available under a 3-clause BSD license; which is         !
! to say that it may be used freely by any and all users; redistributed in        !
! source or binary form; provided that the copyright notices are maintained.      !
! The FoX library is not included into this distribution of EMTF FCU.             !
! It can be downloaded at http://uszla.me.uk/space/software/FoX/                  !
!---------------------------------------------------------------------------------!
module parse_dom

	use FoX_dom
	implicit none
	private

	type(Node), pointer             :: textNode
	type(NodeList), pointer         :: list
	integer                         :: i

	public :: getStringAttr, getString
	public :: getRealAttr, getReal
	public :: getIntegerAttr, getInteger

contains

! *****************************************************************
! Get the attribute with tag attrName from the first child element
! of domNode with tag xmlName. Store the result in a string.

    function getStringAttr(domNode, xmlName, attrName) result (str)
	type(Node), pointer             :: domNode
	type(NodeList), pointer         :: nodes
    character(len=*), intent(in)    :: xmlName
    character(len=*), intent(in)    :: attrName
    logical                         :: hasAttr
	character(len=80)               :: str

	! Initialize output
	str = " "

	! Make sure the pointer is associated
	if (.not.(associated(domNode))) then
		write(0,*) 'Error: pointer not associated in getStringAttr'
		return
	end if

	! First try the attributes of the input node
	if (getLocalName(domNode)==trim(xmlName)) then
		if (hasAttribute(domNode, attrName)) then
			str = getAttribute(domNode, attrName)
			hasAttr = .true.
			return
		end if
	end if

	! If that doesn't work, try all child elements
	list => getElementsByTagName(domNode, trim(xmlName))
	if (getLength(list)<1) then
		write(0,*) 'XML Error: no tag ',trim(xmlName)
		return
	end if

	hasAttr = .false.
	! Find the first child of this node with the tag and attribute of interest
	do i=0,getLength(list)-1
		textNode => item(list, i)
    	if (hasAttribute(textNode, attrName)) then
    		str = getAttribute(textNode, attrName)
    		hasAttr = .true.
			exit
      	end if
    end do

    if (.not.hasAttr) then
      	write(0,*) 'XML Error: no child ',trim(xmlName),' with attribute ',trim(attrName)
      	return
    end if

  end function getStringAttr

! *****************************************************************
! Get the attribute with tag attrName from the first child element
! of domNode with tag xmlName. Store the result as a real value.

  function getRealAttr(domNode, xmlName, attrName) result (value)
	type(Node), pointer             :: domNode
    character(len=*), intent(in)    :: xmlName
    character(len=*), intent(in)    :: attrName
	character(len=80)               :: str
 	real(8)                         :: value

	! Initialize output
	value = 0.0d0

	! Make sure the pointer is associated
	if (.not.(associated(domNode))) then
		write(0,*) 'Error: pointer not associated in getStringAttr'
		return
	end if

	str = getStringAttr(domNode, xmlName, attrName)

	if (len_trim(str)>0) then
		if ((index(str,'e')>0).or.(index(str,'E')>0)) then
			read(str, '(e16.5)') value
		else
			if (index(str,'.')==0) then
				str = trim(str)//'.'
			end if
			read(str, '(f9.6)') value
		end if
	else
		write(0,*) 'XML Error: unable to convert the attribute ',trim(attrName),' to a real value'
	end if

  end function getRealAttr

! *****************************************************************
! Get the attribute with tag attrName from the first child element
! of domNode with tag xmlName. Store the result as an integer.

  function getIntegerAttr(domNode, xmlName, attrName) result (value)
	type(Node), pointer             :: domNode
    character(len=*), intent(in)    :: xmlName
    character(len=*), intent(in)    :: attrName
	character(len=80)               :: str
 	integer                         :: value

	! Initialize output
	value = 0

	! Make sure the pointer is associated
	if (.not.(associated(domNode))) then
		write(0,*) 'Error: pointer not associated in getStringAttr'
		return
	end if

	str = getStringAttr(domNode, xmlName, attrName)

	if (len_trim(str)>0) then
		read(str, '(i8)') value
	else
		write(0,*) 'XML Error: unable to convert the attribute ',trim(attrName),' to an integer'
	end if

  end function getIntegerAttr

! *****************************************************************
! Get the text node from the first child element
! of domNode with tag xmlName. Store the result in a string.

  function getString(domNode, xmlName, attrName, attrValue) result (str)
	type(Node), pointer             :: domNode
    character(len=*), intent(in)    :: xmlName
    character(len=*), intent(in), optional  :: attrName
    character(len=*), intent(in), optional  :: attrValue
    logical                         :: hasAttr
	character(len=400)              :: str

	! Initialize output
	str = " "

	! Make sure the pointer is associated
	if (.not.(associated(domNode))) then
		write(0,*) 'Error: pointer not associated in getStringAttr'
		return
	end if

	if (getLocalName(domNode)==trim(xmlName)) then

		! First try the text node of the input node
		if (present(attrName)) then
			if (hasAttribute(domNode, attrName)) then
				if (present(attrValue)) then
					if (getAttribute(textNode, attrName)==trim(attrValue)) then
						! a) the attribute value is specified and found to be correct
      					textNode => domNode
      				end if
      			else
      				! b) the attribute is correct and the attribute value is not specified
      				textNode => domNode
      			end if
			end if
		end if
		! c) the tag name is correct and the attribute requirement is not specified
		textNode => domNode
	else

		! If that doesn't work, try all child elements
		list => getElementsByTagName(domNode, trim(xmlName))
		if (getLength(list)<1) then
			write(0,*) 'XML Error: no tag ',trim(xmlName)
			return
		end if

		if (present(attrName)) then
			hasAttr = .false.
			! Find the first child of this node with the tag and attribute of interest
			do i=0,getLength(list)-1
				textNode => item(list, i)
	    		if (hasAttribute(textNode, attrName)) then
	    			if (present(attrValue)) then
	      				if (getAttribute(textNode, attrName)==trim(attrValue)) then
	      					hasAttr = .true.
	      					exit
	      				end if
	      			else
	      				hasAttr = .true.
	      				exit
	      			end if
	      		end if
	      	end do
	      	if (.not.hasAttr) then
	      		write(0,*) 'XML Error: no child ',trim(xmlName),' with this value of attribute ',trim(attrName)
	      		return
	      	end if
		else
	  		! Take the first child of this node that has the tag of interest
			textNode => item(list, 0)
		end if

	end if

  	! and we know that the data we are interested in is in the text node which is the first child of that node.
  	if (hasChildNodes(textNode)) then
		str = getData(getFirstChild(textNode))
	else
		!write(0,*) 'XML Warning: empty element ',trim(xmlName)
		return
	end if

  end function getString

! *****************************************************************
! Get the text node from the first child element
! of domNode with tag xmlName. Store the result as a real value.

  function getReal(domNode, xmlName, attrName, attrValue) result (value)
	type(Node), pointer             :: domNode
    character(len=*), intent(in)    :: xmlName
    character(len=*), intent(in), optional  :: attrName
    character(len=*), intent(in), optional  :: attrValue
	character(len=80)               :: str
 	real(8)                         :: value

	! Initialize output
	value = 0.0d0

	! Make sure the pointer is associated
	if (.not.(associated(domNode))) then
		write(0,*) 'Error: pointer not associated in getStringAttr'
		return
	end if

	if (present(attrValue)) then
		str = getString(domNode, xmlName, attrName, attrValue)
	else if (present(attrName)) then
		str = getString(domNode, xmlName, attrName)
	else
		str = getString(domNode, xmlName)
	end if

	if (len_trim(str)>0) then
		if ((index(str,'e')>0).or.(index(str,'E')>0)) then
			read(str, '(e16.5)') value
		else
			if (index(str,'.')==0) then
				str = trim(str)//'.'
			end if
			read(str, '(f9.6)') value
		end if
	else
		write(0,*) 'XML Error: unable to convert the text node from the element ',trim(xmlName),' to a real value'
	end if

  end function getReal


! *****************************************************************
! Get the text node from the first child element
! of domNode with tag xmlName. Store the result as an integer.

  function getInteger(domNode, xmlName, attrName, attrValue) result (value)
	type(Node), pointer             :: domNode
    character(len=*), intent(in)    :: xmlName
    character(len=*), intent(in), optional  :: attrName
    character(len=*), intent(in), optional  :: attrValue
 	character(len=80)               :: str
 	integer                         :: value

	! Initialize output
	value = 0

	! Make sure the pointer is associated
	if (.not.(associated(domNode))) then
		write(0,*) 'Error: pointer not associated in getStringAttr'
		return
	end if

	if (present(attrValue)) then
		str = getString(domNode, xmlName, attrName, attrValue)
	else if (present(attrName)) then
		str = getString(domNode, xmlName, attrName)
	else
		str = getString(domNode, xmlName)
	end if

	if (len_trim(str)>0) then
		read(str, '(i8)') value
	else
		write(0,*) 'XML Error: unable to convert the text node from the element ',trim(xmlName),' to an integer'
	end if

  end function getInteger

end module parse_dom
