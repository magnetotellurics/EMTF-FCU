! *****************************************************************************
module list_write
  ! This module is adapted from ModEM DataIO.f90 to read and write a data list.
  ! List format is an intermediate format for input to ModEM; it should not be
  ! used for long-term storage due to lack of metadata.
  ! For this reason, we only provide a adapted data list writing routine here.
  ! For reading, please refer to ModEM 3D_MT code.
  !
  ! This stand-alone module could be included in ModEM and expanded to read
  ! directly from EMTF files using EMTF FCU functionality. Currently it is not
  ! reading from EMTF FCU storage. It is also NOT used by EMTF FCU.
  ! It is also outdated so should only serve as a reference when you start
  ! getting this code to work. *This isn't expected to work out-of-the-box.*
  ! A. Kelbert, July 19, 2024

  use math_constants
  use file_units
  use utilities
  use dataspace
  use transmitters
  use receivers
  use datatypes

  implicit none

  private

  public     :: read_dataVectorMTX, write_dataVectorMTX

  ! this block of information constitutes user preferences about the data format
  character(200),allocatable, private, save :: info_in_file(:)
  character(20), allocatable, private, save :: sign_info_in_file(:)
  integer,       allocatable, private, save :: sign_in_file(:)
  character(20), allocatable, private, save :: units_in_file(:)
  real,          allocatable, private, save :: origin_in_file(:,:) ! (nDt,2)
  real,          allocatable, private, save :: geographic_orientation(:)

Contains

!**********************************************************************
! writes data in the ASCII list data file; it is convenient to work
! with data ordered by site (NOT by frequency), so we are going into
! some pains here to write them out in this order ...

   subroutine write_Z_list(allData,cfile)

    character(*), intent(in)                  :: cfile
    type(dataVectorMTX_t), intent(in)         :: allData
    ! local variables
    integer                         :: nTx,nRx,nDt,ncomp
    integer                         :: countData
    real(8), allocatable            :: value(:) ! (ncomp)
    real(8), allocatable            :: error(:) ! (ncomp)
    logical, allocatable            :: exist(:) ! (ncomp)
    character(2)                    :: temp = '> '
    character(100)                  :: siteid,ref_siteid,compid
    integer                         :: iTx,iRx,iDt,icomp,i,j,k,istat,ios,nBlocks
    real(8)                         :: x(3),ref_x(3), Period,SI_factor,large
    real(8)                         :: lat,lon,ref_lat,ref_lon
    logical                         :: conjugate, isComplex

    large = 2.0e15

    open(unit=ioDat,file=cfile,form='formatted',status='unknown')

    ! For each data type in dictionary, if data of this type exists, write it out.
    WRITE_DATA_TYPE: do iDt = 1,size(typeDict)

      nBlocks = countDataBlock(allData,iDt)
      if (nBlocks == 0) then
        ! no data for this data type; skip it
        cycle WRITE_DATA_TYPE
      else
        ! count the number of transmitters and receivers
        nTx = 0
        nRx = 0
        do iTx = 1,size(txDict)
            if (typeDict(iDt)%tx_index(iTx) > 0) then
                nTx = nTx + 1
            end if
        end do
        do iRx = 1,size(rxDict)
            do iTx = 1,size(txDict)
                if (typeDict(iDt)%rx_index(iTx,iRx) > 0) then
                    nRx = nRx + 1
                    exit
                end if
            end do
        end do
      end if

      ! write the data type header
      call compact(info_in_file(iDt))
      write(ioDat,'(a32)',advance='no') '# ModEM impedance responses for '
      write(ioDat,'(a100)',iostat=ios) info_in_file(iDt)
      write(ioDat,'(a100)',iostat=ios) ImpHeader(iDt)
      call compact(typeDict(iDt)%name)
      write(ioDat,'(a2)',advance='no') temp
      write(ioDat,*,iostat=ios) trim(typeDict(iDt)%name)
      call compact(sign_info_in_file(iDt))
      write(ioDat,'(a2)',advance='no') temp
      write(ioDat,*,iostat=ios) trim(sign_info_in_file(iDt))
      call compact(units_in_file(iDt))
      write(ioDat,'(a2)',advance='no') temp
      write(ioDat,*,iostat=ios) trim(units_in_file(iDt))
      write(ioDat,'(a2,f8.2)',iostat=ios) temp,geographic_orientation(iDt)
      write(ioDat,'(a2,2f8.3)',iostat=ios) temp,origin_in_file(iDt,1),origin_in_file(iDt,2)
      write(ioDat,'(a2,2i6)',iostat=ios) temp,nTx,nRx

      if (sign_in_file(iDt) == ISIGN) then
        conjugate = .false.
      else if (abs(sign_in_file(iDt)) == 1) then
        conjugate = .true.
      end if
      SI_factor = ImpUnits(typeDict(iDt)%units,units_in_file(iDt))

      ncomp = typeDict(iDt)%nComp
      allocate(value(ncomp),error(ncomp),exist(ncomp),STAT=istat)
      isComplex = typeDict(iDt)%isComplex
      countData = 0

      ! write data
      do iRx = 1,size(rxDict)
        do iTx = 1,size(txDict)

            k = typeDict(iDt)%rx_index(iTx,iRx)
            i = typeDict(iDt)%dt_index(iTx)
            j = typeDict(iDt)%tx_index(iTx)
            if (k == 0) then
                cycle
            end if
            value = SI_factor * allData%d(j)%data(i)%value(:,k)
            if (allData%d(j)%data(i)%errorBar) then
                error = SI_factor * allData%d(j)%data(i)%error(:,k)
            else
                error = large
            end if
            exist = allData%d(j)%data(i)%exist(:,k)
            Period = txDict(iTx)%period
            siteid = rxDict(iRx)%id
            x = rxDict(iRx)%x

            select case (iDt)

                case(Full_Impedance,Off_Diagonal_Impedance,Full_Vertical_Components)

                    do icomp = 1,ncomp/2
                        if (.not. exist(2*icomp-1)) then
                            cycle
                        end if
                        compid = typeDict(iDt)%id(icomp)
                        write(ioDat,'(es13.6)',    iostat=ios,advance='no') Period
                        write(ioDat,'(a40,3f12.3)',iostat=ios,advance='no') trim(siteid),x(:)
                        if (conjugate) then
                            write(ioDat,'(a8,3es15.6)',iostat=ios) trim(compid),value(2*icomp-1),-value(2*icomp),error(2*icomp)
                        else
                            write(ioDat,'(a8,3es15.6)',iostat=ios) trim(compid),value(2*icomp-1),value(2*icomp),error(2*icomp)
                        end if
                        countData = countData + 1
                    end do

                case(Full_Interstation_TF)

                    do icomp = 1,ncomp/2
                        if (.not. exist(2*icomp-1)) then
                            cycle
                        end if
                        compid = typeDict(iDt)%id(icomp)
                        ref_siteid = rxDict(iRx)%id_ref
                        ref_x = rxDict(iRx)%r
                        write(ioDat,'(es13.6)',    iostat=ios,advance='no') Period
                        write(ioDat,'(a40,3f12.3)',iostat=ios,advance='no') trim(siteid),x(:)
                        write(ioDat,'(a40,3f12.3)',iostat=ios,advance='no') trim(ref_siteid),ref_x(:)
                        if (conjugate) then
                            write(ioDat,'(a8,3es15.6)',iostat=ios) trim(compid),value(2*icomp-1),-value(2*icomp),error(2*icomp)
                        else
                            write(ioDat,'(a8,3es15.6)',iostat=ios) trim(compid),value(2*icomp-1),value(2*icomp),error(2*icomp)
                        end if
                        countData = countData + 1
                    end do

                case(Off_Diagonal_Rho_Phase,Phase_Tensor)

                    do icomp = 1,ncomp
                        if (.not. exist(icomp)) then
                            cycle
                        end if
                        compid = typeDict(iDt)%id(icomp)
                        ! For apparent resistivities only, log10 of the values was used
			            if (index(compid,'RHO')>0) then
			                value(icomp) = 10**value(icomp)
			                error(icomp) = 10**error(icomp)
			            end if
                        write(ioDat,'(es13.6)',    iostat=ios,advance='no') Period
                        write(ioDat,'(a40,3f12.3)',iostat=ios,advance='no') trim(siteid),x(:)
                        write(ioDat,'(a8,3es15.6)',iostat=ios) trim(compid),value(icomp),error(icomp)
                        countData = countData + 1
                    end do

            end select

        end do  ! transmitters
      end do  ! receivers

      if (output_level > 4) then
        write(0,*) 'Written ',countData,' data values of type ',trim(typeDict(iDt)%name),' to file'
      end if
      deallocate(value, error, exist, STAT=istat)

    end do WRITE_DATA_TYPE ! data types

    close(ioDat)

   end subroutine write_Z_list


end module list_write
