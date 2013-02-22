module z_write

  use global

  implicit none
  private

  integer                        :: zfile
  character(len=120)             :: temp
  integer                        :: ios,i,j

  save   :: zfile

  public :: initialize_z_output
  public :: write_z_header
  public :: write_z_channels
  public :: write_z_period
  public :: end_z_output

contains

  subroutine initialize_z_output(fname)
     character(len=*), intent(in) :: fname
     integer			  :: ios
     character(20)		  :: str

     zfile=56
     open (unit=zfile,file=fname,status='unknown',iostat=ios)

     if(ios/=0) then
        write(0,*) 'Error opening file:', fname
     endif

  end subroutine initialize_z_output


  subroutine write_z_header(sitename, Site, Info, header1, header2)
    character(len=80), intent(in)    :: sitename
    type(Site_t),  intent(in)        :: Site
	type(UserInfo_t), intent(in)     :: Info
	character(len=80), optional, intent(in)  :: header1, header2
	real(8)							 :: lon, lat

	if (.not. present(header1)) then
    	write (zfile,*) 'TRANSFER FUNCTIONS IN MEASUREMENT COORDINATES'
    else
    	write (zfile,*) trim(header1)
	end if

	if (.not. present(header2)) then
    	write (zfile,*) '********* WITH FULL ERROR COVARIANCE ********'
    else
    	write (zfile,*) trim(header2)
	end if

!...  write header information
!...  declination does not mean what you think it does:
!...  it is set to zero if channel orientations are correct.

    write (zfile,'(a80)') Info%RemoteRefType
    write (zfile,'(a12,a80)') 'station    :', sitename

	lon = Site%Location%lon
	lat = Site%Location%lat
    if(lon < 0.0d0) then
		lon = lon + 360.0d0
	end if

    write(zfile,105) lat, lon, 0.0d0
    write(zfile,110) nch,nf

100   format('station    : ',a20)
105   format('coordinate ',f9.3,1x,f9.3,1x,'declination ',f8.2)
110   format('number of channels ',i3,2x,' number of frequencies',i4)

  end subroutine write_z_header


  subroutine write_z_channels(sitename, Input, Output)
  	character(len=*), intent(in)   :: sitename
    type(Channel_t), dimension(:), intent(in) :: Input
    type(Channel_t), dimension(:), intent(in) :: Output
    character(len=3)               :: temp
    integer                        :: num

    write(zfile,*) 'orientations and tilts of each channel '

	temp = sitename(1:3)

    do i=1,2
       num = i
       write (zfile,115) num, Input(i)%orientation, Input(i)%tilt, temp, Input(i)%ID
    end do

    do i=1,nch-2
       num = i+2
       write (zfile,115) num, Output(i)%orientation, Output(i)%tilt, temp, Output(i)%ID
    end do

    write (zfile,*) ! write empty line at the end

115   format(i5,1x,f8.2,1x,f8.2,1x,a4,2x,a6)

  end subroutine write_z_channels


  subroutine write_z_period(F, TF, TFVar, InvSigCov, ResidCov)
    type(FreqInfo_t),           intent(in)   :: F
    complex(8), dimension(:,:), intent(in)   :: TF
    real(8),    dimension(:,:), intent(in)   :: TFVar
    complex(8), dimension(:,:), intent(in)   :: InvSigCov
    complex(8), dimension(:,:), intent(in)   :: ResidCov
    real(8)           :: period
    !real              :: sampling_freq

    !sampling_freq = 0.0

    if (trim(F%info_type)=='period') then
    	period = F%value
    else
    	period = 1.0d0/F%value
    end if

    write (zfile,121) period
    !write (zfile,125) F%num_points, sampling_freq
    write (zfile,126) F%num_points

!...        TF (impedance + tipper ... emap dipole etc)
    write(zfile,*) 'Transfer Functions'
    do i=1,nch-2
       do j=1,2
          write (zfile,'(2e12.4)',iostat=ios,advance='no') TF(i,j)
       end do
       write (zfile,*)
    end do

!...        SIGMA_S^-1 (inverse coherent signal power matrix)
    write(zfile,*) 'Inverse Coherent Signal Power Matrix'
    do i=1,2
       do j=1,i
          write (zfile,'(2e12.4)',iostat=ios,advance='no') InvSigCov(i,j)
       end do
       write (zfile,*)
    end do

!...        RESIDUAL COVARIANCE
    write(zfile,*) 'Residual Covariance'
    do i=1,nch-2
       do j=1,i
          write (zfile,'(2e12.4)',iostat=ios,advance='no') ResidCov(i,j)
       end do
       write (zfile,*)
    end do

120   format('period : ',f12.5,3x,' decimation level ',i3,3x, &
             ' freq. band from ',i4,' to ',i4)
121   format('period : ',f12.5)
125   format('number of data point ',i6,' sampling freq. ',f7.3,' Hz')
126   format('number of data point ',i6)
140   format(16e12.4)

  end subroutine write_z_period


  subroutine end_z_output

    close(zfile)
  end subroutine end_z_output

end module z_write
