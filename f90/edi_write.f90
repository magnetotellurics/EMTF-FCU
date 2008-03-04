! *****************************************************************************
module edi_write
  !--------------------------------------------------------------------------!
  ! Randie Mackie's wrt_edi.f, substantially modified and converted into F90 !
  ! by Anna Kelbert. Note that integers nf and nch are defined in the module !
  ! global.f90, that is used by this code. In its present form, the module   !
  ! will only work when there are 3 output channels in the input, in the     !
  ! order Hy, Ex, Ey. If more output channels are present, the corresponding !
  ! transfer functions will not be written into the EDI file. All special    !
  ! cases could be easily incorporated into the code, but at the moment it   !
  ! only works for the typical ZXX, ZXY, ZYX, ZYY, TX, TY set of TFs.        !
  ! Date: 1 Nov 2007                                                         !
  !--------------------------------------------------------------------------!

  use global
  use utils

  implicit none
  private

  integer                         :: edifile = 30
  integer                         :: ldataid,lacqby,lyname
  integer                         :: lcdate,lclat,lclong,lcelev,lsectid
  integer                         :: ios,i,j,k,ii,jj,kk,ns
  real                            :: c1,s1,rot,a
  integer                         :: irun=1,istation=100 ! needed to create HMEAS/EMEAS ID's

  public :: write_edi_file

contains

  subroutine write_edi_file(fname,edi_date,sectid,Site, &
  			InputChannel,OutputChannel,F,TF,TFVar, &
  			Info,UserInfo)
  	character(len=*), intent(in)                   :: fname
	character(len=*), intent(in)                   :: edi_date
  	character(len=*), intent(in)                   :: sectid
  	type(Site_t), intent(in)                       :: Site
  	type(UserInfo_t), optional, intent(in)         :: UserInfo
  	type(RemoteRef_t), intent(in)                  :: Info
  	type(Channel_t), dimension(:), intent(in)      :: InputChannel
  	type(Channel_t), dimension(:), intent(in)      :: OutputChannel
  	type(FreqInfo_t), dimension(:), intent(in)     :: F
  	complex(8), dimension(:,:,:), intent(in)       :: TF
  	real(8),    dimension(:,:,:), intent(in)       :: TFVar
  	logical                                        :: tipper_present
  	real                                           :: azimuth
	real(8)                                        :: lat, long, elev
	character(len=80)                              :: dataid, source
	character(len=80), dimension(11)               :: info_block
	real(8), dimension(:), allocatable             :: freq
	complex(8), dimension(:,:), allocatable        :: t
	complex(8), dimension(:,:), allocatable        :: z
	real(8), dimension(:,:), allocatable           :: tvar
	real(8), dimension(:,:), allocatable           :: zvar
	
	allocate(freq(nf),t(nf,2),z(nf,(nch-3)*2),tvar(nf,2),zvar(nf,(nch-3)*2))
	
	freq = 0.0d0
	t = dcmplx(0.0d0,0.0d0)
	z = dcmplx(0.0d0,0.0d0)
	tvar = 0.0d0
	zvar = 0.0d0

	if (nch<5) then
		write(0,*) 'Unable to write the EDI: too few output channels.'
		return
	end if

	do i=1,nf
		if (index(F(i)%info_type,'period')>0) then
			freq(i) = 1.0d0/F(i)%value
		else
			freq(i) = F(i)%value
		end if
	end do
		
	t(:,1) = TF(:,1,1) !TX
	tvar(:,1) = TFVar(:,1,1)

	t(:,2) = TF(:,1,2) !TY
	tvar(:,2) = TFVar(:,1,2)
	
	z(:,1) = TF(:,2,1) !ZXX
	zvar(:,1) = TFVar(:,2,1)
	
	z(:,2) = TF(:,2,2) !ZXY
	zvar(:,2) = TFVar(:,2,2)

	z(:,3) = TF(:,3,1) !ZYX
	zvar(:,3) = TFVar(:,3,1)

	z(:,4) = TF(:,3,2) !ZYY
	zvar(:,4) = TFVar(:,3,2)
	
	dataid = Site%ID
	lat = Site%Location%lat
	long = Site%Location%lon
	elev = Site%Location%elev
	tipper_present = .true. ! assume tipper is always present
	azimuth = InputChannel(1)%orientation ! orientation of Hx
	if (present(UserInfo)) then
		source = UserInfo%Source
	else
		source = 'UNKNOWN'
	end if
	
	! create a block of additional information
	if (present(UserInfo)) then
	    write(info_block(1),*) 'PROJECT=',trim(UserInfo%Project)
		write(info_block(2),*) 'EXPERIMENT=',trim(UserInfo%Experiment)
		write(info_block(3),*) 'YEAR=',UserInfo%YearCollected	
		write(info_block(4),*) 'PROCESSEDBY=',trim(UserInfo%Source)
		write(info_block(5),*) 'PROCESSINGSOFTWARE=',trim(UserInfo%ProcessingSoftware)
	else
	    write(info_block(1),*) 'PROJECT=UNKNOWN'
		write(info_block(2),*) 'EXPERIMENT=UNKNOWN'
		write(info_block(3),*) 'YEAR=UNKNOWN'
		write(info_block(4),*) 'PROCESSEDBY=UNKNOWN'
		write(info_block(5),*) 'PROCESSINGSOFTWARE=UNKNOWN'
	end if	
	write(info_block(6),*) 'PROCESSINGID=',trim(Info%processing_id)
	write(info_block(7),*) 'SITENAME=',trim(Site%Description)
	write(info_block(8),*) 'RUNLIST=',trim(Site%RunList)
	write(info_block(9),*) 'REMOTEREF=',trim(Info%remote_ref_type)
	write(info_block(10),*) 'REMOTESITE=',trim(Info%remote_site_id)
	write(info_block(11),*) 'SIGNCONVENTION=',trim(Info%sign_convention)
	
	call wrt_edi(fname,dataid,sectid,source,info_block, &
			freq,zvar,tvar,z,t,lat,long,elev, &
			edi_date,tipper_present,azimuth)
			
	deallocate(freq,t,z,tvar,zvar)

  end subroutine write_edi_file

!--------------------------------------------------------------------------!
! Randie Mackie's code substantially modified and converted into valid F90 !
! by Anna Kelbert. Note that integers nf and nch are defined in the module !
! global.f90, that is used by this code.                                   !
! Date: 1 Nov 2007                                                         !
!--------------------------------------------------------------------------!
  subroutine wrt_edi(fname,dataid,sectid,source,info, &
  						 f,v,vt,z,t,lat,long,elev, &
                         cdate,ltip,azm)

	  character(80), intent(in)                      :: fname, dataid, sectid, source
	  character(80), dimension(:), intent(in)        :: info !goes into the info block
      real(8), dimension(nf), intent(in)             :: f !list of frequencies
      real(8), dimension(nf,4), intent(in)           :: v !variances for impedances
      real(8), dimension(nf,2), intent(in)           :: vt !variances for the tipper
      complex(8), dimension(nf,4), intent(in)        :: z !impedances
      complex(8), dimension(nf,2), intent(in)        :: t !tippers
      real(8), intent(in)                            :: lat, long, elev !location
      character(9), intent(in)                       :: cdate !character date
      logical, intent(in)                            :: ltip !logical: is tipper defined?
	  real, intent(in)                               :: azm ! azimuth
!
! write out an EDI file with the results
!
	  real(8) fs(nf), vs(nf,4), vts(nf,2)
	  complex(8) zs(nf,4), ts(nf,2)
      integer idx(nf)
      logical empty
      real sid(10)
      character*16 acqby, yname
      character*16 clat,clong,celev
      integer ex_x1,ex_x2,ex_y1,ex_y2,ey_x1,ey_x2,ey_y1,ey_y2
      real site_x, site_y, site_z

!=======================================================================
!
!    sort data into ascending frequency order
!
	    call sortidx(nf,f,idx)
	     
	    fs = f(idx)	
	    zs = z(idx,:)
	    vs = v(idx,:)
	    ts = t(idx,:)
	    vts = vt(idx,:)

!
! write out EDI file
!
!-----------------------------------------------------------------------
!     HEAD block

!      print*,'enter name for output edi file for station ',k
!      read(*,'(a)') fname
!      print*,'enter dataid string'
!      read(*,'(a)') dataid
!      open (edifile,file=fname)

      open (unit=edifile,file=fname,iostat=ios)   
     
      if(ios/=0) then
         write(0,*) 'Error opening file:', fname
      endif

      write(edifile,*)'>HEAD'

!-DATAID
      ldataid=16
      call trmstr(dataid,ldataid,empty)
      ldataid = min( ldataid, 16)
      write(edifile,*) 'DATAID="', dataid(1:ldataid), '"'
!-ACQBY
      acqby=source
      lacqby=16
      call trmstr(acqby,lacqby,empty)
      lacqby = min( lacqby, 16)
      write(edifile,*) 'ACQBY="', acqby(1:lacqby), '"'
!-FILEBY
      yname='z2edi'
      lyname=16
      call trmstr(yname,lyname,empty)
      lyname = min( lyname, 16)
      write(edifile,*) 'FILEBY="', yname(1:lyname), '"'
!-FILEDATE
      lcdate=16
      call trmstr(cdate,lcdate,empty)
      lcdate = min( lcdate, 16)
      write(edifile,*) 'FILEDATE=', cdate(1:lcdate)
!-LAT
!-LONG
!-ELEV
      clat=deg2char(lat)
      clong=deg2char(long)
      write(celev,'(i4)') int(elev)

      lclat=16
      lclong=16
      lcelev=16
      call trmstr( clat,  lclat,  empty)
      call trmstr( clong, lclong, empty)
      call trmstr( celev, lcelev, empty)
      
      write(edifile,*) 'LAT=', clat(1:lclat)
      write(edifile,*) 'LONG=', clong(1:lclong)
      write(edifile,*) 'ELEV=', celev(1:lcelev)
!-STDVERS
      write(edifile,*) 'STDVERS=SEG 1.0'
!-PROGVERS
      write(edifile,*) 'PROGVERS="EMTF File Conversion Utilities 1.0"'
!-PROGDATE
      write(edifile,*) 'PROGDATE=11/02/07'
!-MAXSECT
      write(edifile,*) 'MAXSECT=999'
!-EMPTY
      write(edifile,*) 'EMPTY=1.0e+32'
	      
!-----------------------------------------------------------------------
!     INFO block

      write(edifile,*)' '
      write(edifile,*) '>=INFO'
      write(edifile,*) 'MAXINFO=999'
      
      do j=1,size(info)
		write(edifile,*) info(j)
	  end do

!-----------------------------------------------------------------------
!     DEFINEMEAS block

      write(edifile,*)' '
      write(edifile,*) '>=DEFINEMEAS'

      write(edifile,*) 'MAXCHAN=7'
      write(edifile,*) 'MAXRUN=999'
      write(edifile,*) 'MAXMEAS=9999'
      write(edifile,*) 'UNITS=M'
      write(edifile,*) 'REFTYPE=CART'

      write(edifile,*) 'REFLAT=', clat(1:lclat)
      write(edifile,*) 'REFLONG=', clong(1:lclong)
      write(edifile,*) 'REFELEV=', celev(1:lcelev)

!-----------------------------------------------------------------------
!     EMEAS & HMEAS blocks for each site

      site_x=0.0
      site_y=0.0
      site_z=0.0

! -----get ex coordinates x1, x2, and y1, y2 for "L" array
!        c1=cos(azm/57.2958)
!        s1=sin(azm/57.2958)
!        ex_x1=0
!        ex_x2=int(exl*c1)
!        ex_y1=0
!        ex_y2=int(exl*s1)
! -----get ey coordinates
!        ey_x1=0
!        ey_x2=int(eyl*s1)
!        ey_y1=0
!        ey_y2=int(eyl*c1)

! -----station id

        do j=1,7
          sid(j) = 10.0*float(istation)+j+float(irun)/1000.
        end do
!
! ----WRITE OUT THE MEASUREMENT IDS, STARTING WITH HX,HY,HZ
 3000   FORMAT('>HMEAS ID=',F8.3,' CHTYPE=Hx',' X=',F3.1,' Y=',F3.1,' Z=',F3.1, &
                 ' AZM=',F5.1)
 3100   FORMAT('>HMEAS ID=',F8.3,' CHTYPE=Hy',' X=',F3.1,' Y=',F3.1,' Z=',F3.1, &
               ' AZM=',F5.1)
 3200   FORMAT('>HMEAS ID=',F8.3,' CHTYPE=Hz',' X=',F3.1,' Y=',F3.1,' Z=',F3.1, &
                 ' AZM=',F5.1)
        write (edifile,*) ' '
        WRITE (edifile,3000) sid(1),site_X,site_Y,site_Z,AZM
        WRITE (edifile,3100) sid(2),site_X,site_Y,site_Z,AZM+90
        WRITE (edifile,3200) sid(3),site_X,site_Y,site_Z,0.
!
! ----ADD MEASIDS FOR EX AND EY
! 3300   FORMAT('>EMEAS ID=', F8.3, ' CHTYPE=Ex',' X=',I7,' Y=',I7, &
!                 ' X2=',I7,' Y2=',I7)
! 3400   FORMAT('>EMEAS ID=', F8.3, ' CHTYPE=Ey',' X=',I7,' Y=', I7, &
!                 ' X2=',I7,' Y2=',I7)
!        WRITE (edifile,3300)sid(4),EX_X1,EX_Y1,EX_X2,EX_Y2
!        WRITE (edifile,3400)sid(5),EY_X1,EY_Y1,EY_X2,EY_Y2
 3300   FORMAT('>EMEAS ID=',F8.3,' CHTYPE=Ex',' X=',F3.1,' Y=',F3.1,' Z=',F3.1)
 3400   FORMAT('>EMEAS ID=',F8.3,' CHTYPE=Ey',' X=',F3.1,' Y=',F3.1,' Z=',F3.1)
                 
        WRITE (edifile,3300)sid(4),site_X,site_Y,site_Z
        WRITE (edifile,3400)sid(5),site_X,site_Y,site_Z
!
! ----WRITE OUT IDS FOR REFERENCE CHANNELS
!         WRITE (edifile,3000) sid(6),site_X,site_Y,site_Z,AZM
!         WRITE (edifile,3100) sid(7),site_X,site_Y,site_Z,AZM+90
!-----------------------------------------------------------------------
!     MTSECT blocks for each site

        write (edifile,*) ' '
        write (edifile,'(A8)') '>=MTSECT'

!-SECTID
        lsectid=16
        call trmstr(sectid,lsectid,empty)
        lsectid = min( lsectid, 16)
        write(edifile,*) 'SECTID="', sectid(1:lsectid), '"'

        write (edifile,'(A6,I2 )') 'NFREQ=',nf
        write (edifile,'(A3,F9.3)') 'HX=',sid(1)
        write (edifile,'(A3,F9.3)') 'HY=',sid(2)
        write (edifile,'(A3,F9.3)') 'HZ=',sid(3)
        write (edifile,'(A3,F9.3)') 'EX=',sid(4)
        write (edifile,'(A3,F9.3)') 'EY=',sid(5)

!
!-write out mt section
!
        rot=AZM
        write(edifile,*) ' '
		write(edifile,*) '>!****FREQUENCIES****!'
        WRITE(edifile,'(A8,I2)') '>FREQ //',nf
        WRITE(edifile,50) (Fs(I),I=nf,1,-1)

        write(edifile,*)' '
		write(edifile,*) '>!****IMPEDANCE ROTATION ANGLES****!'
        WRITE(edifile,'(A8,I2)') '>ZROT //',nf
        WRITE(edifile,50) (rot,I=nf,1,-1)

        write(edifile,*)' '
		write(edifile,*) '>!****IMPEDANCES****!'
        WRITE(edifile,'(A18,I2)') '>ZXXR ROT=ZROT //',nf
        WRITE(edifile,50) (REAL(Zs(I,1)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A18,I2)') '>ZXXI ROT=ZROT //',nf
        WRITE(edifile,50) (AIMAG(Zs(I,1)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A22,I2)') '>ZXX.VAR ROT=ZROT //',nf
        WRITE(edifile,50) (Vs(I,1),I=nf,1,-1)

        write (edifile,*) ' '
        WRITE(edifile,'(A18,I2)') '>ZXYR ROT=ZROT //',nf
        WRITE(edifile,50) (REAL(Zs(I,2)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A18,I2)') '>ZXYI ROT=ZROT //',nf
        WRITE(edifile,50) (AIMAG(Zs(I,2)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A22,I2)') '>ZXY.VAR ROT=ZROT //',nf
        WRITE(edifile,50) (Vs(I,2),I=nf,1,-1)

        write (edifile,*) ' '
        WRITE(edifile,'(A18,I2)') '>ZYXR ROT=ZROT //',nf
        WRITE(edifile,50) (REAL(Zs(I,3)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A18,I2)') '>ZYXI ROT=ZROT //',nf
        WRITE(edifile,50) (AIMAG(Zs(I,3)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A22,I2)') '>ZYX.VAR ROT=ZROT //',nf
        WRITE(edifile,50) (Vs(I,3),I=nf,1,-1)

        write (edifile,*) ' '
        WRITE(edifile,'(A18,I2)') '>ZYYR ROT=ZROT //',nf
        WRITE(edifile,50) (REAL(Zs(I,4)),I =nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A18,I2)') '>ZYYI ROT=ZROT //',nf
        WRITE(edifile,50) (AIMAG(Zs(I,4)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A22,I2)') '>ZYY.VAR ROT=ZROT //',nf
        WRITE(edifile,50) (Vs(I,4),I=nf,1,-1)


      if(ltip) then
        write(edifile,*)' '
        write(edifile,*) '>!****TIPPER PARAMETERS****!'      
        WRITE(edifile,'(A13,I2)') '>TXR.EXP  //',nf
        WRITE(edifile,50) (REAL(Ts(I,1)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A13,I2)') '>TXI.EXP  //',nf
        WRITE(edifile,50) (AIMAG(Ts(I,1)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A15,I2)') '>TXVAR.EXP  //',nf
        WRITE(edifile,50) (VTs(I,1),I=nf,1,-1)

        write (edifile,*) ' '
        WRITE(edifile,'(A13,I2)') '>TYR.EXP  //',nf
        WRITE(edifile,50) (REAL(Ts(I,2)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A13,I2)') '>TYI.EXP  //',nf
        WRITE(edifile,50) (AIMAG(Ts(I,2)),I = nf,1,-1)
        write (edifile,*) ' '
        WRITE(edifile,'(A15,I2)') '>TYVAR.EXP  //',nf
        WRITE(edifile,50) (VTs(I,2),I=nf,1,-1)
      end if

      WRITE (edifile,'(A4)') '>END'

50    FORMAT(6es16.6,1X)

      close(edifile)

  end subroutine wrt_edi


! **********************************************************************
  SUBROUTINE TRMSTR(STR,LENGTH,EMPTY)
!
!============================ INTERFACE ================================
!
! WRITTEN BY:        GEOTOOLS Corporation
!                    5808 BALCONES DRIVE, SUITE 202
!                    AUSTIN, TEXAS  78731
!
! DESCRIPTION: RETURNS STR LEFT JUSTIFIED (I.E. NO PRECEDING BLANKS) AND
!              THE POSITION OF THE RIGHTMOST NON-BLANK CHARACTER IN THE
!              STRING (AFTER JUSTIFICATION). BECAUSE THE LENGTH OF A
!              STRING CANNOT BE ZERO, AN EMPTY STRING RETURNS A LENGTH
!              OF 1, BUT THE EMPTY FLAG IS SET TO TRUE. THIS ROUTINE
!              CAN BE USED TO SYNTHESIZE TRIM IN A CONCATINATION AS
!              FOLLOWS:
!
!                  CALL TRMSTR(STR,LENGTH,EMPTY)
!                  IF EMPTY THEN
!                    PRINT '*'//'*'
!                  ELSE
!                    PRINT '*'//STR(1:LENGTH)//'*'
!                  END IF
!
! PARAMETERS:
!   I/O TYPE DIM NAME        DESCRIPTION
!   I/O CHR16  1 STR         STRING TO BE TRIMMED UP TO 80 CHARACTERS
!   I/O INT    1 LENGTH      ON ENTRY, THIS IS THE MAXIMUM LENGTH OF THE
!                            STRING. ON EXIT, IT IS SET TO THE POSITION
!                            OF THE RIGHTMOST NON-BLANK CHARACTER
!   O   LOGIC  1 EMPTY       RETURNS TRUE IF STRING HAS NO NON-BLANK
!                            CHARACTERS
!
! ============= PARAMETER AND LOCAL VARIABLE DECLARATIONS ==============
!
!
!     ---- PASS PARAMETERS
      CHARACTER *(*) STR
      INTEGER LENGTH
      LOGICAL EMPTY
!
!     ---- LOCAL VARIABLES
      CHARACTER*240 TEMP
      INTEGER I,LEFT,RIGHT,OLDLEN
!
! ======================================================================
!
!     ----VALIDATE LENGTH
      IF (LENGTH.GT.240) THEN
        WRITE(*,*) 'STRING TOO LONG PASSED TO TRMSTR.'
      END IF
!
!     ----FIND LEFTMOST NON-BLANK CHAR
      OLDLEN=LENGTH
      DO I=1,LENGTH
        IF (STR(I:I).NE.' ') GOTO 20
      END DO
      I=LENGTH+1
  20  LEFT=I
!
!     ----SCAN LEFT TO RIGHT LOOKING FOR CHR(0) OR CHR(13)
      RIGHT=LENGTH
      DO I=LEFT,LENGTH
        IF ((ICHAR(STR(I:I)).EQ.0).OR.(ICHAR(STR(I:I)).EQ.10)) THEN
          RIGHT=I-1
          GOTO 70
        END IF
      END DO
!
!     ----SCAN RIGHT TO LEFT FOR FIRST NON-BLANK CHARACTER
  70  DO I=RIGHT,1,-1
        IF (STR(I:I).NE.' ')  THEN
          RIGHT=I
          GOTO 50
        END IF
      END DO
!
  50  IF (LEFT.LE.RIGHT) THEN
!
!       ----EXTRACT STRING
        TEMP=STR(LEFT:RIGHT)
        STR(1:OLDLEN)=TEMP
        LENGTH=RIGHT-LEFT+1
        EMPTY=.FALSE.
      ELSE
        STR=' '
        LENGTH=1
        EMPTY=.TRUE.
      END IF
      RETURN
  END SUBROUTINE TRMSTR

! **********************************************************************
! Sort frequencies, and impedances or tippers with variances according 
! to the decreasing frequency order. Written in its original form by
! Randie Mackie, converted to F90 assumed-shape array approach 
! and generalized by Anna Kelbert.
!
! m = 4 if the inputs are impedances; m = 2 if the inputs are tippers. 
!
! Date: 2 Nov 2007

    subroutine piksrt(n,m,arr,z,ze)
      integer, intent(in)                               :: n,m
      real(8), dimension(n), intent(inout)              :: arr !e.g. frequencies
      real(8), dimension(n,m), intent(inout)            :: ze !variances
      complex(8), dimension(n,m), intent(inout)         :: z !impedances or tippers
      complex(8), dimension(m)                          :: b
      real(8), dimension(m)                             :: be
      real(8)                                           :: a
      integer                                           :: i,j,k,ii,jj

      do j=2,n
        a=arr(j)
        do ii=1,m
          b(ii)=z(j,ii)
          be(ii)=ze(j,ii)
        end do
        do i=j-1,1,-1
          if(arr(i)<=a)go to 10
          arr(i+1)=arr(i)
          do k=1,m
          	z(i+1,k)=z(i,k)
          	ze(i+1,k)=ze(i,k)
          end do
		end do
        i=0
10      arr(i+1)=a
	    do k=1,m
	      z(i+1,k)=b(k)
	      ze(i+1,k)=be(k)
	    end do
	    	     write(*,*) 'ok ...',j
	    
      end do

      return
    end subroutine piksrt


! **********************************************************************

    subroutine piksrt_sdm(n,arr,sdm,nsdm)
      real, dimension(n), intent(inout)                 :: arr !e.g. frequencies
      real, dimension(7,7,n), intent(inout)             :: sdm !spectra
      integer, dimension(n), intent(inout)              :: nsdm
      complex, dimension(7,7)                           :: b
      integer                                           :: i,j,ii,jj,n

      do j=2,n
        a=arr(j)
        ns=nsdm(j)
        do ii=1,7
          do jj=1,7
            b(ii,jj)=sdm(ii,jj,j)
          end do
        end do
        do i=j-1,1,-1
          if(arr(i).le.a)go to 10
          arr(i+1)=arr(i)
          nsdm(i+1)=nsdm(i)
          do ii=1,7
            do jj=1,7
              sdm(ii,jj,i+1)=sdm(ii,jj,i)
            end do
          end do
        end do
        i=0
10      arr(i+1)=a
        nsdm(i+1)=ns
        do ii=1,7
          do jj=1,7
            sdm(ii,jj,i+1)=b(ii,jj)
          end do
        end do
      end do

      return
    end subroutine piksrt_sdm

end module edi_write