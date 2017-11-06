! **********************************************************************
! This module is written in Sep 2017 to generalize the earlier rotation
! capabilities of EMTF-FCU. (c) A. Kelbert
! **********************************************************************

module rotation

  use global
  use utils

  implicit none
  private

  integer                        :: ios,i,j,k
  integer, save                  :: nchout
  integer, parameter             :: nchin=2

  public :: rotate_channels
  public :: rotate_data


contains

! **********************************************************************
! Generates a general 2x2 rotation matrix that rotates any two horizontal channels from their original
! layout (theta1, theta2) to a right hand orthogonal coordinate system with azimuth theta0.
! Note that if and only if the original layout is also an orthogonal coordinate system, theta2 = theta1 + 90,
! the determinant is 1 and this reduces to the more traditional form.
! To reverse, use rot2inv.

  function rot2( theta1, theta2, theta0 )
    real(8), intent(in)     :: theta1, theta2, theta0
    real(8)                 :: det
    real(8), dimension(2,2) :: rot2

    call identity(rot2)

    rot2(1,1) = cos(D2R*(theta1 - theta0))
    rot2(1,2) = cos(D2R*(theta2 - theta0))
    rot2(2,1) = sin(D2R*(theta1 - theta0))
    rot2(2,2) = sin(D2R*(theta2 - theta0))

  end function rot2

! **********************************************************************
! Generates the inverse of a general 2x2 rotation matrix that rotates any two horizontal channels
! back to their original layout (theta1, theta2) from a right hand orthogonal coordinate system with azimuth theta0.
! Same as matinv2( rot2(theta1, theta2, theta0) ).

  function rot2inv( theta1, theta2, theta0 )
    real(8), intent(in)     :: theta1, theta2, theta0
    real(8), dimension(2,2) :: rot2inv
    real(8)                 :: det

    call identity(rot2inv)
    det = sin(D2R*(theta2 - theta1))

    rot2inv(1,1) = sin(D2R*(theta2 - theta0))/det
    rot2inv(1,2) = - cos(D2R*(theta2 - theta0))/det
    rot2inv(2,1) = - sin(D2R*(theta1 - theta0))/det
    rot2inv(2,2) = cos(D2R*(theta1 - theta0))/det

  end function rot2inv

! **********************************************************************
! Sets up the rotation matrices for input and output channels, respectively,
! for a general EM data type. To be used to rotate from (and to) an original
! site layout, which doesn't have to be orthogonal. Use this with EDI files
! when ROT=NONE and with general Z-files.
! Implicit assumptions: if a single output channel is found, do not rotate.
! Otherwise, rotate pairwise. Azimuth defaults to zero (geographic coordinates).
! If azimuth is specified, rotates to a right hand orthogonal coordinate system
! defined by that azimuth. If fwdORinv = 'INV', rotates back from the orthogonal
! coordinate system to the original site layout.
! Assumes 2 input channels, Hx and Hy, always (they don't need to be orthogonal,
! but they are horizontal).
! NOTE: The channels themselves are NOT rotated. We keep the original orientations
!       for reference, we might want to go back to these later.
!       If we want them rotated (e.g., for Z-files), there's a separate routine.
  subroutine setup_rotation_site_layout(U, V, Input, Output, theta0, fwdORinv)
    type(Channel_t), dimension(:), intent(in)      :: Input
    type(Channel_t), dimension(:), intent(in)      :: Output
    real(8),         dimension(:,:), intent(inout), allocatable   :: U, V ! rotation matrices
    real(8),         optional, intent(in)          :: theta0 ! new angle to geographic North
    character(3),    optional, intent(in)          :: fwdORinv ! 'FWD' or 'INV'
    ! local variables
    real(8),         dimension(2)                  :: itheta, ithetanew ! orientations for input channels
    real(8),         dimension(:), allocatable     :: otheta, othetanew ! orientations for output channels
    logical                                        :: inverse
    integer                                        :: i, j, istat, nchout

    inverse = .false.
    if (present(fwdORinv)) then
        if (fwdORinv .eq. 'INV') then
            inverse = .true.
        end if
    end if

    ! create the rotation matrix for the two input channels first;
    ! assuming Hx and Hy are the input channels, - pretty much always the case
    itheta(1) = Input(1)%orientation; ithetanew(1) = theta0
    itheta(2) = Input(2)%orientation; ithetanew(2) = theta0 + 90.0
    allocate(U(2,2), stat = istat)
    if (.not. inverse) then
        U = transpose(rot2inv(itheta(1),itheta(2),theta0))
    else
        U = transpose(rot2(itheta(1),itheta(2),theta0))
    end if

    ! now do output channels;
    ! we rotate output channels two at a time, and we implicitly ignore the vertical magnetic field
    nchout = size(Output)
    allocate(otheta(nchout), othetanew(nchout), stat = istat)
    allocate(V(nchout,nchout), stat = istat)
    call identity(V)
    if (nchout == 1) then
        V = 1.0
    else if (mod(nchout,2) == 0) then
        do i=1,nchout,2
           otheta(i) = Output(i)%orientation; othetanew(i) = theta0
           otheta(i+1) = Output(i+1)%orientation; othetanew(i+1) = theta0 + 90.0
           if (.not. inverse) then
              V(i:i+1,i:i+1) = rot2(otheta(i),otheta(i+1),theta0)
           else
              V(i:i+1,i:i+1) = rot2inv(otheta(i),otheta(i+1),theta0)
           end if
        end do
    else
        write(*,*) 'Unable to create rotation matrices: channels need to come in pairs...'
        stop
    end if

    deallocate(otheta, othetanew, stat=istat)

  end subroutine setup_rotation_site_layout


! **********************************************************************
! Sets up the rotation matrices for input and output channels, respectively,
! for a general EM data type. Use this when a site has already been rotated
! to a given orthogonal coordinate system oriented to old_azimuth relative
! to geographic North. This rotates it to new_azimuth relative to geographic North.
! Implicit assumptions: if a single output channel is found, do not rotate.
! Otherwise, rotate pairwise.
! Assumes 2 orthogonal and horizontal input channels, Hx and Hy, always.
  subroutine setup_rotation_orthogonal(U, V, nchout, old_azimuth, new_azimuth)
    integer,         intent(in)                    :: nchout
    real(8),         dimension(:,:), intent(inout), allocatable   :: U, V ! rotation matrices
    real(8),         intent(in)                    :: old_azimuth ! old angle to geographic North
    real(8),         intent(in)                    :: new_azimuth ! new angle to geographic North
    ! local variables
    integer                                        :: i, j, istat

    ! create the rotation matrix for the two input channels first;
    ! assuming Hx and Hy are the input channels, - pretty much always the case
    allocate(U(2,2), stat = istat)
    U = transpose(rot2inv(old_azimuth,old_azimuth + 90.0,new_azimuth))

    ! now do output channels;
    ! we rotate output channels two at a time, and we implicitly ignore the vertical magnetic field
    allocate(V(nchout,nchout), stat = istat)
    call identity(V)
    if (nchout == 1) then
        V = 1.0
    else if (mod(nchout,2) == 0) then
        do i=1,nchout,2
           V(i:i+1,i:i+1) = rot2(old_azimuth,old_azimuth + 90.0,new_azimuth)
        end do
    else
        write(*,*) 'Unable to create rotation matrices: channels need to come in pairs...'
        stop
    end if

  end subroutine setup_rotation_orthogonal

! **********************************************************************
! We rarely need to rotate the channels: for the XML files, we want to keep
! the original site layout so that we could always revert out rotation.
! But in some case, e.g. for Z-files and possibly EDI files, we might want
! to rotate the site layout to match our rotated data. In addition to changing
! the orientation of the channels, we are now updating the relevant instrument
! locations relative to the site center to the new coordinate system.
! Note that no inverse rotation is possible for the channels, so it is not
! supported.
! A.K. NOTE AS OF 8/14/2017: we should also note that in the original version of the code,
! we were only changing the orientations of the channels; we were NOT updating the relevant
! electrode locations relative to site center
! therefore, when applying to EDI files, even SPECTRA, be very careful when you archive the locations
! of the electrodes for rotated sites - make sure that it is clear exactly what is being archived.
  subroutine rotate_channels(Input, Output, theta0)
    type(Channel_t), dimension(:), intent(inout)   :: Input
    type(Channel_t), dimension(:), intent(inout)   :: Output
    real(8),         intent(in)                    :: theta0 ! new azimuth relative to geographic North
    ! local variables
    real(8),         dimension(2)                  :: itheta, ithetanew ! orientations for input channels
    real(8),         dimension(:), allocatable     :: otheta, othetanew ! orientations for output channels
    integer                                        :: i, j, istat, nchout

    ! assuming Hx and Hy are the input channels, - pretty much always the case
    itheta(1) = Input(1)%orientation; ithetanew(1) = theta0
    itheta(2) = Input(2)%orientation; ithetanew(2) = theta0 + 90.0

    ! define the rotated input channel orientations and site layout
    do i=1,size(Input)
        Input(i)%orientation = ithetanew(i)
        Input(i)%X = Input(i)%X * cos(D2R*(itheta(1) - theta0))
        Input(i)%Y = Input(i)%Y * sin(D2R*(itheta(2) - theta0))
        Input(i)%X2 = Input(i)%X2 * cos(D2R*(itheta(1) - theta0))
        Input(i)%Y2 = Input(i)%Y2 * sin(D2R*(itheta(2) - theta0))
    end do

    ! now do output channels;
    ! we rotate output channels two at a time, and we implicitly ignore the vertical magnetic field
    nchout = size(Output)
    allocate(otheta(nchout), othetanew(nchout), stat = istat)
    if (nchout == 1) then
        ! do nothing
        othetanew(1) = Output(i)%orientation
    else if (mod(nchout,2) == 0) then
        do i=1,nchout,2
           otheta(i) = Output(i)%orientation; othetanew(i) = theta0
           otheta(i+1) = Output(i+1)%orientation; othetanew(i+1) = theta0 + 90.0
        end do
    else
        write(*,*) 'Unable to rotate output channels: channels need to come in pairs...'
        stop
    end if

    ! define the rotated output channel orientations and site layout
    do i=1,size(Output)
        Output(i)%orientation = othetanew(i)
        Output(i)%X = Output(i)%X * cos(D2R*(otheta(1) - theta0))
        Output(i)%Y = Output(i)%Y * sin(D2R*(otheta(2) - theta0))
        Output(i)%X2 = Output(i)%X2 * cos(D2R*(otheta(1) - theta0))
        Output(i)%Y2 = Output(i)%Y2 * sin(D2R*(otheta(2) - theta0))
    end do

    deallocate(otheta, othetanew, stat=istat)

  end subroutine rotate_channels


! **********************************************************************
! Rotates one data type, all frequencies.
! if orthogonalORsitelayout .eq. 'orthogonal', rotate to an orthogonal coordinate system defined by theta0.
! if orthogonalORsitelayout .eq. 'sitelayout', rotate to the original site layout defined by channels.
! Site layout does not need to be orthogonal. Done frequency by frequency to cover all bases.
! Can deal with the full covariance, or in its absence rotate the variances only (BAD WAY TO DO THIS!)
! Currently, not computing or recomputing the following: Cov, Coh, MultCoh, SigAmp, SigNoise.
  subroutine rotate_data(Data, Input, Output, orthogonalORsitelayout, theta0)
    type(Data_t),    intent(inout)                 :: Data
    type(Channel_t), dimension(:), intent(in)      :: Input
    type(Channel_t), dimension(:), intent(in)      :: Output
    character(10),   intent(in)                    :: orthogonalORsitelayout
    real(8),         intent(in), optional          :: theta0 ! new angle to geographic North
    ! local variables
    real(8),         dimension(:,:), allocatable   :: U ! rotation matrix size 2x2 for input channels
    real(8),         dimension(:,:), allocatable   :: V ! rotation matrix size (nch-2)x(nch-2) for output channels
    complex(8),      dimension(:,:), allocatable   :: TF ! dimensions (nch-2)x2
    real(8),         dimension(:,:), allocatable   :: TFVar ! dimensions (nch-2)x2
    complex(8),      dimension(:,:), allocatable   :: S ! inverse signal covariance for input channels dimensions 2x2
    complex(8),      dimension(:,:), allocatable   :: N ! residual covariance for output channels dimensions (nch-2)x(nch-2)
    real(8)                                        :: old_azimuth, new_azimuth
    integer                                        :: i, j, k, istat, nchin, nchout

    ! First figure out what it is that we're doing:
    ! if orthogonalORsitelayout .eq. 'orthogonal', rotate to an orthogonal coordinate system defined by theta0.
    ! if orthogonalORsitelayout .eq. 'sitelayout', rotate to the original site layout defined by channels.
    ! if Data%orthogonal, use Data%Rot (frequency by frequency) to rotate to new_azimuth.
    ! if .not. Data%orthogonal, use site layout from channels orientation to rotate to new_azimuth.

    if (orthogonalORsitelayout .eq. 'orthogonal') then
        if (present(theta0)) then
            new_azimuth = theta0
        else
            write(0,*) 'ERROR: unable to rotate to an orthogonal site layout - new azimuth is not provided'
            stop
        end if
    end if

    nchin = Data%nchin
    nchout = Data%nchout
    if (nchin .ne. 2) then
       write(0,*) 'ERROR: our rotation is not setup for anything but two input channels, Hx and Hy'
       stop
    end if
    allocate(U(2,2), V(nchout,nchout), TF(nchout,2), TFVar(nchout,2), S(2,2), N(nchout,nchout), STAT=istat)

    ! rotate the transfer functions and covariance matrices (using * gives strange results!)
    ! if Data%orthogonal, overwrite out general rotation matrices with something that uses
    ! Data%Rot, frequency by frequency, to accomodate old principal axis rotation files.
    ! Otherwise, ignore Data%Rot.
    ! At the risk of unnecessarily recomputing the matrices at every frequency, we opt for generality
    ! and keep this code within the frequency loop.
    do k = 1,Data%nf

        ! Initialize with rotation matrices that use the site layout OR Data%Rot
        if (orthogonalORsitelayout .eq. 'orthogonal') then
            if (Data%orthogonal) then
                ! use Data%Rot (frequency by frequency) to rotate all to new azimuth
                old_azimuth = Data%Rot(k)
                call setup_rotation_orthogonal(U, V, nchout, old_azimuth, new_azimuth)
            else
                ! ignore Data%Rot and rotate from original site layout to new azimuth
                call setup_rotation_site_layout(U, V, Input, Output, new_azimuth)
            end if
            Data%Rot(k) = new_azimuth
        else if (orthogonalORsitelayout .eq. 'sitelayout') then
            if (Data%orthogonal) then
                ! use Data%Rot (frequency by frequency) to rotate to original site layout.
                old_azimuth = Data%Rot(k)
                call setup_rotation_site_layout(U, V, Input, Output, old_azimuth,'INV')
            else
                write(0,*) 'ERROR: the data are not in orthogonal coordinates; unable to rotate back to site layout'
                stop
            end if
        else
            write(0,*) 'ERROR: the data are not in orthogonal coordinates; unable to rotate back to site layout'
            stop
        end if

        TF = Data%Matrix(k,:,:)
        TF = matmul(V, matmul(TF,transpose(U)))
        Data%Matrix(k,:,:) = TF

        if (.not. Data%fullcov) then
            ! In the absence of covariance matrices, rotate the variances matrix just like we rotate the TF
            ! and let's be clear that this is not the right way to do it!
            TFVar = Data%Var(k,:,:)
            TFVar = matmul(V, matmul(TFVar,transpose(U)))
            Data%Var(k,:,:) = TFVar

        else
            S = Data%InvSigCov(k,:,:)
            S = matmul(U, matmul(S,transpose(U)))
            Data%InvSigCov(k,:,:) = S
            N = Data%ResidCov(k,:,:)
            N = matmul(V, matmul(N,transpose(V)))
            Data%ResidCov(k,:,:) = N

            ! finally, update the variances
            ! A.K. NOTE AS OF 8/14/2017: the variance of the real or imaginary component
            ! would be this divided by 2. However, p 52 of the EDI manual explains that e.g. ZXY.VAR
            ! refers to the complex variance (while ZXYR.VAR would mean the variance of the real part).
            ! To be consistent with this definition, we are no longer dividing by two.
            ! This was fixed in the reading routine, but not here, so are now fixing the XML files
            ! in the database.
            do i=1,nchout
               do j=1,nchin
                  TFVar(i,j) = (N(i,i)*S(j,j))
               end do
            end do
            Data%Var(k,:,:) = TFVar

        end if

    end do

    ! Now that we've rotated all frequencies, update Data%orthogonal logical
    if (orthogonalORsitelayout .eq. 'orthogonal') then
        Data%orthogonal = .true.
    elseif (orthogonalORsitelayout .eq. 'sitelayout') then
        Data%orthogonal = .false.
    end if

    deallocate(U, V, TF, TFVar, S, N, STAT=istat)

  end subroutine rotate_data


end module rotation
