module shr_lake_coupling_mod

  !========================================================================
  ! Module for handling passing of lake surface water balance from
  ! land to river models.
  !========================================================================

  use ESMF         , only : ESMF_VMGetCurrent, ESMF_VM, ESMF_VMGet
  use ESMF         , only : ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU, ESMF_SUCCESS
  use shr_sys_mod  , only : shr_sys_abort
  use shr_log_mod  , only : s_logunit => shr_log_Unit
  use shr_kind_mod , only : r8 => shr_kind_r8
  use shr_nl_mod   , only : shr_nl_find_group_name
  use shr_mpi_mod  , only : shr_mpi_bcast

  implicit none
  private

  ! !PUBLIC MEMBER FUNCTIONS
  public :: shr_lake_coupling_readnl       ! Read namelist

  character(len=*), parameter :: &
       u_FILE_u=__FILE__

!====================================================================================
CONTAINS
!====================================================================================

  subroutine shr_lake_coupling_readnl(NLFilename, lake_nfields )

    !========================================================================
    ! reads lake_coupling_nl namelist and sets up driver list of fields for
    ! land -> river communications.
    !========================================================================

    ! input/output variables
    character(len=*), intent(in)  :: NLFilename     ! Namelist filename
    integer         , intent(out) :: lake_nfields   ! Number of lake fields to pass

    !----- local -----
    type(ESMF_VM) :: vm                     ! ESMF virtual machine to get MPI communicator
    integer       :: i                      ! Indices
    integer       :: unitn                  ! namelist unit number
    integer       :: ierr                   ! error code
    logical       :: exists                 ! if file exists or not
    integer       :: rc                     ! Error code
    integer       :: localpet               ! Local processor rank
    integer       :: mpicom                 ! MPI communicator
    logical       :: flds_lake_surface_water_balance_fields ! If lake fields for surface water balance should be passed
    
    character(*),parameter :: subName = '(shr_lake_coupling_readnl) '
    character(*),parameter :: F00   = "('(shr_lake_coupling_readnl) ',8a)"
    ! ------------------------------------------------------------------

    namelist /lake_coupling_nl/ flds_lake_surface_water_balance_fields

    !-----------------------------------------------------------------------------
    ! Read namelist and figure out the lake field list to pass
    ! First check if file exists and if not, lake fields wont be passed
    !-----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lake_nfields = 0

    !--- Open and read namelist ---
    if ( len_trim(NLFilename) == 0 ) then
       call shr_sys_abort( subName//'ERROR: nlfilename not set' )
    end if

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return 

    call ESMF_VMGet(vm, localPet=localPet, mpiCommunicator=mpicom, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return 

    ! Note the following still needs to be called on all processors since the mpi_bcast is a collective 
    ! call on all the pes of mpicom
    if (localpet==0) then
       inquire( file=trim(NLFileName), exist=exists)
       if ( exists ) then
          open(newunit=unitn, file=trim(NLFilename), status='old' )
          write(s_logunit,F00) 'Read in lake_coupling_nl namelist from: ', trim(NLFilename)
          call shr_nl_find_group_name(unitn, 'lake_coupling_nl', ierr)
          if (ierr == 0) then
             ! Note that ierr /= 0, no namelist is present.
             read(unitn, lake_coupling_nl, iostat=ierr)
             if (ierr > 0) then
                call shr_sys_abort(trim(subName) //'problem reading in lake_coupling_nl namelist')
             endif
          endif
          close( unitn )
       end if
    end if
    call shr_mpi_bcast( flds_lake_surface_water_balance_fields, mpicom )

    if ( flds_lake_surface_water_balance_fields )then
       lake_nfields = 2
    end if

  end subroutine shr_lake_coupling_readnl

end module shr_lake_coupling_mod
