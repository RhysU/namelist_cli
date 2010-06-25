!> Proof of concept for parsing Fortran command line arguments using
!! \c NAMELIST.  Check out <a href="http://home.comcast.net/~urbanjost/CLONE/KRACKEN/namelist/index.html">
!! Exploring NAMELIST</a> by John S. Urban for the inspiration.  Compared
!! to John's <a href="http://home.comcast.net/~urbanjost/CLONE/KRACKEN/namelist/cmd.bat">most complete version</a>, this hides all of the string
!! processing details within \ref get_command_arguments().

PROGRAM namelist_cli

  IMPLICIT NONE

  CHARACTER(LEN=255) :: string, message
  INTEGER :: status

  ! Declare and initialize a NAMELIST for values of interest
  INTEGER  :: i=1,   j=2
  REAL     :: s=3.3, t=4.4
  NAMELIST /cmd/ i, j, s, t

  ! Get command line arguments as a string ready for NAMELIST
  CALL get_command_arguments(string, "&cmd", "/")
  READ (string, NML=cmd, IOSTAT=status, IOMSG=message)
  IF (status /= 0) THEN
    WRITE (*,*) 'Error:   ', status
    WRITE (*,*) 'Message: ', message
    CALL EXIT (status)
  END IF

  ! Echo the results
  WRITE (*, NML=cmd)

CONTAINS

!> Gather command arguments into a single string.  Optionally,
!! surround \c string with the given \c prefix and \c suffix.
!!
!! @param[out] string String containing all arguments.
!! @param[in]  prefix Prefix to prepend to \c string.
!! @param[in]  suffix Suffix to append to \c string.
!! @param[in]  first First argument to include using
!!             GET_COMMAND_ARGUMENT's numbering scheme.
!!             Defaults to 1 if not provided.
!! @param[in]  last Last argument to include (inclusive) using
!!             GET_COMMAND_ARGUMENT's numbering scheme.
!!             Defaults to COMMAND_ARGUMENT_COUNT if not provided.
!! @param[out] length Last character position set in \c args.
!! @param[out] status Zero on success.  Nonzero otherwise.
!!             If not present, any error causes <tt>CALL ABORT</tt>.
SUBROUTINE get_command_arguments (string, prefix, suffix, &
                                  first, last, length, status)

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(out)           :: string
  CHARACTER(LEN=*), INTENT(in),  OPTIONAL :: prefix
  CHARACTER(LEN=*), INTENT(in),  OPTIONAL :: suffix
  INTEGER,          INTENT(in),  OPTIONAL :: first
  INTEGER,          INTENT(in),  OPTIONAL :: last
  INTEGER,          INTENT(out), OPTIONAL :: length
  INTEGER,          INTENT(out), OPTIONAL :: status

! Bonus points if someone can figure out dynamically allocatable
! character arrays so that there are not fixed buffer limits.
! Seems only possible in newer Fortran standards.
  INTEGER, PARAMETER    :: buflen = 255
  CHARACTER(LEN=buflen) :: buf
  INTEGER               :: bufpos, arglen, i
  INTEGER               :: ifirst, ilast, ilength, istatus

! Provide default values and sanity checks on first and last
  IF (PRESENT(last)) THEN
    ilast = MIN(last, COMMAND_ARGUMENT_COUNT())
  ELSE
    ilast = COMMAND_ARGUMENT_COUNT()
  ENDIF
  IF (PRESENT(first)) THEN
    ifirst = MIN(first, ilast)
  ELSE
    ifirst = 1
  ENDIF

! Determine the total storage necessary to hold the returned string
  IF (PRESENT(prefix)) THEN
    ilength = LEN_TRIM(prefix) + 1
  ELSE
    ilength = 0
  END IF

  DO i = ifirst, ilast
    CALL GET_COMMAND_ARGUMENT (i, buf, arglen, istatus)
    IF (istatus /= 0) GOTO 5                  ! Unrecoverable
    ilength = LEN_TRIM(buf)                   ! Accumulate length
    IF (i /= ilast) ilength = ilength + 1     ! Accumulate padding
  END DO

  IF (PRESENT(suffix)) THEN
    ilength = 1 + LEN_TRIM(suffix)
  ELSE
    ilength = 0
  END IF

  IF (PRESENT(length)) length = ilength

! Ensure caller has the storage necessary to house the result
  IF (LEN(string) < ilength) THEN
    status = -1
    GOTO 5
  END IF

! Accumulate the desired string tracking length in bufpos
  IF (PRESENT(prefix)) THEN
    string = TRIM(prefix) // " "
    bufpos = LEN_TRIM(prefix) + 1
  ELSE
    string = ''
    bufpos = 0
  ENDIF

  DO i = ifirst, ilast
    CALL GET_COMMAND_ARGUMENT (i, buf, arglen, istatus)
    IF (istatus /= 0) GOTO 5
    arglen = LEN_TRIM(buf)
    IF (i == ifirst) THEN
      string = string(:bufpos) // TRIM(buf)
      bufpos = bufpos + arglen
    ELSE
      string = string(:bufpos) // " " // TRIM(buf)
      bufpos = bufpos + 1 + arglen
    END IF
  END DO

  IF (PRESENT(suffix)) THEN
      string = string(:bufpos) // " " // TRIM(suffix)
  END IF

  RETURN

! Common error handling code for handling subroutine failure
5 IF (PRESENT(status)) THEN
    status = istatus
    RETURN
  END IF
  WRITE (*,*) "get_command_arguments: " // &
              "error encountered when status not used"
  CALL ABORT

END SUBROUTINE get_command_arguments

END PROGRAM namelist_cli
