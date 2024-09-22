PROGRAM FIND_MINIMUN

!This program uses the module minimun_tools to perform a search
!of local minimuns within a global field. Then it can compute also
!the associated systems (based on the system basin principle).
!This is just a driver for the execution of the routines contained in
!the minimun_tools module.

USE MINIMUN_TOOLS

IMPLICIT NONE

CALL ALLOCATE_ARRAYS         !Save space in memory for the arrays.

CALL READ_DATA               !Ingest the anomaly field

CALL GET_LOCAL_MINIMA        !Get local minimuns in the field

CALL GET_SYSTEM              !Find the "systems" associated with each anomaly minimun
 
!CALL MERGE_MINIMUN           !Merge close minimuns

!CALL INTERP_MINIMUN_POSITION !Get adjusted minimun position.

CALL GET_SYSTEM_INFO         !Get area, intensity of the systems.

CALL WRITE_DATA              !Write out the information.

STOP 
END PROGRAM FIND_MINIMUN

