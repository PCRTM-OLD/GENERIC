MODULE PROF_PC_SOLUTION

  USE PCRTM_FILE_UTILITIES, ONLY       : GETLUN
  USE PCRTM_TYPE_KIND  

  TYPE PROF_PC_SOLUTION_TYPE
     INTEGER                   :: ITYPE
     
     INTEGER                   :: NTEMP
     INTEGER                   :: NH2O
     INTEGER                   :: NCO2
     INTEGER                   :: NO3
     INTEGER                   :: NN2O
     INTEGER                   :: NCO
     INTEGER                   :: NCH4
     INTEGER                   :: NEM
     INTEGER                   :: NVEC

     REAL(SINGLE), ALLOCATABLE :: PROF_VEC(:)
     REAL(SINGLE), ALLOCATABLE :: SXINV(:,:)
     REAL(SINGLE), ALLOCATABLE :: SXINV_OC(:,:),SXINV_LD(:,:)

     REAL(SINGLE), ALLOCATABLE :: UTEMP(:,:)
     REAL(SINGLE), ALLOCATABLE :: UH2O(:,:)
     REAL(SINGLE), ALLOCATABLE :: UCO2(:,:)
     REAL(SINGLE), ALLOCATABLE :: UO3(:,:)
     REAL(SINGLE), ALLOCATABLE :: UN2O(:,:)
     REAL(SINGLE), ALLOCATABLE :: UCO(:,:)
     REAL(SINGLE), ALLOCATABLE :: UCH4(:,:)

     REAL(SINGLE), ALLOCATABLE :: TEMPBK(:)
     REAL(SINGLE), ALLOCATABLE :: H2OBK(:)
     REAL(SINGLE), ALLOCATABLE :: CO2BK(:)
     REAL(SINGLE), ALLOCATABLE :: O3BK(:)
     REAL(SINGLE), ALLOCATABLE :: N2OBK(:)
     REAL(SINGLE), ALLOCATABLE :: COBK(:)
     REAL(SINGLE), ALLOCATABLE :: CH4BK(:)
     REAL(SINGLE)              :: TSBK 
     
     REAL(SINGLE), ALLOCATABLE :: TEMPMX(:),TEMPMN(:)
     REAL(SINGLE), ALLOCATABLE :: H2OMX(:),H2OMN(:)
     REAL(SINGLE), ALLOCATABLE :: CO2MX(:),CO2MN(:)
     REAL(SINGLE), ALLOCATABLE :: O3MX(:),O3MN(:)
     REAL(SINGLE), ALLOCATABLE :: N2OMX(:),N2OMN(:)
     REAL(SINGLE), ALLOCATABLE :: COMX(:),COMN(:)
     REAL(SINGLE), ALLOCATABLE :: CH4MX(:),CH4MN(:)

     REAL(SINGLE), ALLOCATABLE :: UEM_LD(:,:), UEM_OC(:,:)
     REAL(SINGLE), ALLOCATABLE :: EMBK_LD(:), EMBK_OC(:)  
     REAL(SINGLE), ALLOCATABLE :: UEM(:,:)
     REAL(SINGLE), ALLOCATABLE :: EMBK(:)
  END TYPE PROF_PC_SOLUTION_TYPE

CONTAINS

  SUBROUTINE INIT_PROF_PC_SOLUTION_TYPE( PROF_PC_SOLUTION,   &
                                         NLEV,               &
                                         NM,                 &
                                         NTEMP,              &
                                         NH2O,               &
                                         NCO2,               &
                                         NO3,                &
                                         NN2O,               &
                                         NCO,                &
                                         NCH4,               &
                                         NLEVO3,             &
                                         NLEVCO,             &
                                         NEM,                &
                                         NCLD )
                                         

    TYPE(PROF_PC_SOLUTION_TYPE),INTENT(OUT):: PROF_PC_SOLUTION
    INTEGER,                    INTENT(IN) :: NLEV
    INTEGER,                    INTENT(IN) :: NM
    INTEGER,                    INTENT(IN) :: NTEMP
    INTEGER,                    INTENT(IN) :: NH2O
    INTEGER,                    INTENT(IN) :: NCO2
    INTEGER,                    INTENT(IN) :: NO3
    INTEGER,                    INTENT(IN) :: NN2O
    INTEGER,                    INTENT(IN) :: NCO
    INTEGER,                    INTENT(IN) :: NCH4
    INTEGER,                    INTENT(IN) :: NLEVO3
    INTEGER,                    INTENT(IN) :: NLEVCO
    INTEGER,                    INTENT(IN) :: NEM
    INTEGER,                    INTENT(IN) :: NCLD

    INTEGER                                :: NX
    INTEGER                                :: ALLOC_STAT

    NX = NEM + NTEMP + 1 + NH2O + NCO2 + NO3 + NN2O + NCO + NCH4 + NCLD*3

    PROF_PC_SOLUTION%NVEC = NX
    
    ALLOCATE( PROF_PC_SOLUTION%PROF_VEC(NX),      &
              PROF_PC_SOLUTION%SXINV(NX,NX),      &
              PROF_PC_SOLUTION%SXINV_OC(NEM,NEM), &
              PROF_PC_SOLUTION%SXINV_LD(NEM,NEM), &
              PROF_PC_SOLUTION%UTEMP(NLEV,NTEMP), &
              PROF_PC_SOLUTION%UH2O(NLEV,NH2O),   &
              PROF_PC_SOLUTION%UCO2(NLEV,NCO2),   &
              PROF_PC_SOLUTION%UO3(NLEVO3,NO3),   &
              PROF_PC_SOLUTION%UN2O(NLEV,NN2O),   &
              PROF_PC_SOLUTION%UCO(NLEVCO,NCO),   &
              PROF_PC_SOLUTION%UCH4(NLEV,NCH4),   &
              PROF_PC_SOLUTION%UEM(NM,NEM),       &
              PROF_PC_SOLUTION%UEM_OC(NM,NEM),    &
              PROF_PC_SOLUTION%UEM_LD(NM,NEM),    &
              PROF_PC_SOLUTION%TEMPBK(NLEV),      &
              PROF_PC_SOLUTION%H2OBK(NLEV),       &
              PROF_PC_SOLUTION%CO2BK(NLEV),       &
              PROF_PC_SOLUTION%O3BK(NLEVO3),      &
              PROF_PC_SOLUTION%N2OBK(NLEV),       &
              PROF_PC_SOLUTION%COBK(NLEVCO),      &
              PROF_PC_SOLUTION%CH4BK(NLEV),       &
              PROF_PC_SOLUTION%TEMPMX(NLEV),      &
              PROF_PC_SOLUTION%H2OMX(NLEV),       &
              PROF_PC_SOLUTION%CO2MX(NLEV),       &
              PROF_PC_SOLUTION%O3MX(NLEV),        &
              PROF_PC_SOLUTION%N2OMX(NLEV),       &
              PROF_PC_SOLUTION%COMX(NLEV),        &
              PROF_PC_SOLUTION%CH4MX(NLEV),       &
              PROF_PC_SOLUTION%TEMPMN(NLEV),      &
              PROF_PC_SOLUTION%H2OMN(NLEV),       &
              PROF_PC_SOLUTION%CO2MN(NLEV),       &
              PROF_PC_SOLUTION%O3MN(NLEV),        &
              PROF_PC_SOLUTION%N2OMN(NLEV),       &
              PROF_PC_SOLUTION%COMN(NLEV),        &
              PROF_PC_SOLUTION%CH4MN(NLEV),       &
              PROF_PC_SOLUTION%EMBK(NM),          &
              PROF_PC_SOLUTION%EMBK_OC(NM),       &
              PROF_PC_SOLUTION%EMBK_LD(NM),       &
              STAT = ALLOC_STAT )

    IF( ALLOC_STAT /= 0 ) THEN
       PRINT*,'ERROR TRYING TO ALLOCATE PROF_PC_SOLUTION'
       STOP
    ENDIF

    PROF_PC_SOLUTION%NTEMP = NTEMP
    PROF_PC_SOLUTION%NH2O  = NH2O
    PROF_PC_SOLUTION%NCO2  = NCO2
    PROF_PC_SOLUTION%NO3   = NO3
    PROF_PC_SOLUTION%NN2O  = NN2O
    PROF_PC_SOLUTION%NCO   = NCO
    PROF_PC_SOLUTION%NCH4  = NCH4
    PROF_PC_SOLUTION%NEM   = NEM
    PROF_PC_SOLUTION%SXINV = 0
    PROF_PC_SOLUTION%SXINV_OC = 0
    PROF_PC_SOLUTION%SXINV_LD = 0
    PROF_PC_SOLUTION%PROF_VEC = 0

  END SUBROUTINE INIT_PROF_PC_SOLUTION_TYPE
  
  SUBROUTINE RD_PROF_COV_BK( PROF_PC_SOLUTION,    &
                             EM_BKFILE,           &
                             EMLD_BKFILE,         &
                             EMOC_BKFILE,         &
                             BKFILENAME,          &
                             NLEV,                &
                             NM,                  &
                             NTEMP,               &
                             NH2O,                &
                             NCO2,                &
                             NO3,                 &
                             NN2O,                &
                             NCO,                 &
                             NCH4,                &
                             NEM,                 &
                             NCLD )

    TYPE(PROF_PC_SOLUTION_TYPE),INTENT(INOUT) :: PROF_PC_SOLUTION
    CHARACTER(160),             INTENT(IN)    :: EM_BKFILE
    CHARACTER(160),             INTENT(IN)    :: EMLD_BKFILE
    CHARACTER(160),             INTENT(IN)    :: EMOC_BKFILE
    CHARACTER(160),             INTENT(IN)    :: BKFILENAME(*)
    INTEGER,                    INTENT(IN)    :: NLEV
    INTEGER,                    INTENT(IN)    :: NM
    INTEGER,                    INTENT(IN)    :: NTEMP
    INTEGER,                    INTENT(IN)    :: NH2O
    INTEGER,                    INTENT(IN)    :: NCO2
    INTEGER,                    INTENT(IN)    :: NO3
    INTEGER,                    INTENT(IN)    :: NN2O
    INTEGER,                    INTENT(IN)    :: NCO
    INTEGER,                    INTENT(IN)    :: NCH4
    INTEGER,                    INTENT(IN)    :: NEM
    INTEGER,                    INTENT(IN)    :: NCLD
                             
    INTEGER                                   :: N1, N2
    INTEGER                                   :: NLEVO3 = 70
    INTEGER                                   :: NLEVCO = 71
    REAL(SINGLE)                              :: CLDCOV(3)
    INTEGER                                   :: IUBK
    INTEGER                                   :: I,J,N
    REAL(DOUBLE),             ALLOCATABLE     :: VEC1(:)

    
    CALL     INIT_PROF_PC_SOLUTION_TYPE( PROF_PC_SOLUTION,   &
                                         NLEV,               &
                                         NM,                 &
                                         NTEMP,              &
                                         NH2O,               &
                                         NCO2,               &
                                         NO3,                &
                                         NN2O,               &
                                         NCO,                &
                                         NCH4,               &
                                         NLEVO3,             &
                                         NLEVCO,             &
                                         NEM,                &
                                         NCLD )

    N2   = 0

    CALL GETLUN(IUBK)

!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    IF(NEM.GT.0)THEN
       N1=N2+1
       N2=N1+NEM-1
       ALLOCATE(VEC1(NM))
!*********************************************************************
! READ COVRARIANCE FOR LAND EMISSIVITIES

       OPEN(IUBK,FILE=EMLD_BKFILE,RECL=NM*8,&
            ACCESS='DIRECT',CONVERT='BIG_ENDIAN')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%EMBK_LD=SNGL(VEC1)

       DO I = 1,NEM 
          READ(IUBK,REC=I+1)VEC1
          DO J = 1,NM
             PROF_PC_SOLUTION%UEM_LD(J,I)=SNGL(VEC1(J))
          END DO
       END DO

       READ(IUBK,REC=12)VEC1
       DO N = 1,NEM
          IF (VEC1(N) .GT. 1E6) PRINT*, 'STOP, SXINV_LD'
          PROF_PC_SOLUTION%SXINV_LD(N,N)=1/SNGL(VEC1(N))
          PRINT*,N1+N-1,VEC1(N)
       END DO
       CLOSE(IUBK) 
       PRINT*,EMLD_BKFILE


!*********************************************************************
! READ COVRARIANCE FOR OCEAN EMISSIVITIES
       OPEN(IUBK,FILE=EMOC_BKFILE,RECL=NM*8,&
            ACCESS='DIRECT',CONVERT='BIG_ENDIAN')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%EMBK_OC=SNGL(VEC1)

       DO I = 1,NEM 
          READ(IUBK,REC=I+1)VEC1
          DO J = 1,NM
             PROF_PC_SOLUTION%UEM_OC(J,I)=SNGL(VEC1(J))
          END DO
       END DO

       READ(IUBK,REC=12)VEC1
       DO N = 1,NEM
          IF (VEC1(N) .GT. 1E6) PRINT*, 'STOP, SXINV_OC'
          PROF_PC_SOLUTION%SXINV_OC(N,N)=1/SNGL(VEC1(N))
          PRINT*,N1+N-1,VEC1(N)
       END DO
       PRINT*,EMOC_BKFILE


!*********************************************************************
! READ IASI EMIS BUILT FROM 'ALLCONSTRUCTEDIN'
       OPEN(IUBK,FILE=EM_BKFILE,RECL=NM*8,&
            ACCESS='DIRECT',CONVERT='BIG_ENDIAN')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%EMBK=SNGL(VEC1)

       DO I = 1,NEM 
          READ(IUBK,REC=I+1)VEC1
          DO J = 1,NM
             PROF_PC_SOLUTION%UEM(J,I)=SNGL(VEC1(J))
          END DO
       END DO

       READ(IUBK,REC=12)VEC1
       DO N = 1,NEM
          PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1/SNGL(VEC1(N))
          PRINT*,N1+N-1,VEC1(N)
       END DO
       PRINT*,EM_BKFILE  

!*********************************************************************
       DEALLOCATE(VEC1)  
    END IF
!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

    ALLOCATE(VEC1(NLEV))
    IF(NTEMP.GT.0)THEN
       N1 = N2+1
       N2 = N1+NTEMP-1
       OPEN(IUBK,FILE=BKFILENAME(1),RECL=NLEV*8,ACCESS='DIRECT')
       READ(IUBK,REC=1)VEC1    
       PROF_PC_SOLUTION%TEMPBK=SNGL(VEC1)
       READ(IUBK,REC=2)VEC1    
       PROF_PC_SOLUTION%TEMPMX=SNGL(VEC1)
       READ(IUBK,REC=3)VEC1    
       PROF_PC_SOLUTION%TEMPMN=SNGL(VEC1)
       READ(IUBK,REC=4)VEC1
       DO N = 1,NTEMP
          PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1/VEC1(N)
          PRINT*,N1+N-1,PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1),VEC1(N)
       END DO
       DO I = 1,NTEMP
          READ(IUBK,REC=I+4)VEC1
          DO J = 1,NLEV
             PROF_PC_SOLUTION%UTEMP(J,I)=SNGL(VEC1(J))
          END DO
       END DO
       CLOSE(IUBK)
    END IF
    PRINT*,BKFILENAME(1)

    N1    = N2+1
    N2    = N1
    PROF_PC_SOLUTION%TSBK         = 282.9595  
    PROF_PC_SOLUTION%SXINV(N2,N2) = 1.0/439.12

    IF(NH2O.GT.0)THEN
       N1 = N2+1
       N2 = N1+NH2O-1
       OPEN(IUBK,FILE=BKFILENAME(2),RECL=NLEV*8,ACCESS='DIRECT')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%H2OBK=SNGL(VEC1)
       READ(IUBK,REC=2)VEC1
       PROF_PC_SOLUTION%H2OMX=SNGL(VEC1)
       READ(IUBK,REC=3)VEC1
       PROF_PC_SOLUTION%H2OMN=SNGL(VEC1)
       READ(IUBK,REC=4)VEC1
       DO N = 1,NH2O
          PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1/VEC1(N)
          PRINT*,N1+N-1,PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1),VEC1(N)
       END DO
       DO I = 1,NH2O
          READ(IUBK,REC=I+4)VEC1
          DO J = 1,NLEV
             PROF_PC_SOLUTION%UH2O(J,I)=SNGL(VEC1(J))
          END DO
       END DO
       CLOSE(IUBK)
    END IF
    PRINT*,BKFILENAME(2)

    IF(NCO2.GT.0)THEN
       N1 = N2+1
       N2 = N1+NCO2-1
       OPEN(IUBK,FILE=BKFILENAME(3),RECL=NLEV*8,ACCESS='DIRECT')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%CO2BK=SNGL(VEC1)
       READ(IUBK,REC=2)VEC1
       PROF_PC_SOLUTION%CO2MX=SNGL(VEC1)
       READ(IUBK,REC=3)VEC1
       PROF_PC_SOLUTION%CO2MN=SNGL(VEC1)
       READ(IUBK,REC=4)VEC1
       DO N = 1,NCO2
          PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1/VEC1(N)
          PRINT*,N1+N-1,PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1),VEC1(N)
       END DO
       DO I = 1,NCO2
          READ(IUBK,REC=I+4)VEC1
          DO J = 1,NLEV
             PROF_PC_SOLUTION%UCO2(J,I)=SNGL(VEC1(J))
          END DO
       END DO
       CLOSE(IUBK)
    END IF
    PRINT*,BKFILENAME(3)
    DEALLOCATE(VEC1)

    IF(NO3.GT.0)THEN
       N1 = N2+1
       N2 = N1+NO3-1
       ALLOCATE(VEC1(NLEVO3))
       OPEN(IUBK,FILE=BKFILENAME(4),RECL=NLEVO3*8,ACCESS='DIRECT')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%O3BK=SNGL(VEC1)
       READ(IUBK,REC=2)VEC1
       PROF_PC_SOLUTION%O3MX(5:4+NLEVO3)=SNGL(VEC1)
       PROF_PC_SOLUTION%O3MX(1:4) = PROF_PC_SOLUTION%O3MX(5)
       PROF_PC_SOLUTION%O3MX(5+NLEVO3:NLEV) = PROF_PC_SOLUTION%O3MX(4+NLEVO3)
       READ(IUBK,REC=3)VEC1
       PROF_PC_SOLUTION%O3MN(5:4+NLEVO3)=SNGL(VEC1)
       READ(IUBK,REC=4)VEC1
       DO N = 1,NO3
          PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1/VEC1(N)
          PRINT*,N1+N-1,PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1),VEC1(N)
       END DO
       DO I = 1,NO3
          READ(IUBK,REC=I+4)VEC1
          DO J = 1,NLEVO3
             PROF_PC_SOLUTION%UO3(J,I)=SNGL(VEC1(J))
          END DO
       END DO
       CLOSE(IUBK)
       DEALLOCATE(VEC1)
    END IF
    PRINT*,BKFILENAME(4)    

    IF(NN2O.GT.0)THEN
       N1 = N2+1
       N2 = N1+NN2O-1
       ALLOCATE(VEC1(NLEV))
       OPEN(IUBK,FILE=BKFILENAME(5),RECL=NLEV*8,ACCESS='DIRECT')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%N2OBK=SNGL(VEC1)
       READ(IUBK,REC=2)VEC1
       PROF_PC_SOLUTION%N2OMX=SNGL(VEC1)
       READ(IUBK,REC=3)VEC1
       PROF_PC_SOLUTION%N2OMN=SNGL(VEC1)
       READ(IUBK,REC=4)VEC1
       DO N = 1,NN2O
          PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1/VEC1(N)
          PRINT*,N1+N-1,PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1),VEC1(N)
       END DO
       DO I = 1,NN2O
          READ(IUBK,REC=I+4)VEC1
          DO J = 1,NLEV
             PROF_PC_SOLUTION%UN2O(J,I)=SNGL(VEC1(J))
          END DO
       END DO
       CLOSE(IUBK)
       DEALLOCATE(VEC1)
    END IF
    PRINT*,BKFILENAME(5)    

    IF(NCO.GT.0)THEN
       N1 = N2+1
       N2 = N1+NCO-1
       ALLOCATE(VEC1(NLEVCO))
       OPEN(IUBK,FILE=BKFILENAME(6),RECL=NLEVCO*8,ACCESS='DIRECT')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%COBK=SNGL(VEC1)
       READ(IUBK,REC=2)VEC1
       PROF_PC_SOLUTION%COMX(NLEV-NLEVCO+1:NLEV)=SNGL(VEC1)
       PROF_PC_SOLUTION%COMX(1:NLEV-NLEVCO) = PROF_PC_SOLUTION%COMX(NLEV-NLEVCO+1)
       READ(IUBK,REC=3)VEC1
       PROF_PC_SOLUTION%COMN(NLEV-NLEVCO+1:NLEV)=SNGL(VEC1)
       PROF_PC_SOLUTION%COMN(1:NLEV-NLEVCO) = PROF_PC_SOLUTION%COMN(NLEV-NLEVCO+1)
       READ(IUBK,REC=4)VEC1
       DO N = 1,NCO
          PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1/VEC1(N)
          PRINT*,N1+N-1,PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1),VEC1(N)
       END DO
       DO I = 1,NCO
          READ(IUBK,REC=I+4)VEC1
          DO J = 1,NLEVCO
             PROF_PC_SOLUTION%UCO(J,I)=SNGL(VEC1(J))
          END DO
       END DO
       CLOSE(IUBK)
       DEALLOCATE(VEC1)
    END IF
    PRINT*,BKFILENAME(6)    

    IF(NCH4.GT.0)THEN
       N1 = N2+1
       N2 = N1+NCH4-1
       ALLOCATE(VEC1(NLEV))
       OPEN(IUBK,FILE=BKFILENAME(7),RECL=NLEV*8,ACCESS='DIRECT')
       READ(IUBK,REC=1)VEC1
       PROF_PC_SOLUTION%CH4BK=SNGL(VEC1)
       READ(IUBK,REC=2)VEC1
       PROF_PC_SOLUTION%CH4MX=SNGL(VEC1)
       READ(IUBK,REC=3)VEC1
       PROF_PC_SOLUTION%CH4MN=SNGL(VEC1)
       READ(IUBK,REC=4)VEC1
       DO N = 1,NCH4
          PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1/VEC1(N)
          PRINT*,N1+N-1,PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1),VEC1(N)
       END DO
       DO I = 1,NCH4
          READ(IUBK,REC=I+4)VEC1
          DO J = 1,NLEV
             PROF_PC_SOLUTION%UCH4(J,I)=SNGL(VEC1(J))
          END DO
       END DO
       CLOSE(IUBK)
       DEALLOCATE(VEC1)
    END IF
    PRINT*,BKFILENAME(7)    

    IF(NCLD.GT.0)THEN
       CLDCOV(1) = 0.04
       CLDCOV(2) = 200.0
       CLDCOV(3) = 40000
       DO I  = 1, NCLD
          N1 = N2 + 1
          N2 = N1 + 2
          DO N = 1,3
             PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1)=1.0/CLDCOV(N)
             IF (PROF_PC_SOLUTION%SXINV(N1+N-1,N1+N-1) .LT. 1E-6) PRINT*, 'STOP, CLDCOV(N)',CLDCOV(N) 
          ENDDO 
       END DO
    ENDIF

  END SUBROUTINE RD_PROF_COV_BK

END MODULE PROF_PC_SOLUTION
  
