        !COMPILER-GENERATED INTERFACE MODULE: Thu Oct 17 14:34:30 2019
        MODULE COMPUTE_NEWTON_FLUID__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_NEWTON_FLUID(F,CN,CN_NEW,PK1N,N,DT,ETAN)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: F(3,3,N)
              REAL(KIND=8) :: CN(3,3,N)
              REAL(KIND=8) :: CN_NEW(3,3,N)
              REAL(KIND=8) :: PK1N(3,3,N)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: ETAN
            END SUBROUTINE COMPUTE_NEWTON_FLUID
          END INTERFACE 
        END MODULE COMPUTE_NEWTON_FLUID__genmod
