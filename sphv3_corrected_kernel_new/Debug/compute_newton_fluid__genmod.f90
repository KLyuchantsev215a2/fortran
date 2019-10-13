        !COMPILER-GENERATED INTERFACE MODULE: Sun Oct 13 23:47:47 2019
        MODULE COMPUTE_NEWTON_FLUID__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_NEWTON_FLUID(F,ETAN,CN,CN_NEW,PK1N,N,DT)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: F(3,3,N)
              REAL(KIND=8) :: ETAN
              REAL(KIND=8) :: CN(3,3,N)
              REAL(KIND=8) :: CN_NEW(3,3,N)
              REAL(KIND=8) :: PK1N(3,3,N)
              REAL(KIND=8) :: DT
            END SUBROUTINE COMPUTE_NEWTON_FLUID
          END INTERFACE 
        END MODULE COMPUTE_NEWTON_FLUID__genmod
