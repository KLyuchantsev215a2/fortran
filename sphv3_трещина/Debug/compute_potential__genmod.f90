        !COMPILER-GENERATED INTERFACE MODULE: Sun Sep 29 21:35:36 2019
        MODULE COMPUTE_POTENTIAL__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_POTENTIAL(F,THICHNESS,CI,U,MU,K,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: F(3,3,N)
              REAL(KIND=8) :: THICHNESS(N)
              REAL(KIND=8) :: CI(3,3,N)
              REAL(KIND=8) :: U(N)
              REAL(KIND=8) :: MU
              REAL(KIND=8) :: K
            END SUBROUTINE COMPUTE_POTENTIAL
          END INTERFACE 
        END MODULE COMPUTE_POTENTIAL__genmod
