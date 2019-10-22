        !COMPILER-GENERATED INTERFACE MODULE: Tue Oct 22 00:03:45 2019
        MODULE ONESTEPPLASTICITY__genmod
          INTERFACE 
            SUBROUTINE ONESTEPPLASTICITY(F,S,CI,THICHNESS,CAUCHY,PK1,MU0&
     &,K0,ETA,MU,K,GAMMAR,PHI_N,PHI_G,DT,YIELDSTRESS,YIELDSTRESS0,      &
     &GAMMAR0,BETAR,N,FLAG)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: F(3,3,N)
              REAL(KIND=8) :: S(N)
              REAL(KIND=8) :: CI(3,3,N)
              REAL(KIND=8) :: THICHNESS(N)
              REAL(KIND=8) :: CAUCHY(3,3,N)
              REAL(KIND=8) :: PK1(3,3,N)
              REAL(KIND=8) :: MU0
              REAL(KIND=8) :: K0
              REAL(KIND=8) :: ETA
              REAL(KIND=8) :: MU(N)
              REAL(KIND=8) :: K(N)
              REAL(KIND=8) :: GAMMAR(N)
              REAL(KIND=8) :: PHI_N(N)
              REAL(KIND=8) :: PHI_G(N)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: YIELDSTRESS(N)
              REAL(KIND=8) :: YIELDSTRESS0(N)
              REAL(KIND=8) :: GAMMAR0
              REAL(KIND=8) :: BETAR
              INTEGER(KIND=4) :: FLAG
            END SUBROUTINE ONESTEPPLASTICITY
          END INTERFACE 
        END MODULE ONESTEPPLASTICITY__genmod
