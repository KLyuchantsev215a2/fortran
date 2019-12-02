        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 27 10:29:07 2019
        MODULE ONESTEPPLASTICITY__genmod
          INTERFACE 
            SUBROUTINE ONESTEPPLASTICITY(F,S,CI,THICHNESS,CAUCHY,PK1,MU0&
     &,K0,ETA,MU,K,GAMMAR,PHI,DT,YIELDSTRESS,YIELDSTRESS0,GAMMAR0,BETAR,&
     &N,FLAG,TABLE,TABLE_NON_LOCAL,COR_W1,VOL,X)
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
              REAL(KIND=8) :: PHI(N)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: YIELDSTRESS(N)
              REAL(KIND=8) :: YIELDSTRESS0(N)
              REAL(KIND=8) :: GAMMAR0
              REAL(KIND=8) :: BETAR
              INTEGER(KIND=4) :: FLAG
              INTEGER(KIND=4) :: TABLE(N,120)
              INTEGER(KIND=4) :: TABLE_NON_LOCAL(N,120)
              REAL(KIND=8) :: COR_W1(N,N)
              REAL(KIND=8) :: VOL
              REAL(KIND=8) :: X(2,N)
            END SUBROUTINE ONESTEPPLASTICITY
          END INTERFACE 
        END MODULE ONESTEPPLASTICITY__genmod
