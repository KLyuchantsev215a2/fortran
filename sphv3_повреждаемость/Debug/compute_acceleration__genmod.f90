        !COMPILER-GENERATED INTERFACE MODULE: Thu Oct 31 15:30:03 2019
        MODULE COMPUTE_ACCELERATION__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_ACCELERATION(X,CI,S,ACC,PK1,F,FEDGE,     &
     &CAUCHY,X_INIT,THICHNESS,TABLE,TABLE_NON_LOCAL,COR_W,COR_W1,       &
     &NABLA_W_0_1,NABLA_W_0_2,MU0,K0,ETA,MU,K,GAMMAR,PHI,ETAN,VOL,      &
     &YIELDSTRESS,YIELDSTRESS0,BETAR,GAMMAR0,RHO_0,DT,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(2,N)
              REAL(KIND=8) :: CI(3,3,N)
              REAL(KIND=8) :: S(N)
              REAL(KIND=8) :: ACC(2,N)
              REAL(KIND=8) :: PK1(3,3,N)
              REAL(KIND=8) :: F(3,3,N)
              REAL(KIND=8) :: FEDGE(2)
              REAL(KIND=8) :: CAUCHY(3,3,N)
              REAL(KIND=8) :: X_INIT(2,N)
              REAL(KIND=8) :: THICHNESS(N)
              INTEGER(KIND=4) :: TABLE(N,120)
              INTEGER(KIND=4) :: TABLE_NON_LOCAL(N,120)
              REAL(KIND=8) :: COR_W(N,N)
              REAL(KIND=8) :: COR_W1(N,N)
              REAL(KIND=8) :: NABLA_W_0_1(N,N)
              REAL(KIND=8) :: NABLA_W_0_2(N,N)
              REAL(KIND=8) :: MU0
              REAL(KIND=8) :: K0
              REAL(KIND=8) :: ETA
              REAL(KIND=8) :: MU(N)
              REAL(KIND=8) :: K(N)
              REAL(KIND=8) :: GAMMAR(N)
              REAL(KIND=8) :: PHI(N)
              REAL(KIND=8) :: ETAN
              REAL(KIND=8) :: VOL
              REAL(KIND=8) :: YIELDSTRESS(N)
              REAL(KIND=8) :: YIELDSTRESS0(N)
              REAL(KIND=8) :: BETAR
              REAL(KIND=8) :: GAMMAR0
              REAL(KIND=8) :: RHO_0
              REAL(KIND=8) :: DT
            END SUBROUTINE COMPUTE_ACCELERATION
          END INTERFACE 
        END MODULE COMPUTE_ACCELERATION__genmod
