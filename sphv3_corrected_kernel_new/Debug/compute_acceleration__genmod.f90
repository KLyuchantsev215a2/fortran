        !COMPILER-GENERATED INTERFACE MODULE: Thu Oct 17 11:52:52 2019
        MODULE COMPUTE_ACCELERATION__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_ACCELERATION(X,CI,CN,CN_NEW,S,ACC,PK1,   &
     &PK1N,F,FEDGE,CAUCHY,X_INIT,THICHNESS,TABLEN,COR_WN,NABLA_W_0_1N,  &
     &NABLA_W_0_2N,TABLE,COR_W,NABLA_W_0_1,NABLA_W_0_2,MU,K,ETA,ETAN,VOL&
     &,YIELDSTRESS,BETAR,GAMMAR,BETAS,GAMMAS,RHO_0,DT,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(2,N)
              REAL(KIND=8) :: CI(3,3,N)
              REAL(KIND=8) :: CN(3,3,N)
              REAL(KIND=8) :: CN_NEW(3,3,N)
              REAL(KIND=8) :: S(N)
              REAL(KIND=8) :: ACC(2,N)
              REAL(KIND=8) :: PK1(3,3,N)
              REAL(KIND=8) :: PK1N(3,3,N)
              REAL(KIND=8) :: F(3,3,N)
              REAL(KIND=8) :: FEDGE(2)
              REAL(KIND=8) :: CAUCHY(3,3,N)
              REAL(KIND=8) :: X_INIT(2,N)
              REAL(KIND=8) :: THICHNESS(N)
              INTEGER(KIND=4) :: TABLEN(N,120)
              REAL(KIND=8) :: COR_WN(N)
              REAL(KIND=8) :: NABLA_W_0_1N(N,N)
              REAL(KIND=8) :: NABLA_W_0_2N(N,N)
              INTEGER(KIND=4) :: TABLE(N,120)
              REAL(KIND=8) :: COR_W(N)
              REAL(KIND=8) :: NABLA_W_0_1(N,N)
              REAL(KIND=8) :: NABLA_W_0_2(N,N)
              REAL(KIND=8) :: MU
              REAL(KIND=8) :: K
              REAL(KIND=8) :: ETA
              REAL(KIND=8) :: ETAN
              REAL(KIND=8) :: VOL
              REAL(KIND=8) :: YIELDSTRESS(N)
              REAL(KIND=8) :: BETAR
              REAL(KIND=8) :: GAMMAR
              REAL(KIND=8) :: BETAS
              REAL(KIND=8) :: GAMMAS
              REAL(KIND=8) :: RHO_0
              REAL(KIND=8) :: DT
            END SUBROUTINE COMPUTE_ACCELERATION
          END INTERFACE 
        END MODULE COMPUTE_ACCELERATION__genmod
