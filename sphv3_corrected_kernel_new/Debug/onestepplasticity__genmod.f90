        !COMPILER-GENERATED INTERFACE MODULE: Thu Oct 17 10:01:46 2019
        MODULE ONESTEPPLASTICITY__genmod
          INTERFACE 
            SUBROUTINE ONESTEPPLASTICITY(F,S,CI,THICHNESS,COUCHY,PK1,MU,&
     &K,ETA,DT,YIELDSTRESS,GAMMAR,BETAR,GAMMAS,BETAS,N,FLAG)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: F(3,3,N)
              REAL(KIND=8) :: S(N)
              REAL(KIND=8) :: CI(3,3,N)
              REAL(KIND=8) :: THICHNESS(N)
              REAL(KIND=8) :: COUCHY(3,3,N)
              REAL(KIND=8) :: PK1(3,3,N)
              REAL(KIND=8) :: MU
              REAL(KIND=8) :: K
              REAL(KIND=8) :: ETA
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: YIELDSTRESS(N)
              REAL(KIND=8) :: GAMMAR
              REAL(KIND=8) :: BETAR
              REAL(KIND=8) :: GAMMAS
              REAL(KIND=8) :: BETAS
              INTEGER(KIND=4) :: FLAG
            END SUBROUTINE ONESTEPPLASTICITY
          END INTERFACE 
        END MODULE ONESTEPPLASTICITY__genmod
