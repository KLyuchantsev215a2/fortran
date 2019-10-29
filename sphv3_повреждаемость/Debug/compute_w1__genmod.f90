        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct 28 22:05:44 2019
        MODULE COMPUTE_W1__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_W1(X,XPER,H,N,VOL,W,TABLE)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(2,N)
              REAL(KIND=8) :: XPER(2,N)
              REAL(KIND=8) :: H
              REAL(KIND=8) :: VOL
              REAL(KIND=8) :: W(N,N)
              INTEGER(KIND=4) :: TABLE(N,120)
            END SUBROUTINE COMPUTE_W1
          END INTERFACE 
        END MODULE COMPUTE_W1__genmod
