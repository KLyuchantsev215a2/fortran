        !COMPILER-GENERATED INTERFACE MODULE: Sat Dec 21 16:37:49 2019
        MODULE COMPUTE_W_BAZANT__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_W_BAZANT(X,H,N,W,TABLE)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(2,N)
              REAL(KIND=8) :: H
              REAL(KIND=8) :: W(N,N)
              INTEGER(KIND=4) :: TABLE(N,1800)
            END SUBROUTINE COMPUTE_W_BAZANT
          END INTERFACE 
        END MODULE COMPUTE_W_BAZANT__genmod
