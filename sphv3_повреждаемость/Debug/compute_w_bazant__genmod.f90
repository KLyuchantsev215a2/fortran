        !COMPILER-GENERATED INTERFACE MODULE: Sun Nov 03 16:32:15 2019
        MODULE COMPUTE_W_BAZANT__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_W_BAZANT(X,H,N,W,TABLE)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(2,N)
              REAL(KIND=8) :: H
              REAL(KIND=8) :: W(N,N)
              INTEGER(KIND=4) :: TABLE(N,120)
            END SUBROUTINE COMPUTE_W_BAZANT
          END INTERFACE 
        END MODULE COMPUTE_W_BAZANT__genmod
