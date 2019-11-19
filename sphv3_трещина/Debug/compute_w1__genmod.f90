        !COMPILER-GENERATED INTERFACE MODULE: Thu Oct 31 15:17:05 2019
        MODULE COMPUTE_W1__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_W1(X,H,N,W,TABLE)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(2,N)
              REAL(KIND=8) :: H
              REAL(KIND=8) :: W(N,N)
              INTEGER(KIND=4) :: TABLE(N,120)
            END SUBROUTINE COMPUTE_W1
          END INTERFACE 
        END MODULE COMPUTE_W1__genmod
