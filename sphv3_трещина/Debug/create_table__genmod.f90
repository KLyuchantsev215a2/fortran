        !COMPILER-GENERATED INTERFACE MODULE: Sun Nov 17 18:24:44 2019
        MODULE CREATE_TABLE__genmod
          INTERFACE 
            SUBROUTINE CREATE_TABLE(X,H,TABLE,N,DH)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(2,N)
              REAL(KIND=8) :: H
              INTEGER(KIND=4) :: TABLE(N,120)
              REAL(KIND=8) :: DH
            END SUBROUTINE CREATE_TABLE
          END INTERFACE 
        END MODULE CREATE_TABLE__genmod
