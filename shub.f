      REAL FUNCTION SHUB(X)
      IMPLICIT NONE
      real X
      real AA,BB,YC
      COMMON/SH/AA,BB,YC 
      SHUB=(YC-BB/AA*SQRT(AA**2-X*X))*SQRT(1.+BB**2/AA**2*X*X/(AA**2-X*X 
     1))
      RETURN
      END



      REAL FUNCTION SHUB2(Y) 
      IMPLICIT NONE
      real Y
      real AA,BB,YC
      COMMON/SH/AA,BB,YC 
      SHUB2=Y*SQRT(1.+AA**2/BB**2*(Y-YC)**2/(BB**2-(Y-YC)**2))
      RETURN 
      END
