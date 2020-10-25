C
C
      SUBROUTINE PABC(X,Y,A,B,C)
      IMPLICIT NONE
C
C--PABC CALCULATES COEFFICIENTS A,B,C OF THE PARABOLA 
C--Y=A*X**2+B*X+C, PASSING THROUGH THE GIVEN X,Y POINTS
C
      real X,Y,A,B,C
      DIMENSION X(3),Y(3)
      real C1,C2
C
      C1= X(3)-X(1)
      C2= (Y(2)-Y(1))/(X(2)-X(1))
      A = (C1*C2-Y(3)+Y(1))/C1/(X(2)-X(3))
      B = C2-(X(1)+X(2))*A
      C = Y(1)-X(1)*B-X(1)**2*A
      RETURN 
      END
