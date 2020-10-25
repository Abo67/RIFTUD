      real function sonic_speed(h0,s0,p,Ref)
C
C     returns speed_of_sound once
C     total enthalpy (h0) and entropy (s0) are given
C
      implicit none
c     scalar arguments
      real h0,s0,p
      character(LEN=*) Ref
caldo
      real arg1,arg2
      character(LEN=32) Fluid
      common/CPCOM1/arg1,arg2
      common/CPCOM2/Fluid
caldo
c     local scalars
      character(LEN=32) NameA, NameP, NameS, NameH
      double precision outVal,zk
      real b,c,r,re,ae,help,EX,p0
      integer ifail
c     external function
      real df
      external df
c
      re = .000001
      ae = .00001
Coolprop
      NameA  = "A"//CHAR(0) ! speed of sound
      NameP  = "P"//CHAR(0) ! pressure
      NameH  = "H"//CHAR(0) ! pressure
      NameS  = "S"//CHAR(0) ! Mass specific entropy
Coolprop
      Fluid = Ref
      arg1 = h0
      arg2 = s0
c compute (total) pressure for the given (h0,s0) pair
      call propssi(NameP, NameH, DBLE(h0), NameS, DBLE(s0),
     &Ref, outVal)
      p0 = outVal ! total pressure
      call propssi("ISENTROPIC_EXPANSION_COEFFICIENT"//CHAR(0), NameH, 
     &DBLE(h0), NameS, DBLE(s0), Ref, zk)
      EX=zk/(zk-1.)
c
      r = ((2./(zk+1.))**EX) * p0 ! Initial guess for the rootfinder
      b = 0.1*r
      c = p0
c
      CALL FZERO(df,B,C,R,RE,AE,IFAIL)
c
      IF(.NOT.((IFAIL.EQ.1).OR.(IFAIL.EQ.2)))THEN
         write(6,*)'FZERO has returned IFAIL = ',IFAIL,df(b)
         call exit(IFAIL)
      ENDIF
      p = b
c     compute the speed of sound
      call propssi(NameA, NameP, DBLE(B), NameS, DBLE(s0),
     &Ref, outVal)
      sonic_speed = outVal
      return
      end
C
C
C
      real function df(p)
C
C     function to be zeroed by the rootfinder
C
C     df(p) = h(s0,p^*) + 0.5*a^2(s0,p^*) - h0
C
C
      implicit none
C     scalar arguments
      real p ! INOUT
      real h0,s0 ! IN
C     local scalars
      real dum
      double precision outVal
      character(LEN=32) Fluid
      common/CPCOM1/h0,s0
      common/CPCOM2/Fluid
      character(LEN=32) NameA, NameH, NameS, NameP
C
      NameA  = "A"//CHAR(0) ! speed of sound
      NameH  = "H"//CHAR(0) ! Mass specific enthalpy
      NameS  = "S"//CHAR(0) ! Mass specific entropy
      NameP  = "P"//CHAR(0) ! pressure
C
C compute the speed of sound using pressure and entropy
C
      call propssi(NameA, NameP, DBLE(P), NameS, DBLE(S0),
     &Fluid, outVal)
      dum = 0.5*outVal*outVal ! kinetic energy
C
C compute the (static) enthalpy using pressure and entropy
C
      call propssi(NameH, NameP, DBLE(P), NameS, DBLE(S0),
     &Fluid, outVal)
C
      dum = dum+outVal
      df = dum -h0
      return
      end

