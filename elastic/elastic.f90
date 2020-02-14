! This program prepares the necessary lattice vectors
! for calculating elastic constants using strain tensors
! up to now only hexagonal structure is done
program elastic

implicit none

integer:: ngscalc,i
character(50):: system,lattice,str
double precision:: a1(3),a2(3),a3(3),maxdelta,mindelta,interval,a1p(3),a2p(3),a3p(3),strain(6)

namelist /input/ system,lattice,a1,a2,a3,maxdelta,mindelta,interval

!Read input file
open(5)
read(5,input)
close(5)
open(6)
write(6,*) "Program Elastic started to be written in 18 April 2011"

write(6,*) 'System    : ', system
write(6,*) 'Lattice   : ', Lattice
write(6,'(" a1        : ",3F15.10)'), a1
write(6,'(" a2        : ",3F15.10)'), a2
write(6,'(" a3        : ",3F15.10)'), a3
write(6,'(" Max Delta : ",F8.4)'), maxdelta
write(6,'(" Min Delta : ",F8.4)'), mindelta
write(6,'(" Interval  : ",F8.4)'), interval
ngscalc=int(((maxdelta-mindelta)/interval)+1)
write(6,'("No of ground state calcs  : ",I5)'), ngscalc
write(6,*)
write(6,*)
write(6,*)

if (lattice=="hexagonal") then

write(6,*) "Hexagonal structure!!!!"
write(6,*) "Followed ﻿Wang & Ye (2003) Journal of Physics: Condensed Matter, 15, 5307"
write(6,*) "============================================================"
write(6,*) "calc of c11+c12"
! calc of c11+c12
do i=1,ngscalc
 strain(1)=mindelta+dble(i-1)*interval
 strain(2)=mindelta+dble(i-1)*interval;
 strain(3)=0.0d0
 strain(4)=0.0d0
 strain(5)=0.0d0
 strain(6)=0.0d0
 call findNewRprims(a1,a2,a3,strain,a1p,a2p,a3p)
 if (i<10) then
   write(str, '(i1)' )  i
 end if
 if (i>=10) then
   write(str, '(i2)' )  i
 end if
 str='rprim'//trim(str)//''
 write(6,*), str
 write(6,'(" ",3F15.10)'), a1p
 write(6,'(" ",3F15.10)'), a2p
 write(6,'(" ",3F15.10)'), a3p
 write(6,*)
enddo
 write(6,*) "============================================================"
 write(6,*) "calc of c11-c12"
! calc of c11-c12
do i=1,ngscalc
 strain(1)=0.0d0
 strain(2)=0.0d0
 strain(3)=0.0d0
 strain(4)=0.0d0
 strain(5)=0.0d0
 strain(6)=mindelta+dble(i-1)*interval
 call findNewRprims(a1,a2,a3,strain,a1p,a2p,a3p)
 if (i<10) then
   write(str, '(i1)' )  i
 end if
 if (i>=10) then
   write(str, '(i2)' )  i
 end if
 str='rprim'//trim(str)//''
 write(6,*), str
 write(6,'(" ",3F15.10)'), a1p
 write(6,'(" ",3F15.10)'), a2p
 write(6,'(" ",3F15.10)'), a3p
 write(6,*)
enddo

 write(6,*) "============================================================"
 write(6,*) "calc of c33"
! calc of c33
do i=1,ngscalc
 strain(1)=0.0d0
 strain(2)=0.0d0
 strain(3)=mindelta+dble(i-1)*interval
 strain(4)=0.0d0
 strain(5)=0.0d0
 strain(6)=0.0d0
 call findNewRprims(a1,a2,a3,strain,a1p,a2p,a3p)
 if (i<10) then
   write(str, '(i1)' )  i
 end if
 if (i>=10) then
   write(str, '(i2)' )  i
 end if
 str='rprim'//trim(str)//''
 write(6,*), str
 write(6,'(" ",3F15.10)'), a1p
 write(6,'(" ",3F15.10)'), a2p
 write(6,'(" ",3F15.10)'), a3p
 write(6,*)
enddo

 write(6,*) "============================================================"
 write(6,*) "calc of c44"
! calc of c44
do i=1,ngscalc
 strain(1)=0.0d0
 strain(2)=0.0d0
 strain(3)=0.0d0
 strain(4)=mindelta+dble(i-1)*interval
 strain(5)=mindelta+dble(i-1)*interval
 strain(6)=0.0d0
 call findNewRprims(a1,a2,a3,strain,a1p,a2p,a3p)
 if (i<10) then
   write(str, '(i1)' )  i
 end if
 if (i>=10) then
   write(str, '(i2)' )  i
 end if
 str='rprim'//trim(str)//''
 write(6,*), str
 write(6,'(" ",3F15.10)'), a1p
 write(6,'(" ",3F15.10)'), a2p
 write(6,'(" ",3F15.10)'), a3p
 write(6,*)
enddo

 write(6,*) "============================================================"
 write(6,*) "calc of B"
! calc of B
do i=1,ngscalc
 strain(1)=mindelta+dble(i-1)*interval
 strain(2)=mindelta+dble(i-1)*interval
 strain(3)=mindelta+dble(i-1)*interval
 strain(4)=0.0d0
 strain(5)=0.0d0
 strain(6)=0.0d0
 call findNewRprims(a1,a2,a3,strain,a1p,a2p,a3p)
 if (i<10) then
   write(str, '(i1)' )  i
 end if
 if (i>=10) then
   write(str, '(i2)' )  i
 end if
 str='rprim'//trim(str)//''
 write(6,*), str
 write(6,'(" ",3F15.10)'), a1p
 write(6,'(" ",3F15.10)'), a2p
 write(6,'(" ",3F15.10)'), a3p
 write(6,*)
enddo

 write(6,*) "============================================================"
 write(6,*) "calc of C11-C12 Tahir Hodja's  delta -delta"
! calc of B
do i=1,ngscalc
 strain(1)=mindelta+dble(i-1)*interval
 strain(2)=-(mindelta+dble(i-1)*interval)
 strain(3)=0.0d0
 strain(4)=0.0d0
 strain(5)=0.0d0
 strain(6)=0.0d0
 call findNewRprims(a1,a2,a3,strain,a1p,a2p,a3p)
 if (i<10) then
   write(str, '(i1)' )  i
 end if
 if (i>=10) then
   write(str, '(i2)' )  i
 end if
 str='rprim'//trim(str)//''
 write(6,*), str
 write(6,'(" ",3F15.10)'), a1p
 write(6,'(" ",3F15.10)'), a2p
 write(6,'(" ",3F15.10)'), a3p
 write(6,*)
enddo



endif




close(6)

contains

subroutine findNewRprims(a1,a2,a3,strain,a1p,a2p,a3p)

implicit none

!Arguments -------------------------------
!scalars

!arrays
 double precision,intent(in) :: a1(3),a2(3),a3(3),strain(6)
 double precision,intent(out) :: a1p(3),a2p(3),a3p(3)

!Local variables -------------------------
!scalars
! integer :: i,j,k,n
!arrays
 double precision:: matA(3,3),matB(3,3),matdelta(3,3)

 matdelta(1,1)=1.0d0+strain(1)
 matdelta(2,2)=1.0d0+strain(2)
 matdelta(3,3)=1.0d0+strain(3)
 matdelta(1,2)=strain(6)/2.0d0
 matdelta(2,1)=strain(6)/2.0d0
 matdelta(1,3)=strain(5)/2.0d0
 matdelta(3,1)=strain(5)/2.0d0
 matdelta(2,3)=strain(4)/2.0d0
 matdelta(3,2)=strain(4)/2.0d0
! write(6,*),i,sigma1,sigma2
 matA(1,1)=a1(1)
 matA(1,2)=a1(2)
 matA(1,3)=a1(3)
 matA(2,1)=a2(1)
 matA(2,2)=a2(2)
 matA(2,3)=a2(3)
 matA(3,1)=a3(1)
 matA(3,2)=a3(2)
 matA(3,3)=a3(3)
 matB=matmul(matA,matdelta)
 a1p(1)=matB(1,1)
 a1p(2)=matB(1,2)
 a1p(3)=matB(1,3)
 a2p(1)=matB(2,1)
 a2p(2)=matB(2,2)
 a2p(3)=matB(2,3)
 a3p(1)=matB(3,1)
 a3p(2)=matB(3,2)
 a3p(3)=matB(3,3)



end subroutine findNewRprims
end program elastic

