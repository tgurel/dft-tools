program extractphonons
implicit none

integer:: natom,io
!character(50):: system,lattice,str
!double precision:: a1(3),a2(3),a3(3),maxdelta,mindelta,interval,a1p(3),a2p(3),a3p(3),strain(6)


!  DOUBLE PRECISION:: DW,D
!  INTEGER::N,P,T

!DEFINE BUFFER HOLDS THE COMMAND LINE ARGUMENT
  CHARACTER(100):: BUFFER,line
  CHARACTER(30):: file1,file2

!GET THE PARAMETERS FROM THE COMMAND LINE ARGUMENT
    CALL GETARG(1,BUFFER)
    READ(BUFFER,*) natom
    CALL GETARG(2,BUFFER)
    READ(BUFFER,*) file1
    CALL GETARG(3,BUFFER)
    READ(BUFFER,*) file2
write(*,*) "result===>",natom,file1,file2

open(unit=10,file=file1)
do
 do i=1,natom*3
  read (10,*),  


 enddo
enddo


DO
   READ(10,'(A)',IOSTAT=io)  x
   IF (io > 0) THEN
      WRITE(*,*) 'Check input.  Something was wrong'
      EXIT
   ELSE IF (io < 0) THEN
      WRITE(*,*)  'The total is ', sum
      EXIT
   ELSE
      sum = sum + x
   END IF
END DO



end program extractphonons
