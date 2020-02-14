program elastic

implicit none

integer:: st
character(50):: system,structure,source
double precision:: c(6,6),ksia,ksic,ksiv

namelist /input/ system,structure,c,source

c(:,:)=0

!Read input file
open(5)
read(5,input)
close(5)
open(6)
write(6,*) "Program Elastic written in 28 December 2010"

write(6,*) 'System    : ', system
write(6,*) 'Source    : ', source
write(6,*) 'Structure : ', structure
write(6,*)
write(6,*) 'Elastic Constants given in input file (In GPa)'
if (structure=='tetragonal') then
  write(6,'("  C11:",F10.4)'), c(1,1) 
  write(6,'("  C12:",F10.4)'), c(1,2) 
  write(6,'("  C13:",F10.4)'), c(1,3) 
  write(6,'("  C33:",F10.4)'), c(3,3) 
  write(6,'("  C44:",F10.4)'), c(4,4) 
  write(6,'("  C66:",F10.4)'), c(6,6)
  write(6,*)
  write(6,*) 'Linear and volumetric compressibilities (in 1/GPa)'
  write(6,*) '(Gurel and Eryigit J. Phys.: Condens. Matter 18 (2006) 1413–1425)'
  ksia=(c(3,3)-c(1,3))/(c(3,3)*(c(1,1)+c(1,2))-2.0d0*c(1,3)*c(1,3))
  ksic=(c(1,1)+c(1,2)-2d0*c(1,3))/(c(3,3)*(c(1,1)+c(1,2))-2.0d0*c(1,3)*c(1,3))
  ksiv=2.0d0*ksia+ksic
  write(6,'("  ksi_a= ",F12.8)'), ksia
  write(6,'("  ksi_c= ",F12.8)'), ksic
  write(6,'("  ksi_v= ",F12.8)'), ksiv
  write(6,*)
  write(6,*)'Bulk Modulus (in GPa) (taken from Phys. Scr. 82 (2010) 055601)'
  write(6,'("  Bulk Modulus (1/ksi_v) = ",F13.8)'), 1.0d0/ksiv
  write(6,'("  Bulk Modulus (Voigt)   = ",F13.8)'), (2.0d0*c(1,1)+c(3,3)+2.0d0*c(1,2)+4.0d0*c(1,3))/9.0d0
  write(6,'("  Bulk Modulus (Reuss)   = ",F13.8)'), ((c(1,1)+c(1,2))*c(3,3)-2.0d0*c(1,3)*c(1,3))/(c(1,1)+c(1,2)+2.0d0*c(3,3)-4.0d0*c(1,3))
  write(6,*)
  write(6,*) 'Some relations'
  write(6,*) ' 1-Stability criterias (Control the criterias by yourself carefully !!!!)'
  write(6,*)
  write(6,'("   c11,c33,c44,c66 > 0   : c11= ",F10.4,L"     c33= ",F10.4,L,"     c44= ",F10.4,L,"     c66= ",F10.4,L)'),c(1,1),c(1,1)>0,c(3,3),c(3,3)>0,c(4,4),c(4,4)>0,c(6,6),c(6,6)>0
  write(6,'("   c11 > |c12|           : c11= ",F10.4,"     c12= ",F10.4,L)'),c(1,1),c(1,2),c(1,1)>abs(c(1,2))
  write(6,'("   c11c33 > c13^2        : c11c33= ",F10.4,"     c13^2= ",F10.4,L)'), (c(1,1)*c(3,3)),(c(1,3)*c(1,3)),(c(1,1)*c(3,3))>(c(1,3)*c(1,3))
  write(6,'("   (c11+c12)c33 > 2c13^2 : (c11+c12)c33= ",F10.4,"     2c13^2= ",F10.4,L)'),(c(1,1)+c(1,2))*c(3,3),2.0d0*c(1,3)*c(1,3),(c(1,1)+c(1,2))*c(3,3)>2.0d0*c(1,3)*c(1,3)
  write(6,*)
  write(6,*) ' 2-Cubicness of a tetragonal structure !!!!'
  write(6,*)
  write(6,'("   c11/c33= ",F10.4)'),c(1,1)/c(3,3)
  write(6,'("   c44/c66= ",F10.4)'),c(4,4)/c(6,6)
  write(6,'("   c12/c13= ",F10.4)'),c(1,2)/c(1,3)
  write(6,*)
  write(6,*) ' 3-Anisotropic factors (taken from Phys. Scr. 82 (2010) 055601)'
  write(6,*)
  write(6,'("   alpha1 = 2c44/(c11-c12) = ",F10.4)'),2.0d0*c(4,4)/(c(1,1)-c(1,2))
  write(6,'("   alpha2 = c66/c44        = ",F10.4)'),c(6,6)/c(4,4)
  write(6,*)


endif

if (structure=='hexagonal') then
  write(6,'("  C11:",F10.4)'), c(1,1) 
  write(6,'("  C12:",F10.4)'), c(1,2) 
  write(6,'("  C13:",F10.4)'), c(1,3) 
  write(6,'("  C33:",F10.4)'), c(3,3) 
  write(6,'("  C44:",F10.4)'), c(4,4) 
  write(6,'("  C66: is equal to (c11-c12)/2 (Wu, Hou, & Zhu (2007) Solid State Commun., 143, 425)",F10.4)'), (c(1,1)-c(1,2))/2.0d0
  write(6,*)
!  write(6,*) 'Linear and volumetric compressibilities (in 1/GPa)'
!  write(6,*) '(Gurel and Eryigit J. Phys.: Condens. Matter 18 (2006) 1413–1425)'
!  ksia=(c(3,3)-c(1,3))/(c(3,3)*(c(1,1)+c(1,2))-2.0d0*c(1,3)*c(1,3))
!  ksic=(c(1,1)+c(1,2)-2d0*c(1,3))/(c(3,3)*(c(1,1)+c(1,2))-2.0d0*c(1,3)*c(1,3))
!  ksiv=2.0d0*ksia+ksic
!  write(6,'("  ksi_a= ",F12.8)'), ksia
!  write(6,'("  ksi_c= ",F12.8)'), ksic
!  write(6,'("  ksi_v= ",F12.8)'), ksiv
!  write(6,*)
  write(6,*) "Bulk Modulus (in GPa) (taken from Wu, Hou, & Zhu (2007) Solid State Commun., 143, 425)"
!  write(6,'("  Bulk Modulus (1/ksi_v) = ",F13.8)'), 1.0d0/ksiv
  write(6,'("  Bulk Modulus (Voigt)   = ",F13.8)'), (2.0d0*(c(1,1)+2.0d0*c(1,3))+c(3,3)+2.0d0*c(1,2))/9.0d0
  write(6,'("  Bulk Modulus (Reuss)   = ",F13.8)'), ((c(1,1)+c(1,2))*c(3,3)-2.0d0*c(1,3)*c(1,3))/(c(1,1)+c(1,2)+2.0d0*c(3,3)-4.0d0*c(1,3))
  write(6,*)
  write(6,*) 'Some relations'
  write(6,*) ' 1-Stability criterias (Control the criterias by yourself carefully !!!!)'
  write(6,*)
  write(6,'("   c44   :   c44= ",F10.4,L)'),c(4,4),c(4,4)>0
  write(6,'("   c11 > |c12|           : c11= ",F10.4,"     c12= ",F10.4,L)'),c(1,1),c(1,2),c(1,1)>abs(c(1,2))
  write(6,'("   c11c33 > c13^2        : c11c33= ",F10.4,"     c13^2= ",F10.4,L)'), (c(1,1)*c(3,3)),(c(1,3)*c(1,3)),(c(1,1)*c(3,3))>(c(1,3)*c(1,3))
  write(6,'("   (c11+c12)c33 > 2c13^2 : (c11+c12)c33= ",F10.4,"     2c13^2= ",F10.4,L)'),(c(1,1)+c(1,2))*c(3,3),2.0d0*c(1,3)*c(1,3),(c(1,1)+c(1,2))*c(3,3)>2.0d0*c(1,3)*c(1,3)
  write(6,*)
!  write(6,*) ' 2-Cubicness of a tetragonal structure !!!!'
!  write(6,*)
!  write(6,'("   c11/c33= ",F10.4)'),c(1,1)/c(3,3)
!  write(6,'("   c44/c66= ",F10.4)'),c(4,4)/c(6,6)
!  write(6,'("   c12/c13= ",F10.4)'),c(1,2)/c(1,3)
!  write(6,*)
!  write(6,*) ' 3-Anisotropic factors (taken from Phys. Scr. 82 (2010) 055601)'
!  write(6,*)
!  write(6,'("   alpha1 = 2c44/(c11-c12) = ",F10.4)'),2.0d0*c(4,4)/(c(1,1)-c(1,2))
!  write(6,'("   alpha2 = c66/c44        = ",F10.4)'),c(6,6)/c(4,4)
  write(6,*)


endif


if (structure=='cubic') then
  write(6,*) 'Cubic is not ready yet... Contact with devoloper tgurel@nku.edu.tr'
endif

write(6,*)
write(6,*) 'Program Elastic Ends!!'
close(6)
end program elastic
