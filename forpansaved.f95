program looking

implicit none

integer, parameter :: pr=4, maxObj=10000, comcol=5, xcol=3, ycol=4, ecol=2, symbolsdata=39
character(*), parameter :: filecat='checked.dat', fileonlyxy='onlyxy.dat'
character(*), parameter :: fileinit='ellsorted.dat', fileregions='savedgal.reg'
character(200), dimension(maxObj) :: catlist
type xycomment
	real(pr) :: e, x, y
	character(200) :: comment
end type xycomment
type(xycomment), dimension(maxObj) :: line
real(pr), dimension(comcol) :: dump
integer :: numobj, i, j, k

!--------------------------------------

open(100,file=filecat,status='old')
open(200,file=fileinit,status='replace')
open(300,file=fileregions,status='replace')
	write(300,*) '# Region file format: DS9 version 4.1'
	write(300,*) 'global color=blue dashlist=8 3 width=1 font="helvetica 8 normal roman" &
		&select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1'
	write(300,*) 'image'
	numobj=0
	do 
		j=numobj+1
		read(100,'(*(a))',end=999) catlist(j)
		write(200,*) catlist(j)(1:symbolsdata)
		read(catlist(j)(1:symbolsdata),*) dump(1:ecol-1), line(j)%e, line(j)%x, line(j)%y, dump(ycol+1:comcol-1)
		read(catlist(j)(symbolsdata+1:len(catlist(1))),'(*(a))') line(j)%comment
			line(j)%comment=adjustl(line(j)%comment)
		write(300,1112) line(j)%x, line(j)%y, line(j)%e, trim(line(j)%comment)//' }'
			1112 format (" circle ",f10.5,2x,f10.5," 25"," # text = { e=",f6.3," comment: ",*(a)) 
		numobj=numobj+1
	end do
	999 continue
close(300,status='keep')
close(200,status='keep')
close(100,status='keep')

open(400,file=fileonlyxy,status='replace')
	do j=1,numobj
		write(400,*) line(j)%x, line(j)%y
	end do
close(400,status='keep')

end program looking