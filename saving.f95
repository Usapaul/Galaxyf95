program savesex

implicit none

character(*), parameter :: filecat='ellsorted.dat', filecom='comments.dat', filefound='foundlist.dat'
integer,      parameter :: pr=4, maxObj=10000
character(300), dimension(maxObj) :: catlist, commentlist
integer :: i, j, k, numobj

!--------------------------------------

open(100,file=filecat,status='old')
open(200,file=filecom,status='old')
	numobj=0
	do 
		j=numobj+1
		read(100,'(*(a))',end=999) catlist(j)
		read(200,'(*(a))') commentlist(j)
		numobj=numobj+1
	end do
	999 continue
close(200,status='keep')
close(100,status='keep')


open(300,file=filefound,status='replace')
	call printing('ok')
	call printing('--')	
	call printing()
close(300,status='keep')

!====================================================================

contains

subroutine printing(word)
	implicit none

	character(*), optional, intent(in) :: word
	logical :: notno
	integer :: j
	character(20), dimension(0:30), save :: savedwords
	integer, save :: numsaved=0

	if (present(word)) then
		numsaved=numsaved+1
		savedwords(numsaved)=word
		do j=1,numobj
			notno=all((/'no','No','NO'/)/=trim(commentlist(j)))
			if (notno.and.trim(commentlist(j))==word) then
				write(300,*) trim(catlist(j)), '  ', trim(commentlist(j))
			end if
		end do
	else
		do j=1,numobj
			notno=all((/'no','No','NO'/)/=trim(commentlist(j)))
			if (notno.and.all(savedwords(1:numsaved)/=trim(commentlist(j)))) then
				write(300,*) trim(catlist(j)), '  ', trim(commentlist(j))
			end if
		end do
	end if
end subroutine printing

end program savesex