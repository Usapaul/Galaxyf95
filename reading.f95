program readsex

implicit none

character(*), parameter :: filename='catalog.cat', filecat='ellsorted.dat'
character(*), parameter :: fileregions='regions.reg', fileonly_xy='onlyxy.dat'
integer,      parameter :: pr=4, maxNobj=100000
real(pr),     parameter :: xell=0.70
type object
	integer  :: id
	real(pr) :: x, y, a, b, el
	real(pr) :: awin, bwin, ell
end type object
integer :: i, j, k, numobj=0, nhead=0
real(pr),   dimension(:,:), allocatable :: list_dump
type(object), dimension(:), allocatable :: list
integer,      dimension(:), allocatable :: sorted ! В массиве хранятся индексы сортированных элементов
character(28) :: dump
character(22), dimension(50) :: name ! Не помню, сколько всего может быть парам-ов, возьму много -- 50
logical :: redregionsyes=.TRUE., redcatalogyes=.TRUE.
type getparam
	logical :: number, x, y, a, b, e, awin, bwin
end type getparam
type(getparam) :: indicator

!--------------------------------------

open(100,file=filename,status='old')
	header: do 
		read(100,'(a28)') dump
		if (dump(1:1)=='#') then
			read(dump,'(x,x,i3,x,a22)') i, name(i)
			nhead=i
		else 
			if (nhead>0) then
				allocate(list_dump(nhead,maxNobj))
				backspace(100)
				exit header
			else
				stop '#N header lines = 0'
			end if
		end if
	end do header
	objects: do i=1,maxNobj
		read(100,*,end=999) list_dump(1:nhead,i)
		numobj=i
	end do objects
	if (numobj==maxNobj) then
		stop 'maxNobj<realNobs'
	else if (numobj==0) then
		stop 'No objects found in the catalog'
	end if 
	999 continue
close(100,status='keep')

allocate(list(numobj),sorted(numobj))

indicator=getparam(.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.)
do k=1,nhead
	select case (trim(name(k)))
		case('NUMBER')
			forall (i=1:numobj) list(i)%id=int(list_dump(k,i))
			indicator%number=.TRUE.
		case('X_IMAGE')
			forall (i=1:numobj) list(i)%x=list_dump(k,i)
			indicator%x=.TRUE.
		case('Y_IMAGE')
			forall (i=1:numobj) list(i)%y=list_dump(k,i)
			indicator%y=.TRUE.
		case('A_IMAGE')
			forall (i=1:numobj) list(i)%a=list_dump(k,i)
			indicator%a=.TRUE.
		case('B_IMAGE')
			forall (i=1:numobj) list(i)%b=list_dump(k,i)
			indicator%b=.TRUE.
		case('ELLIPTICITY')
			forall (i=1:numobj) list(i)%el=list_dump(k,i)
			indicator%e=.TRUE.
		case('AWIN_IMAGE')
			forall (i=1:numobj) list(i)%awin=list_dump(k,i)
			indicator%awin=.TRUE.
		case('BWIN_IMAGE')
			forall (i=1:numobj) list(i)%bwin=list_dump(k,i)
			indicator%bwin=.TRUE.
		case default
			continue
	end select
end do
deallocate(list_dump)
! Выполняется проверка на наличие всех необходимых данных
call checkdata(list,indicator)


! Сортировка по значениям ellipticity и последующая запись в файл
call sorting(list%el,sorted)
open(100,file=trim(filecat),    status='replace')
open(200,file=trim(fileregions),status='replace')
open(300,file=trim(fileonly_xy),status='replace')
	write(200,*) '# Region file format: DS9 version 4.1'
	write(200,*) 'global color=blue dashlist=8 3 width=1 font="helvetica 6 normal roman" &
		&select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1'
	write(200,*) 'image'
	do i=1,numobj
		j=sorted(i)
		if (any((/list(j)%el,list(j)%ell/)>=xell)) then	
			write(100,1111) list(j)%id, list(j)%el, list(j)%x, list(j)%y
			1111 format (i6,2x,f5.3,3x,f10.5,2x,f10.5)
			write(200,1112) list(j)%x, list(j)%y, list(j)%id, list(j)%el
			1112 format (" circle ",f10.5,2x,f10.5," 25 "," # text = {",i6," e=",f5.3," }")
			write(300,'(x,f10.5,2x,f10.5)') list(j)%x, list(j)%y
		end if
	end do
close(300,status='keep')
close(200,status='keep')
close(100,status='keep')

if (redregionsyes) then ! Просто вывожу разницу -- у каких объектов el-ell велико
	call sorting(list%el-list%ell,sorted)
	open(100,file='regelell.reg',status='replace')

		if (redcatalogyes) then
			open(200,file='ELxELL.dat',status='replace')
		end if

		write(100,*) '# Region file format: DS9 version 4.1'
		write(100,*) 'global color=green dashlist=8 3 width=1 font="helvetica 6 normal roman" &
			&select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1'
		write(100,*) 'image'

		do i=1,numobj
			j=sorted(i)

			if (any((/list(j)%el,list(j)%ell/)>=xell) .and. (list(j)%el-list(j)%ell>0.3.or.list(j)%el<list(j)%ell)) then		
				write(100,1121) list(j)%x, list(j)%y, list(j)%id, list(j)%el-list(j)%ell
				1121 format (" circle ",f10.5,2x,f10.5," 11 "," # color = red text = {",i6," de=",f6.3," }")

				if (redcatalogyes) then
					write(200,1122) list(j)%id, list(j)%el-list(j)%ell, list(j)%x, list(j)%y, list(j)%a, list(j)%b
					1122 format (i6,2x,f6.3,3x,f10.5,2x,f10.5,3x,f8.3,2x,f8.3)
				end if

			end if
		end do

	close(200,status='keep')
	close(100,status='keep')
end if

!====================================================================

contains

subroutine checkdata(list,il)
	implicit none

	type(object), dimension(1:), intent(inout) :: list
	type(getparam), intent(in) :: il
	integer :: i

	!----------------------------------
	if (all((/il%x,il%y/)).eqv..FALSE.) stop 'X_ or Y_IMAGE is not specified'
	if (il%number.eqv..FALSE.) then
		write(*,*) 'No numbers; assignment: numbers=1'
		list%id=1
	end if
	if (il%e.eqv..FALSE.) then
		if (all((/il%a,il%b/)).eqv..TRUE.) then
			list%el=1.0_pr-list%b-list%a
		else 
			stop 'The program needs ELLIPTICITY or A and B'
		end if
	end if

	if (all((/il%awin,il%bwin/))) then
		forall (i=1:numobj) list(i)%ell=1.0_pr-list(i)%bwin/list(i)%awin
	else
		list%ell=list%el
		redregionsyes=.FALSE.
		redcatalogyes=.FALSE.
	end if

	do i=1,numobj
		if (any((/real(list(i)%id),list(i)%x,list(i)%y,list(i)%a,list(i)%b,list(i)%el/)<0)) then
			write(*,'("The object with number ",i6," has values <0")') list(i)%id 
			stop
		end if
	end do
end subroutine checkdata

subroutine sorting(array,numbers)
	implicit none

	real(pr), dimension(1:), intent(in) :: array
	integer,  dimension(1:size(array)), intent(out) :: numbers
	logical,  dimension(1:size(array))  :: mask
	integer :: i, k, n

	n=size(array)
	!----------------------------------
	mask=.TRUE.
	do k=1,n
		numbers(k)=sum(maxloc(array,mask)) ! sum для того, чтобы превратить массив из 1 эл-та в скаляр
		mask(numbers(k))=.FALSE.
	end do
end subroutine sorting

end program readsex