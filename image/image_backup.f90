program main
  use mpi
  use pgmio
  implicit none

  integer :: comm,rank,size,error
  integer :: i,j,k,nx,ny,n_iter=10000
  real,allocatable :: buf(:,:),new(:,:),old(:,:),edge(:,:)

  comm = MPI_COMM_WORLD

  call MPI_INIT(error)

  call pgmsize('edge768x768.pgm',nx,ny)
  
  allocate(buf(nx,ny))
  allocate(new(0:nx+1,0:ny+1))
  allocate(old(0:nx+1,0:ny+1))
  allocate(edge(nx,ny))

  call pgmread('edge768x768.pgm',buf)

  old = 255.0
  edge = buf

  do k=1,n_iter
     do i=1,nx
        do j=1,ny
           new(i,j)=0.25*(old(i-1,j)+old(i+1,j)+old(i,j+1)+old(i,j-1)-edge(i,j))
        enddo
     enddo
     old = new
  enddo

  buf = old(1:nx,1:ny)

  call pgmwrite('picture.pgm',buf)

endprogram
