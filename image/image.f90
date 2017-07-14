program main
  use mpi
  use pgmio
  implicit none

  integer :: comm,rank,size,error
  integer :: i,j,k,nx,ny,nx_p,ny_p,n_iter=1000
  integer,parameter :: P=8
  real,allocatable :: master_buf(:,:),buf(:,:),new(:,:),old(:,:),edge(:,:)

  comm = MPI_COMM_WORLD

  call MPI_INIT(error)
  call MPI_COMM_RANK(comm,rank,error)
  call MPI_COMM_SIZE(comm,size,error)

  if(size/=P)then
     print*,'ERROR: size not equal to P, size is', size
     stop
  else
     print*,'using P = ', P, 'processors'
  endif

  call pgmsize('edge192x128.pgm',nx,ny)

  nx_p = nx
  ny_p = ny/P

  allocate(master_buf(nx,ny))

  if(rank==0)then
     call pgmread('edge192x128.pgm',master_buf)
  endif

  print*, 'allocating on ', rank

  allocate(buf(nx_p,ny_p))
  allocate(new(0:nx_p+1,0:ny_p+1))
  allocate(old(0:nx_p+1,0:ny_p+1))
  allocate(edge(nx_p,ny_p))

  call MPI_SCATTER(master_buf,nx_p*ny_p,MPI_REAL,buf,nx_p*ny_p,MPI_REAL,0,comm,error)

  old = 255.0
  edge = buf

  do k=1,n_iter
     do i=1,nx_p
        do j=1,ny_p
           new(i,j)=0.25*(old(i-1,j)+old(i+1,j)+old(i,j+1)+old(i,j-1)-edge(i,j))  
        enddo
     enddo
     old = new
  enddo

  buf = old(1:nx_p,1:ny_p)

  call MPI_GATHER(buf,nx_p*ny_p,MPI_REAL,master_buf,nx_p*ny_p,MPI_REAL,0,comm,error)

  if(rank==0)then
     call pgmwrite('picture.pgm',master_buf)
  endif

  call MPI_FINALIZE(error)

endprogram
