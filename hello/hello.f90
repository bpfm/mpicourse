program hello

  use mpi

  implicit none

  integer :: ierr
  integer :: comm,rank,proc

  call MPI_Init(ierr)

  !write(*,*) 'Hello World!'

  comm = MPI_COMM_WORLD

  call MPI_COMM_RANK(comm,rank,ierr)
  call MPI_COMM_SIZE(comm,proc,ierr)

  print*, "rank of this processor = ", rank, "out of ", proc

  if(rank==0) print*, "I am the Master"

  call MPI_FINALIZE(ierr)

end program hello
