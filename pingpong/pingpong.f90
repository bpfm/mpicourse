program main
  use mpi

  implicit none

  integer :: comm,ierr,size,i,rank,size,status(mpi_status_size)
  integer :: buffer(100)

  comm = mpi_comm_world

  call mpi_init(ierr)
  call mpi_comm_size(comm,size,ierr)
  call mpi_comm_rank(comm,rank,ierr)
  
  buffer = 123

  print*, rank

  do i=1,100
      if(rank==0)then
        print*, "starting"
        call mpi_ssend(buffer,100,mpi_integer,1,0,comm,ierr)
        print*, "sent from 0"
        call mpi_recv(buffer,100,mpi_integer,0,0,comm,status,ierr)
        print*, "received from 1"
     elseif(rank==1)then
        call mpi_recv(buffer,100,mpi_integer,1,0,comm,status,ierr)
        print*, "received from 0"
        call mpi_ssend(buffer,100,mpi_integer,0,0,comm,ierr)
        print*, "sent from 1"
     endif
  enddo


endprogram
