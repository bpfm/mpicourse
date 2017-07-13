program main

  use mpi

  implicit none

  integer :: i,N=840,ierr,rank,comm,size,start,end,recv_status(MPI_STATUS_SIZE)
  real :: pi,sum,sum_tot,sum_proc

  call MPI_INIT(ierr)
  
  comm = MPI_COMM_WORLD

  call MPI_COMM_RANK(comm,rank,ierr)
  call MPI_COMM_SIZE(comm,size,ierr)
  
  if(rank==0)then
     sum_tot = 0.0 
     do i=1,size-1
        call MPI_RECV(sum_proc,1,MPI_REAL,i,0,comm,recv_status,ierr)
        sum_tot = sum_tot + sum_proc
     enddo

     pi = 4.0*sum_tot*1.0/real(N)

     print*, "The value of pi = ", pi

  else

     sum = 0.0

     start = 1 + ((rank-1)*N/(size-1))
     end = start + N/(size-1) - 1 

     print*, "rank = ",rank,"start = ",start,"end = ",end

     do i=start,end
        sum = sum + 1.0/(1.0+((real(i)-0.5)/real(N))**2)
     enddo

     call MPI_SSEND(sum,1,MPI_REAL,0,0,comm,ierr)

  endif

  call MPI_FINALIZE(ierr)


endprogram
