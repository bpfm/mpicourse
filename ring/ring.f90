program main
  
  use mpi

  implicit none

  integer :: i,j,k,N=10
  integer :: comm,rank,size,error,status(MPI_STATUS_SIZE),req
  integer :: send_val,recv_val,left,right,total
  real*8 :: tstart,tstop

  comm = MPI_COMM_WORLD

  call MPI_INIT(error)
  call MPI_COMM_RANK(comm,rank,error)
  call MPI_COMM_SIZE(comm,size,error)

  tstart = MPI_WTIME()

  do k=1,100000

     send_val = (rank+1)**2
     total = send_val

     if(rank==0)then
        left=size-1
        right=1
     elseif(rank==size-1)then
        left=rank-1
        right=0
     else
        do i=1,size-2
           if(i==rank)then
              left=rank-1 
              right=rank+1
           endif
        enddo
     endif

     do j=0,size-2
        do i=0,size-1
           if(i==rank)then
              call MPI_ISSEND(send_val,1,MPI_INTEGER,right,0,comm,req,error)
              call MPI_RECV(recv_val,1,MPI_INTEGER,left,0,comm,status,error)
              call MPI_WAIT(req,status,error)
              total = total + recv_val
              send_val = recv_val
              print*, "from rank ", rank, " the total is ", total
           endif
        enddo
     enddo
  enddo

  tstop = MPI_WTIME()

  print*, "total time = ", tstop - tstart

endprogram
