program main
  
  use mpi

  implicit none

  integer :: i,j,k,N=10
  integer :: comm,rank,size,error,status(MPI_STATUS_SIZE),req
  integer :: send_val,recv_val,left,right,total,dims(1),comm_cart
  logical :: periods(1)=.FALSE.,reorder
  real*8 :: tstart,tstop

  comm = MPI_COMM_WORLD

  call MPI_INIT(error)
  call MPI_COMM_RANK(comm,rank,error)
  call MPI_COMM_SIZE(comm,size,error)

  call MPI_DIMS_CREATE(size,1,dims,error)
  call MPI_CART_CREATE(comm,1,dims,periods,reorder,comm_cart,error)
  
  tstart = MPI_WTIME()

  do k=1,1

     send_val = rank
     total = send_val

     do i=0,size-1
        if(i==rank)then
           call MPI_CART_SHIFT(comm_cart,0,1,left,right,error)
        endif
     enddo

     do j=0,size-2
        do i=0,size-1
           if(i==rank)then
              call MPI_ISSEND(send_val,1,MPI_INTEGER,right,0,comm,req,error)
              call MPI_RECV(recv_val,1,MPI_INTEGER,left,0,comm,status,error)
              call MPI_WAIT(req,status,error)
              total = total + recv_val
              send_val = recv_val
              !print*, "from rank ", rank, " the total is ", total
           endif
        enddo
     enddo

     print*, "Finished Rank = ", rank, " total =  ", total

  enddo

  tstop = MPI_WTIME()

  print*, "total time = ", tstop - tstart

endprogram
