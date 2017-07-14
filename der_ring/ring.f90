program main
  
  use mpi

  implicit none

  integer :: i,j,k,N=10
  integer :: comm,rank,size,error,status(MPI_STATUS_SIZE),req
  integer :: left,right,itotal,dims(1),comm_cart,extent,MPI_COMPOUND
  logical :: periods(1)=.TRUE.,reorder
  real*8 :: tstart,tstop,dtotal
  integer(kind=mpi_address_kind) :: disp

  type compound
     integer :: ival
     real*8 :: dval
  end type compound

  type(compound) :: send_val,recv_val

  comm = MPI_COMM_WORLD

  call MPI_INIT(error)
  call MPI_COMM_RANK(comm,rank,error)
  call MPI_COMM_SIZE(comm,size,error)

  call MPI_DIMS_CREATE(size,1,dims,error)
  call MPI_CART_CREATE(comm,1,dims,periods,reorder,comm_cart,error)
  
  call MPI_TYPE_EXTENT(MPI_DOUBLE_PRECISION,extent,error)

  disp = extent

  call MPI_TYPE_CREATE_STRUCT(2,[1,1],[0_mpi_address_kind,disp],[MPI_INTEGER,MPI_DOUBLE_PRECISION],MPI_COMPOUND,error)

  call MPI_TYPE_COMMIT(MPI_COMPOUND,error)

  tstart = MPI_WTIME()

  do k=1,1

     send_val%ival = rank
     send_val%dval = dble(rank)
     itotal = send_val%ival
     dtotal = send_val%dval

     do i=0,size-1
        if(i==rank)then
           call MPI_CART_SHIFT(comm_cart,0,1,left,right,error)
        endif
     enddo

     do j=0,size-2
        do i=0,size-1
           if(i==rank)then
              call MPI_ISSEND(send_val,1,MPI_COMPOUND,right,0,comm,req,error)
              call MPI_RECV(recv_val,1,MPI_COMPOUND,left,0,comm,status,error)
              call MPI_WAIT(req,status,error)
              itotal = itotal + recv_val%ival
              dtotal = dtotal + recv_val%dval
              send_val = recv_val
              !print*, "from rank ", rank, " the total is ", total
           endif
        enddo
     enddo

     print*, "Finished Rank = ", rank, " total =  ", itotal, dtotal

  enddo

  tstop = MPI_WTIME()

  print*, "total time = ", tstop - tstart

endprogram
