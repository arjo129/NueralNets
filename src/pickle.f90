module pickle

  subroutine loadMatrix(file,matrix)
    integer::file
    real, dimension(:,:)::matrix
    integer, dimension(:), allocatable::shape_mat
    integer::i,ma
    shape_mat = shape(matrix)
    ma = shape_mat(1)
    do i = 1,ma
      read (file), matrix(i,:)
    end do
  end subroutine

  subroutine saveMatrix(file,matrix)
    integer::file
    real, dimension(:,:)::matrix
    integer, dimension(:), allocatable::shape_mat
    integer::i,ma
    shape_mat = shape(matrix)
    ma = shape_mat(1)
    do i = 1,ma
      write (file), matrix(i,:)
    end do
  end subroutine

  subroutine loadKernel(file,matrix)
    integer::file
    real(:,:,:,:)::matrix
    integer, allocatable shape_mat
    integer i,j,k
    shape_mat = shape(matrix)
    do i = 0,shape_mat(1)
      do j = 0,shape_mat(2)
        do k = 0,shape_mat(3)
          read (file) matrix(i,j,k,:)
        end do
      end do
    end do
  end subroutine
  subroutine loadKernel(file,matrix)
    integer::file
    real(:,:,:,:)::matrix
    integer, allocatable shape_mat
    integer i,j,k
    shape_mat = shape(matrix)
    do i = 0,shape_mat(1)
      do j = 0,shape_mat(2)
        do k = 0,shape_mat(3)
          write (file) matrix(i,j,k,:)
        end do
      end do
    end do
  end subroutine
end module
