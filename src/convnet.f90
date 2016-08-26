!! This package contains primitives to work with convolution networks
module convnet
  implicit none

  type ConvPool
    real, dimension(:,:,:), allocatable::input_image, back_prop
    real, dimension(:,:,:,:), allocatable::kernels, w_error
    real, dimension(:,:), allocatable::cache
    real, dimension(:,:,:), allocatable::outconv, e_cache
    real, dimension(:,:,:), allocatable::error,output
  end type
contains
  subroutine convolve(input, kernel, output, success)
    !Arguments
    real,dimension(:,:) :: input,kernel
    real,dimension(:,:),intent(inout) :: output
    logical,intent(out) :: success
    !Local variables
    integer :: i,j,k,m,bounds
    integer,dimension(:),allocatable :: kernel_shape, input_shape
    !Initialize
    success = .FALSE.
    kernel_shape = shape(kernel)
    input_shape = shape(input)
    bounds = (kernel_shape(1) - 1)/2
    do i = bounds,input_shape(1)-bounds
      do j = bounds,input_shape(2)-bounds
        do k = -bounds,bounds
          do m = -bounds,bounds
              output(i,j) = kernel(k+bounds+1,m+bounds+1)*input(i+k,j+m)+output(i,j)
          end do
        end do
      end do
    end do
    success = .TRUE.
  end subroutine convolve

  subroutine convolve_flipped(input, kernel, output, success)
    !Arguments
    real,dimension(:,:) :: input,kernel
    real,dimension(:,:),intent(inout) :: output
    logical,intent(out) :: success
    !Local variables
    integer :: i,j,k,m,bounds
    integer,dimension(:),allocatable :: kernel_shape, input_shape
    !Initialize
    success = .FALSE.
    kernel_shape = shape(kernel)
    input_shape = shape(input)
    bounds = (kernel_shape(1) - 1)/2
    do i = bounds,input_shape(1)-bounds
      do j = bounds,input_shape(2)-bounds
        do k = -bounds,bounds
          do m = -bounds,bounds
            output(i,j) = kernel(k+bounds+1,m+bounds+1)*input(i-k,j-m)+output(i,j)
          end do
        end do
      end do
    end do
    success = .TRUE.
  end subroutine convolve_flipped

  subroutine back_convolve(input, errors, output, success)
    !Arguments
    real, dimension(:,:)::input,errors
    real, dimension(:,:),intent(inout)::output
    logical, intent(out)::success
    !Local variables
    integer :: i,j,k,m,bounds
    integer,dimension(:),allocatable :: kernel_shape, input_shape
    real :: totalerr

    success = .FALSE.
    kernel_shape = shape(output)
    input_shape = shape(input)
    bounds = (kernel_shape(1)-1)/2
    do k = -bounds,bounds
      do m = -bounds,bounds
        totalerr = 0
        do i = bounds,input_shape(1)-bounds
          do j = bounds,input_shape(2)-bounds
            totalerr = input(i,j)*errors(i+k,j+m)
          end do
        end do
        output(k+bounds,m+bounds) = totalerr
      end do
    end do
    success = .TRUE.
  end subroutine back_convolve

  subroutine maxpool(input,stride,output,pointlist)
    !Arguments
    real, dimension(:,:)::input
    real, dimension(:,:), intent(out):: output
    complex, dimension(:,:), intent(out):: pointlist
    integer::stride
    !Local variables
    integer,dimension(:),allocatable :: input_shape, output_shape, tmparr
    integer::i,j,k,l,max
    max = 0
    input_shape = shape(input)
    output_shape = shape(output)
    !print *,stride
    do i = 1, output_shape(1)
      do j = 1, output_shape(2)
        output(i,j) = maxval(input((i-1)*stride+1:i*stride, (j-1)*stride+1:j*stride))
        tmparr =  maxloc(input((i-1)*stride+1:i*stride, (j-1)*stride+1:j*stride))
        pointlist(i,j) = cmplx((i-1)*stride+1+tmparr(1)-1, (j-1)*stride+1+tmparr(2)-1)
      end do
    end do
  end subroutine maxpool

  subroutine back_maxpool(error, pointlist, output)
    !Arguments
    real, dimension(:,:)::error
    real, dimension(:,:), intent(out):: output
    complex, dimension(:,:), intent(out):: pointlist
    !Local variables
    integer::i,j
    integer, dimension(:), allocatable::size_error
    complex::maxpt
    size_error = shape(error)
    do i = 1,size_error(1)
      do j = 1,size_error(2)
        maxpt = pointlist(i,j)
        !print *, maxpt
        output(int(real(maxpt)),int(aimag(maxpt))) = error(i,j)
      end do
    end do
  end subroutine

  subroutine randkernel(kernel)
    real, dimension(:,:), intent(inout)::kernel
    integer:: i,j
    integer, dimension(:), allocatable::size_kern
    size_kern = shape(kernel)
    do i= 1,size_kern(1)
      do j=1,size_kern(2)
        kernel(i,j) = 0.5-rand()
      end do
    end do
  end subroutine

  subroutine fillWithZero(matrix)
    real, dimension(:,:)::matrix
    integer, dimension(:), allocatable :: shape_mat
    integer i,j
    shape_mat = shape(matrix)
    do i = 1,shape_mat(1)
      do j = 1,shape_mat(2)
        matrix(i,j) = 0
      end do
    end do
  end subroutine

  subroutine initConvPool(weights)
    real, dimension(:,:,:,:)::weights
    integer::input_channels, outputchannels,i,j
    integer, dimension(:), allocatable::weight_dim
    weight_dim = shape(weights)
    input_channels = weight_dim(2)
    outputchannels = weight_dim(1)
    do i=1,outputchannels
      do j=1,input_channels
        call randkernel(weights(i,j,:,:))
      end do
    end do
  end subroutine

  elemental subroutine reLU(input)
    real, intent(inout)::input
    if (input<0) then
      input = 0
    end if
  end subroutine

  subroutine backConvPool(input, output,error, weights, w_error, e_cache,pointlist,back_prop)
    real, dimension(:,:,:)::  input, output, error, back_prop, e_cache
    real, dimension(:,:,:,:):: weights, w_error
    complex, dimension(:,:,:)::pointlist
    !local variablex
    integer input_channels, output_channels, i, j
    integer, dimension(:), allocatable::shape_in, shape_out
    logical success
    shape_in = shape(input)
    input_channels = shape_in(1)
    shape_out = shape(output)
    output_channels = shape_out(1)
    do i = 1,output_channels
      call back_maxpool(error(i,:,:),pointlist(i,:,:),e_cache(i,:,:))
      do j = 1,input_channels
        call back_convolve(input(j,:,:), e_cache(i,:,:), w_error(i,j,:,:), success)
        call convolve_flipped(error(i,:,:),weights(i,j,:,:),back_prop(j,:,:), success)
      end do
    end do
  end subroutine

  subroutine runConvPool(input,weights,stride, out_cache,output_conv, output_pool, pointlist)
    !Arguments
    real, dimension(:,:,:,:)::weights
    real, dimension(:,:,:)::input
    real, dimension(:,:,:)::output_conv, output_pool
    real, dimension(:,:)::out_cache
    complex, dimension(:,:,:), intent(out):: pointlist
    integer::stride
    !Local variables
    integer, dimension(:), allocatable::input_size
    integer::inpchannels,outpchannels,i,j
    logical::success
    input_size = shape(weights)
    outpchannels = input_size(1)
    inpchannels = input_size(2)
    do i = 1,outpchannels
      do j = 1,inpchannels
        call convolve(input(j,:,:),weights(i,j,:,:),out_cache,success)
        call reLU(out_cache)
        call maxpool(out_cache, stride, output_pool(i,:,:), pointlist(i,:,:))
        output_conv(i,:,:) = output_conv(i,:,:) + out_cache
      end do
    end do
  end subroutine

  subroutine runConvRelu(input,weights,stride, out_cache, output_conv, output_pool)
    !Arguments
    real, dimension(:,:,:,:)::weights
    real, dimension(:,:,:)::input
    real, dimension(:,:,:), target::output_conv, output_pool
    real, dimension(:,:)::out_cache
    integer::stride
    !Local variables
    integer, dimension(:), allocatable::input_size
    integer::inpchannels,outpchannels,i,j
    logical::success
    input_size = shape(weights)
    outpchannels = input_size(1)
    inpchannels = input_size(2)
    do i = 1,outpchannels
      do j = 1,inpchannels
        call convolve(input(j,:,:),weights(i,j,:,:),out_cache,success)
        call reLU(out_cache)
        output_conv(i,:,:) = output_conv(i,:,:) + out_cache
      end do
    end do
  end subroutine

end module
