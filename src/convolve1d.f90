subroutine convolve1d(input, kernel, output)
    real, dimension(:), intent(in):: input, kernel
    real, dimension(:), intent(out):: output
    integer, dimension(:)::shapekern, shapein
    integer:: i,j
    shapein = shape(input)
    shapekern = shape(kernel)
    do i = (shapekern(1)-1)/2,shapein(1) - (shapekern(1)-1)/2
      do j = -(shapekern(1)-1)/2,(shapekern(1)-1)/2
        output(i) = output(i)+kernel(j+(shapekern(1)-1)/2)*input(i+j)
      end do
    end do
end subroutine

subroutine convolve1d_flipped(input, kernel, output)
  real, dimension(:), intent(in):: input, kernel
  real, dimension(:), intent(out):: output
  integer, dimension(:)::shapekern, shapein
  integer:: i,j
  shapein = shape(input)
  shapekern = shape(kernel)
  do i = (shapekern(1)-1)/2,shapein(1) - (shapekern(1)-1)/2
    do j = -(shapekern(1)-1)/2,(shapekern(1)-1)/2
      output(i) = output(i)+kernel(j+(shapekern(1)-1)/2)*input(i-j)
    end do
  end do
end subroutine
