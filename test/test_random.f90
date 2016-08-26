program testConvnet
  use convnet
  implicit none
  real, dimension(15,15):: kernel
  call srand(85697)
  call randkernel(kernel)
end program testConvnet
