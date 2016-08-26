module fully_connected
  use convnet

  implicit none

  type fully_connected_adam
    real, dimension(:,:), allocatable::weights
    real, dimension(:,:), allocatable::m,v,dweights
  end type

contains
  elemental subroutine activate_sigmoid(input,output)
    real, intent(in):: input
    real, intent(out):: output
    output = 1 / (1 + exp(-input))
  end subroutine

  elemental subroutine back_sigmoid(output,  error, error_bprop)
    real, intent(in):: output, error
    real, intent(out):: error_bprop
    error_bprop = error*output*(1-output)
  end subroutine

  elemental subroutine activate_tanh(input,output)
    real, intent(in):: input
    real, intent(out):: output
    call activate_sigmoid(2*input, output)
    output = 2*output - 1
  end subroutine

  elemental subroutine back_tanh(output,  error, error_bprop)
    real, intent(in):: output, error
    real, intent(out):: error_bprop
    error_bprop = error*(1-output*output)
  end subroutine

  pure subroutine forward_linear(input, weights, output)
    real,dimension(:), intent(in)::input
    real,dimension(:), intent(out) ::output
    real,dimension(:,:), intent(in)::weights
    output = matmul(input,weights)
  end subroutine

  pure subroutine backward_linear(input, weights, err_output, error_bprop, error_weights)
    real, dimension(:), intent(in)::input, err_output
    real, dimension(:), intent(out) ::error_bprop
    real, dimension(:,:), intent(in)::weights
    real, dimension(:,:), intent(out)::error_weights
    integer, dimension(:), allocatable::shape_w
    integer::i, max_w
    shape_w = shape(weights)
    max_w = shape_w(2)
    error_bprop = matmul(err_output,transpose(weights))
    do i = 1, max_w
      error_weights(:,i) = input*err_output(i)
    end do
  end subroutine

  subroutine InitializeFullyConnectedAdam(weight, inputs, outputs)
    type(fully_connected_adam) :: weight
    integer :: inputs, outputs
    allocate(weight%weights(inputs,outputs))
    allocate(weight%m(inputs,outputs))
    allocate(weight%v(inputs,outputs))
    allocate(weight%dweights(inputs,outputs))
    call fillWithZero(weight%m)
    call fillWithZero(weight%v)
    call fillWithZero(weight%dweights)
    call randkernel(weight%weights)
    weight%weights = weight%weights * 2/sqrt(real(inputs))
  end subroutine

  subroutine forwardFCA(weights,input, output)
    type(fully_connected_adam) :: weights
    real, dimension(:) :: input, output
    call forward_linear(input, weights%weights, output)
  end subroutine

  subroutine backwardUpdateFCA(weights, input, error_in, error_bprop, learning_rate)
    type(fully_connected_adam) :: weights
    real, dimension(:) :: input, error_in, error_bprop
    real :: learning_rate
    call backward_linear(input, weights%weights, error_in, error_bprop, weights%dweights)
    weights%m = weights%m*0.8 + weights%dweights*0.2
    weights%v = weights%v*0.99 + 0.01*(weights%dweights**2)
    where(weights%v == 0) weights%v = 1e-3
    weights%weights = weights%weights - learning_rate*weights%m/(sqrt(weights%v))
  end subroutine

  elemental subroutine adjusterror(weight, error, randomNoise)
    real, intent(in) :: weight, randomNoise
    real, intent(inout) :: error
    if((weight > 0.99).AND.(error>0)) error = 0
    if((weight < -0.99).AND.(error<0)) error = 0
    if(abs(error) > 1) then
      error = error/REAL(NINT(LOG10(error)))
    end if
  end subroutine

  subroutine backwardUpdateFCB(weights, input, error_in, error_bprop, learning_rate)
    type(fully_connected_adam) :: weights
    real, dimension(:) :: input, error_in, error_bprop
    real :: learning_rate
    call backward_linear(input, weights%weights, error_in, error_bprop, weights%dweights)
    call adjusterror(weights%weights,weights%dweights,rand())
    weights%weights = weights%weights - weights%dweights
  end subroutine

  subroutine backwardUpdateFCSGD(weights, input, error_in, error_bprop, learning_rate)
    type(fully_connected_adam) :: weights
    real, dimension(:) :: input, error_in, error_bprop
    real :: learning_rate
    call backward_linear(input, weights%weights, error_in, error_bprop, weights%dweights)
    weights%weights = weights%weights - learning_rate*weights%dweights
  end subroutine

end module
