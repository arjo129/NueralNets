! This program runs a simple network to train XOR data
! The network looks like this:
! Input vector dimension: (2 x 1)
!   Fully connected Layer one weights (2 x 2)
!   Sigmoid Activation
!   Fully connected Layer two weights (2 x 1)
!   Sigmoid Activation
module xor_net
  !import the modules
  use fully_connected

  type xornet
    type(fully_connected_adam) :: layer1
    real, dimension(2)::hidden_res, back_prop
    real, dimension(2)::in
    type(fully_connected_adam) :: layer2
    real, dimension(1)::out
  end type

contains
  subroutine initxornet(net)
    type(xornet) net
    call InitializeFullyConnectedAdam(net%layer1, 2,2)
    call InitializeFullyConnectedAdam(net%layer2, 2,1)
  end subroutine

  subroutine forwardxornet(net, input, output)
    type(xornet) net
    real, dimension(2)::input
    real, dimension(1)::output
    call forwardFCA(net%layer1,input,net%hidden_res)
    call activate_tanh(net%hidden_res,net%hidden_res)
    call forwardFCA(net%layer2,net%hidden_res,output)
    call activate_tanh(output,output)
    net%in = input
    net%out = output
  end subroutine

  subroutine backxornet(net, error)
    type(xornet) net
    real, dimension(1) :: error
    real, dimension(2) :: useless
    call back_tanh(net%out,error,error)
    call backwardUpdateFCA(net%layer2, net%hidden_res, error, net%back_prop,1.0)
    call back_tanh(net%hidden_res,net%back_prop,net%back_prop)
    call backwardUpdateFCA(net%layer1, net%in, net%back_prop, useless, 1.0)
  end subroutine
end module

program testFullyconnected
  !convnet needed for random kernel
  use xor_net
  implicit none

  !Local variables
  real, dimension(2,4) :: traininginput ! This variable declares the possible inputs for XOR
  real, dimension(4,1) :: trainingoutput ! This holds the corresponding output
  real, dimension(1)::out !this holds the temporary result.
  real, dimension(1)::error !this holds the temporary error.
  real, dimension(1)::avgerr !this holds the overall error.
  type(xornet) :: net ! This is the main network

  integer i ! used in do loop for iteration
  integer sel ! used to select the truth table row

  ! Map the truth table for XOR
  !   Input A | Input B | Output
  !     0     |   0     | 0
  !     1     |   0     | 1
  !     0     |   1     | 1
  !     1     |   1     | 0
  trainingoutput = reshape((/1,1,0,0/),(/4,1/))
  traininginput = reshape((/0,1,1,0,1,1,0,0/),(/2,4/))

  !Seed the random number generator
  call srand(84567)

  !Initialize the weights of the layers
  call initxornet(net)
  print *,"Initializing..."
  print *, net%layer1%weights
  print *, net%layer2%weights
  ! Iterate through examples
  do i = 0, 1000000
    ! Set the selector to the correct example
    sel = mod(i,4)+1
    call forwardxornet(net, traininginput(:,sel), out)
    error = out-trainingoutput(sel,:)
    call backxornet(net, error)
    avgerr = abs(error) + avgerr

    if (sel == 4) then
      !print *,i
      !print *, abs(avgerr)
      avgerr = 0
    end if
  end do

  print *, "Test:"
  call forwardxornet(net, traininginput(:,1), out)
  print *, traininginput(:,1)
  print *, out
  print *, "Test:"
  call forwardxornet(net, traininginput(:,2), out)
  print *, traininginput(:,2)
  print *, out
  print *, "Test:"
  call forwardxornet(net, traininginput(:,3), out)
  print *, traininginput(:,3)
  print *, out
  print *, "Test:"
  call forwardxornet(net, traininginput(:,4), out)
  print *, traininginput(:,4)
  print *, out
  print *, "Weights"
  print *, net%layer1%weights
  print *, net%layer2%weights
end program
