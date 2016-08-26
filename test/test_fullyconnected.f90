! This program runs a simple network to train XOR data
! The network looks like this:
! Input vector dimension: (2 x 1)
!   Fully connected Layer one weights (2 x 2)
!   Sigmoid Activation
!   Fully connected Layer two weights (2 x 1)
!   Sigmoid Activation

program testFullyconnected
  !import the modules
  use fully_connected

  implicit none

  !Local variables
  real, dimension(2,4) :: traininginput ! This variable declares the possible inputs for XOR
  real, dimension(4,1) :: trainingoutput ! This holds the corresponding output
  real, dimension(2) :: layerone_res !Layer one result
  real, dimension(2) :: layerone_e !The errors propafated from layer two to layer one
  real, dimension(2) :: useless !useless as the name implies.
  real, dimension(2,2) :: layerone ! weights for layer one
  real, dimension(2,2) :: layerone_w ! error correction for layer one
  real, dimension(2,1) :: layertwo ! weights for layer two
  real, dimension(2,1) :: layertwo_w ! error correction for layer two
  real, dimension(1) :: output ! output of network
  real, dimension(1) :: error ! error of output
  real, dimension(1) ::avgerr ! used to measure overall network error
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
  call randkernel(layerone)
  call randkernel(layertwo)
  print *, ""
  print *, ""
  print *, ""
  ! Iterate through examples
  do i = 0, 10000
    ! Set the selector to the correct example
    sel = mod(i,4)+1

    ! First fully connected layer
    call forward_linear(traininginput(:,sel),layerone,layerone_res)
    call activate_sigmoid(layerone_res,layerone_res)

    ! Second fully connected layer
    call forward_linear(layerone_res,layertwo,output)
    call activate_sigmoid(output,output)

    ! Calculate the error
    error = trainingoutput(sel,:) - output
    avgerr = abs(error) + avgerr

    ! Print the error every 4 cycles
    if (sel == 4) then
      print *, abs(avgerr)
      avgerr = 0
    end if

    ! Back propagate layer 2
    call back_sigmoid(output,error,error)
    call backward_linear(layerone_res, layertwo, error, layerone_e, layertwo_w)

    ! Back propagate layer 1
    call back_sigmoid(layerone_res, layerone_e, layerone_e)
    call backward_linear(traininginput(:,sel), layerone, layerone_e, useless, layerone_w)

    ! Update weights
    layertwo =  layertwo + layertwo_w*0.1
    layerone =  layerone + layerone_w*0.1
  end do

  ! Run tests... lets find out the truth table
  call forward_linear(traininginput(:,4),layerone,layerone_res)
  call activate_sigmoid(layerone_res,layerone_res)
  call forward_linear(layerone_res,layertwo,output)
  call activate_sigmoid(output,output)
  !print *,"Test:"
  !print *,traininginput(:,4)
  !print *,output

  call forward_linear(traininginput(:,3),layerone,layerone_res)
  call activate_sigmoid(layerone_res,layerone_res)
  call forward_linear(layerone_res,layertwo,output)
  call activate_sigmoid(output,output)
  !print *,"Test:"
  !print *,traininginput(:,3)
  !print *,output

  call forward_linear(traininginput(:,2),layerone,layerone_res)
  call activate_sigmoid(layerone_res,layerone_res)
  call forward_linear(layerone_res,layertwo,output)
  call activate_sigmoid(output,output)
  !print *,"Test:"
  !print *,traininginput(:,2)
  !print *,output

  call forward_linear(traininginput(:,1),layerone,layerone_res)
  call activate_sigmoid(layerone_res,layerone_res)
  call forward_linear(layerone_res,layertwo,output)
  call activate_sigmoid(output,output)
  !print *,"Test:"
  !print *,traininginput(:,1)
  !print *,output

end program
