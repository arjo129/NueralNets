program testMaxpool
  use convnet
  !implicit none
  real, dimension(2,4)::array,test_result
  real, dimension(2,2,2)::testarr
  !data array(1,:) /0,0,0,0/
  !data array(2,:) /0,0,0,0/
  real, dimension(1,2)::out
  complex, dimension(1,2)::points
  data array(1,:) /0,2,9,0/
  data array(2,:) /1,1,1,1/
  call fillWithZero(test_result)
  call maxpool(array,2,out,points)
  print *, out
  print *, points
  call back_maxpool(out,points,test_result)
  print *,"Result"
  print *,test_result(1,:)
  print *,test_result(2,:)
end program testMaxpool
