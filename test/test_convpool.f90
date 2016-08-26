program testConv1layer
  use convnet
  real, dimension(3,640,480)::input_image, back_prop
  real, dimension(15,3,15,15)::kernels, w_error
  real, dimension(640,480)::cache
  real, dimension(15,640,480)::outconv, e_cache
  real, dimension(15,40,30)::error,output
  complex, dimension(15,40,30)::pointlist
  call initConvPool(kernels)
  call runConvPool(input_image, kernels, 16, cache,outconv, output,pointlist)
  print *,"back propagating"
  call backConvPool(input_image, output, error, kernels, w_error, e_cache, pointlist, back_prop)
end program
