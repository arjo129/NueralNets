all: tests

tests: testFullyconnected

testFullyconnected:
	gfortran ../src/fully_connected.f90 ../src/convnet.f90 test_fullyconnected.f90  -o ../bin/tests/testFullyconnected

testConvPool:
	gfortran ../src/convnet.f90 test_convpool.f90 -o ../bin/tests/testFullyconnected

testFCadam:
	gfortran ../src/convnet.f90 ../src/fully_connected.f90 test_fullyconnected_adam.f90  -o ../bin/tests/testFullyconnected_adam

test1DConv:
	
