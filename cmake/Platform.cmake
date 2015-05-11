# Determine platform- and compiler-specific settings

# check for and activate c++11 support
include(${PROJECT_SOURCE_DIR}/cmake/CXX11.cmake)
check_for_cxx11_compiler(CXX11_COMPILER)
if(CXX11_COMPILER)
  enable_cxx11()
  ADD_DEFINITIONS("-DCPP11=1")
else(CXX11_COMPILER)
  MESSAGE(FATAL_ERROR "Your C++ compiler version (${CMAKE_CXX_COMPILER_ID} ${COMPILER_VERSION}) is not compatible with C++11. Please use a fully C++11 compliant compiler -- see the EUDAQ documentation for more information.")
endif(CXX11_COMPILER)

# determine compiler type (32/64bit)
if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
#    MESSAGE(STATUS "64-bit compiler detected" )
    SET( EX_PLATFORM 64 )
    SET( EX_PLATFORM_NAME "x64" )
else( CMAKE_SIZEOF_VOID_P EQUAL 8 ) 
#    MESSAGE(STATUS "32-bit compiler detected" )
    SET( EX_PLATFORM 32 )
    SET( EX_PLATFORM_NAME "x86" )
endif( CMAKE_SIZEOF_VOID_P EQUAL 8 )
