########################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.6 FATAL_ERROR)
########################################################
# project name
PROJECT( Nov14Analysis )

# project version
SET( ${PROJECT_NAME}_VERSION_MAJOR 1 )
SET( ${PROJECT_NAME}_VERSION_MINOR 0 )
SET( ${PROJECT_NAME}_VERSION_PATCH 0 )

# include debugging symbols
set(CMAKE_BUILD_TYPE RelWithDebInfo)

# set the path to which we will install later: default project home, can be changed using
# cmake -DINSTALL_PREFIX=/some/dir ..
SET(INSTALL_PREFIX "${PROJECT_SOURCE_DIR}" CACHE PATH "Prefix prepended to install directories")
SET(CMAKE_INSTALL_PREFIX "${INSTALL_PREFIX}" CACHE INTERNAL "Prefix prepended to install directories" FORCE)

# additional packages to be searched for by cmake
LIST( APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake )

# Set the rpath so that our executables find the library even when installed into non-default locations
include(RPath)

# Determine platform- and compiler-specific settings
include(Platform)

# search for the packages needed for the main library
FIND_PACKAGE(ROOT REQUIRED)
INCLUDE_DIRECTORIES(${ROOT_INCLUDE_DIR})

INCLUDE_DIRECTORIES( include )
SET ( LIBRARY_SOURCES
  src/plotResults.cpp
  )

ADD_LIBRARY( plotResults SHARED ${LIBRARY_SOURCES} )
target_link_libraries(plotResults ${ROOT_LIBRARIES})



add_executable(runplotResults       src/runplotResults.cxx      )

target_link_libraries(runplotResults plotResults ${ROOT_LIBRARIES})

INSTALL(TARGETS plotResults runplotResults
                              RUNTIME DESTINATION bin
                              LIBRARY DESTINATION lib
                              ARCHIVE DESTINATION lib)

#######################################################
# Print out system information useful for bug reports #
############################################### #######
include(PrintSystemInformation)
