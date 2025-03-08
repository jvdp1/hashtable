# File based on test-drive
# This file is part of test-drive.
# SPDX-Identifier: Apache-2.0 OR MIT
#
# Licensed under either of Apache License, Version 2.0 or MIT license
# at your option; you may not use this file except in compliance with
# the License.
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

cmake_minimum_required(VERSION 3.20)

# Include overwrites before setting up the project
set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/config/DefaultFlags.cmake)

get_directory_property(is-subproject PARENT_DIRECTORY)

project(
  "hashtable"
  LANGUAGES "Fortran"
  VERSION "0.1.0"
  DESCRIPTION "Hash table"
)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# General configuration information
add_subdirectory("config")

# --- find preprocessor
find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp not found! Please install fypp following the instructions in https://fypp.readthedocs.io/en/stable/fypp.html#installing")
endif()

# Custom preprocessor flags
set(fyppFlags "")
list(
  APPEND fyppFlags
  "-I${PROJECT_SOURCE_DIR}/src"
)

# Collect source of the project
set(srcs)
add_subdirectory("src")

# We need the module directory before we finish the configure stage
if(NOT EXISTS "${PROJECT_BINARY_DIR}/include")
  make_directory("${PROJECT_BINARY_DIR}/include")
endif()

set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include"
)
target_include_directories(
  "${PROJECT_NAME}-lib"
  PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${module-dir}>
)
target_compile_definitions(
  "${PROJECT_NAME}-lib"
  PRIVATE
)

# Export targets for other projects
add_library("${PROJECT_NAME}" INTERFACE)
target_link_libraries("${PROJECT_NAME}" INTERFACE "${PROJECT_NAME}-lib")
install(
  TARGETS
  "${PROJECT_NAME}"
  "${PROJECT_NAME}-lib"
  EXPORT
  "${PROJECT_NAME}-targets"
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)
install(
  EXPORT
  "${PROJECT_NAME}-targets"
  NAMESPACE
  "${PROJECT_NAME}::"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
install(
  DIRECTORY
  "${PROJECT_BINARY_DIR}/include/"
  DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${module-dir}"
)
# Package license files
install(
  FILES
  "LICENSE"
  DESTINATION "${CMAKE_INSTALL_DATADIR}/licenses/${PROJECT_NAME}"
)

# add the testsuite
include(CTest)
if(BUILD_TESTING AND HASHTABLE_BUILD_TESTING)
  add_subdirectory("test")
endif()
