# File based on test-drive and on stdlib
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


# Preprocesses a list of files with given preprocessor and preprocessor options
#
# Args:
#     preproc [in]: Preprocessor program
#     preprocopts [in]: Preprocessor options
#     srcext [in]: File extension of the source files
#     trgext [in]: File extension of the target files
#     srcfiles [in]: List of the source files
#     trgfiles [out]: Contains the list of the preprocessed files on exit
#
function(preprocess preproc preprocopts srcext trgext srcfiles trgfiles)

  set(_trgfiles)
  foreach(srcfile IN LISTS srcfiles)
    string(REGEX REPLACE "\\.${srcext}$" ".${trgext}" trgfile ${srcfile})
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${trgfile}
      COMMAND ${preproc} ${preprocopts} ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile} ${CMAKE_CURRENT_BINARY_DIR}/${trgfile}
      MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile})
    list(APPEND _trgfiles ${CMAKE_CURRENT_BINARY_DIR}/${trgfile})
  endforeach()
  set(${trgfiles} ${_trgfiles} PARENT_SCOPE)

endfunction()

# Preprocesses fortran files with fypp.
#
# It assumes that source files have the ".fypp" extension. Target files will be
# created with the extension ".f90". The FYPP variable must contain the path to
# the fypp-preprocessor.
#
# Args:
#     fyppopts [in]: Options to pass to fypp.
#     fyppfiles [in]: Files to be processed by fypp
#     f90files [out]: List of created f90 files on exit
#
function (fypp_f90 fyppopts fyppfiles f90files)
  preprocess("${FYPP}" "${fyppopts}" "fypp" "f90" "${fyppfiles}" _f90files)
  set(${f90files} ${_f90files} PARENT_SCOPE)
endfunction()

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

set(
  fppFiles
  "modhash.fypp"
  "modtable.fypp"
)

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

list(
    APPEND srcs
    "${outFiles}"
)

set(srcs "${srcs}" PARENT_SCOPE)

add_library(${PROJECT_NAME}-lib ${srcs})
