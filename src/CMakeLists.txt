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
