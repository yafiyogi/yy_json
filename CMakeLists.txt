#
#
#  MIT License
#
#  Copyright (c) 2024 Yafiyogi
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.
#
#

# PKG_CONFIG_PATH=${HOME}/code/tp/lib/pkgconfig:${HOME}/code/tp/share/pkgconfig:${PKG_CONFIG_PATH} cmake --fresh -DCMAKE_INSTALL_PREFIX=${HOME}/code/cpp/build -DYY_THIRD_PARTY_LIBRARY=${HOME}/code/tp -DCMAKE_POSITION_INDEPENDENT_CODE=TRUE -DCMAKE_BUILD_TYPE=Release .
# cmake --fresh -DCMAKE_INSTALL_PREFIX=${HOME}/code/cpp/build -DYY_THIRD_PARTY_LIBRARY=${HOME}/code/tp -DCMAKE_POSITION_INDEPENDENT_CODE=TRUE -DCMAKE_BUILD_TYPE=Release .

cmake_minimum_required(VERSION 3.24)

project(yy_json LANGUAGES CXX)

set(YY_ROOT "${PROJECT_SOURCE_DIR}/..")
set(YY_CMAKE "${YY_ROOT}/yy_cmake")

include(${YY_CMAKE}/cmake_boost-1.84.0.txt)
include(${YY_CMAKE}/cmake_clang_tidy.txt)
include(${YY_CMAKE}/cmake_common.txt)

find_package(Boost 1.84.0 REQUIRED COMPONENTS json)

add_library(yy_json STATIC)

target_include_directories(yy_json
  PRIVATE
    "${CMAKE_INSTALL_PREFIX}/include"
    "${YY_THIRD_PARTY_LIBRARY}/include" )

target_sources(yy_json
  PRIVATE
  yy_json_pointer_util.cpp)

set(YY_JSON_HEADERS
  "yy_json_constants.h"
  "yy_json_pointer.h"
  "yy_json_pointer_util.h" )

add_subdirectory(unit_tests)

install(TARGETS yy_json )
install(FILES ${YY_JSON_HEADERS} DESTINATION include/yy_json )

get_target_property(YY_JSON_SOURCES yy_json SOURCES)

add_custom_target(tidy-all
 COMMAND rm -f clang-tidy.log \; for f in "$$(" echo \"${YY_JSON_HEADERS} ${YY_JSON_SOURCES}\" | sed -n -e 's/;/ /pg' ")" \; do ${YY_CLANG_TIDY_CMD} ${YY_CLANG_TIDY_OPTIONS} "$$f" -- ${YY_TIDY_COMPILER_OPTIONS} >> clang-tidy.log \; done)
