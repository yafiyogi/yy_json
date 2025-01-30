#
#
#  MIT License
#
#  Copyright (c) 2024-2025 Yafiyogi
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

cmake_minimum_required(VERSION 3.25)

project(test_yy_json LANGUAGES CXX)

include(${YY_CMAKE}/cmake_boost-1.84.0.txt)

find_package(Boost CONFIG 1.84.0 REQUIRED COMPONENTS json locale)
find_package(fmt REQUIRED)
find_package(GTest REQUIRED)
find_package(spdlog REQUIRED)
find_package(yy_cpp REQUIRED)
find_package(yy_test REQUIRED)

add_executable(test_yy_json
  json_pointer_tests.cpp
  json_pointer_util_tests.cpp )

target_compile_options(yy_json
  PRIVATE
  "-DSPDLOG_COMPILED_LIB"
  "-DSPDLOG_FMT_EXTERNAL")

target_include_directories(test_yy_json
  SYSTEM PRIVATE
    "${PROJECT_SOURCE_DIR}/.."
    "${CMAKE_INSTALL_PREFIX}/include"
    "${YY_THIRD_PARTY_LIBRARY}/include" )

target_link_directories(test_yy_json
  PUBLIC
    "${CMAKE_INSTALL_PREFIX}/lib"
    /opt/local/lib )

target_link_libraries(test_yy_json
  yy_json
  yy_test
  yy_cpp
  Boost::locale
  Boost::json
  fmt::fmt
  spdlog::spdlog
  GTest::gtest
  GTest::gtest_main)

# Test target
add_custom_target(test
  COMMAND
    ./test_yy_json)

add_dependencies(test test_yy_json)

# Coverage target
add_cmake_coverage(yy_json test)
add_cmake_asan(yy_json test_yy_json)
add_cmake_lsan(yy_json test_yy_json)
