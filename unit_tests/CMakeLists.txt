cmake_minimum_required(VERSION 3.24)

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
