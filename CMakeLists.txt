# PKG_CONFIG=cmake --fresh -DCMAKE_INSTALL_PREFIX=${HOME}/code/cpp/build -DYY_THIRD_PARTY_LIBRARY=${HOME}/code/tp -DCMAKE_POSITION_INDEPENDENT_CODE=TRUE -DCMAKE_BUILD_TYPE=Release .

cmake_minimum_required(VERSION 3.24)

project(yy_json LANGUAGES CXX)

set(YY_ROOT "${PROJECT_SOURCE_DIR}/..")
set(YY_CMAKE "${YY_ROOT}/yy_cmake")

include(${YY_CMAKE}/cmake_common.txt)
include(${YY_CMAKE}/cmake_boost-1.84.0.txt)

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
