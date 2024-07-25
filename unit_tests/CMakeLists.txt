cmake_minimum_required(VERSION 3.24)

project(test_yy_json LANGUAGES CXX)

find_package(Boost 1.84.0 REQUIRED COMPONENTS locale)
find_package(fmt REQUIRED)
find_package(GTest REQUIRED)
find_package(spdlog REQUIRED)
find_package(yy_cpp REQUIRED)
find_package(yy_test REQUIRED)

add_executable(test_yy_json
  json_pointer_tests.cpp )

target_include_directories(test_yy_json
  PRIVATE
    "${PROJECT_SOURCE_DIR}/.."
    "${CMAKE_INSTALL_PREFIX}/include"
    "${YY_THIRD_PARTY_LIBRARY}/include" )

target_link_directories(test_yy_json
  PUBLIC
  /opt/local/lib )

target_compile_options(yy_json
  PRIVATE
  "-DSPDLOG_COMPILED_LIB"
  "-DSPDLOG_FMT_EXTERNAL")

target_link_libraries(test_yy_json
  yy_json
  yy_test::yy_test
  yy_cpp::yy_cpp
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
cmake_path(GET PROJECT_SOURCE_DIR PARENT_PATH YY_PROJECT_SOURCE_PARENT_DIR)
add_custom_target(coverage
  COMMAND ${FASTCOV_CMD} --include "${YY_PROJECT_SOURCE_PARENT_DIR}" --exclude "${PROJECT_SOURCE_DIR}" --search-directory .. --process-gcno --output json-gcov.json
  COMMAND ${FASTCOV_CMD} -C json-gcov.json --lcov --output json-gcov.info
  COMMAND genhtml -o json-gcov.html json-gcov.info )

add_dependencies(coverage test)

add_custom_target(asan
  COMMAND ./test_yy_json --gtest_brief=1 heap_leak 2> test_yy_json.asan_heap_leak.log
  COMMAND ./test_yy_json --gtest_brief=1 global_buffer_overflow 2> test_yy_json.asan_global_buffer_overflow.log)

add_dependencies(asan test_yy_json)

add_custom_target(lsan
  COMMAND ./test_yy_json --gtest_brief=1 heap_leak 2> test_yy_json.lsan_heap_leak.log
  COMMAND ./test_yy_json --gtest_brief=1 heap_buffer_overflow 2> test_yy_json.lsan_heap_buffer_overflow.log )

add_dependencies(lsan test_yy_json)
