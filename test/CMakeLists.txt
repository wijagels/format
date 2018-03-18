include(GoogleTest)
find_package(GTest REQUIRED)

add_executable(ftest format_test.cpp)
target_link_libraries(ftest PUBLIC Wijagels::format gtest gtest_main)

gtest_discover_tests(ftest)
