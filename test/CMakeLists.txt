find_package(GTest REQUIRED)
include(GoogleTest)

add_executable(ftest format_test.cpp)
target_link_libraries(ftest PUBLIC Wijagels::format GTest::GTest GTest::Main)

gtest_discover_tests(ftest)
