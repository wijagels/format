cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

project(format LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_compile_options(-Wall -Wextra -pedantic -Wcast-align -Wcast-qual -Wctor-dtor-privacy -Wdisabled-optimization -Wformat=2 -Winit-self -Wmissing-include-dirs -Wold-style-cast -Woverloaded-virtual -Wredundant-decls -Wshadow -Wsign-promo -Wstrict-overflow=5 -Wundef -Wswitch-default -Wno-gnu-string-literal-operator-template)
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG -fno-omit-frame-pointer")

add_library(format INTERFACE)
add_library(Wijagels::format ALIAS format)
target_include_directories(format INTERFACE format)

add_library(format_dummy format/format.cpp)
target_link_libraries(format_dummy PRIVATE Wijagels::format)
set_target_properties(
    format_dummy PROPERTIES
    CXX_CLANG_TIDY clang-tidy
)

# set_target_properties(
#     format_dummy PROPERTIES
#     CXX_INCLUDE_WHAT_YOU_USE include-what-you-use
# )

enable_testing()
add_subdirectory(test)
