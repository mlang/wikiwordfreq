cmake_minimum_required(VERSION 2.8)
if("${CMAKE_CXX_COMPILER_ID}" MATCHES "(GNU|Clang)")
    set(CMAKE_CXX_FLAGS "-std=c++14")
endif()
find_package(Threads)
add_executable(dewiki-words dewiki-words.cxx)
target_link_libraries(dewiki-words ${CMAKE_THREAD_LIBS_INIT} studxml)

