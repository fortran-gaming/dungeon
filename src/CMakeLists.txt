cmake_minimum_required (VERSION 2.8.12)
project(dungeon Fortran)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ..)

add_compile_options(-Wline-truncation -g -fbacktrace )# -Wall -Wfatal-errors)

add_library(dparam dparam.f io.f90)

add_executable(dungeon dungeon.f game.f objects.f parser.f rooms.f subr.f timefnc.f verbs.f)
target_link_libraries(dungeon dparam)

#add_executable(decrypt decrypt.f90 game.f objects.f parser.f rooms.f subr.f timefnc.f verbs.f)
#target_link_libraries(decrypt dparam)
