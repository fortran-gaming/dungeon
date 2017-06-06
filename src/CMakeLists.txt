cmake_minimum_required (VERSION 2.8.12)
project(dungeon Fortran)

add_compile_options(-O0 -Wline-truncation -g )# -Wfatal-errors)

add_library(dparam dparam.f )

add_executable(dungeon dungeon.f game.f objects.f parser.f rooms.f subr.f timefnc.f verbs.f)
target_link_libraries(dungeon dparam)
