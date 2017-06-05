cmake_minimum_required (VERSION 2.8.12)
project(dungeon Fortran)

add_compile_options(-Wline-truncation)# -Wfatal-errors)

add_library(state verbs.f subr.f state.f90 io.f90)

add_executable(dungeon dungeon.f90 game.f objects.f parser.f rooms.f timefnc.f)
target_link_libraries(dungeon state)

#gdt.f

