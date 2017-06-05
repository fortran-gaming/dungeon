cmake_minimum_required (VERSION 2.8.12)
project(dungeon Fortran) 

add_compile_options(-Wline-truncation -Wfatal-errors)

add_library(state state.f90)

add_executable(dungeon subr.f parser.f rooms.f  timefnc.f game.f objects.f  verbs.f dungeon.f)
target_link_libraries(dungeon state)
 
#gdt.f

