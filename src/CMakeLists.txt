cmake_minimum_required (VERSION 2.8.12)
project(dungeon Fortran)

add_compile_options(-Wline-truncation)# -Wfatal-errors)

#add_library(state state.f90)
#add_library(action )
#target_link_libraries(action state)

add_executable(dungeon dungeon.f90 game.f objects.f parser.f rooms.f timefnc.f verbs.f subr.f state.f90 io.f90)
#target_link_libraries(dungeon action state)

#gdt.f

