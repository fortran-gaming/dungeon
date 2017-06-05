cmake_minimum_required (VERSION 2.8.12)
project(dungeon Fortran) 

add_compile_options(-Wline-truncation)# -Wfatal-errors)

add_executable(dungeon state.f90 parser.f rooms.f  timefnc.f game.f objects.f subr.f verbs.f dungeon.f)
 
#gdt.f

