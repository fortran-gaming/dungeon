cmake_minimum_required (VERSION 2.8.12)
project(dungeon Fortran)

add_compile_options(-Wline-truncation)# -Wfatal-errors)

add_library(state subr.f state.f90 io.f90   objects.f  )

add_executable(dungeon dungeon.f90 game.f  parser.f verbs.f)
target_link_libraries(dungeon state)

#gdt.f
