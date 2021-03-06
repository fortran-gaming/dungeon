.. image:: https://travis-ci.org/scivision/dungeon.svg?branch=master
    :target: https://travis-ci.org/scivision/dungeon

=============
DUNGEON V.4.0
=============


DUNGEON is the original game that served as the basis for the classic Zork text adventure series.

Originally implemented in MDL (pronounced "muddle") on a DEC PDP-10 at MIT in the late 1970s by Tim Anderson, Marc Blank, Bruce Daniels, and Dave Lebling, DUNGEON was ported to VAX FORTRAN in 1980 by Robert Supnik. 
Most of Supnik's source code remains intact here, but it made extensive use of a nonstandard VAX FORTRAN extension which is no longer supported by modern fortran compilers. 
The purpose of this update is to make DUNGEON compilable using gfortran, enabling a new generation of adventurers to "GET LAMP."

.. contents::

COMPILATION
===========
You can use CMake or the traditional Makefile::

    (cd bin; cmake ../src && make)
    ./dungeon

or using just the Makefile::

    (cd src; make && mv dungeon ..)
    ./dungeon

``dungeon`` executable *must* be run from the directory containing the ``dindx`` and ``dtext`` files. 

Prereqs
-------
::

    apt install gfortran make cmake


CHANGES FROM EARLIER VERSIONS
=============================

While updating the code for gfortran required extensive changes to DUNGEON'S source code, there should be no functional differences from Supnik's ultimate version from late 1994, V3.2B. 

DISCLAIMER
==========

This software is for *non-commercial use only*.
