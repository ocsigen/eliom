
##----------------------------------------------------------------------
##                SETTING FOR PROJECT %%%PROJECT_NAME%%%
##----------------------------------------------------------------------

PROJECT_NAME := %%%PROJECT_NAME%%%

## Do not forget `make depend' after adding a file!
%%%ifdef OCAML4%%%
## Source files for the server
SERVER_FILES := $(wildcard *.eliomi *.eliom)
## Source files for the client
CLIENT_FILES := $(wildcard *.eliomi *.eliom)
%%%endif%%%
%%%ifdef OCAML3%%%
## In OCaml<4 you must be sure those files are given in the correct linking order :(
## Source files for the server
SERVER_FILES := %%%PROJECT_NAME%%%.eliom
## Source files for the client
CLIENT_FILES := %%%PROJECT_NAME%%%.eliom
%%%endif%%%

## OCamlfind packages for the server
SERVER_PACKAGES =
## OCamlfind packages for the client
CLIENT_PACKAGES =

## Generate debugging information (-g)
GENERATE_DEBUG := no

## Local folder for make run.local
LOCAL_PREFIX := local
## Root of installation
INSTALL_PREFIX := /

include Makefile.common
