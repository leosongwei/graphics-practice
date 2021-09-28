#!/bin/sh
#/opt/c2ffi/c2ffi opengl.h -x c -o opengl.x86_64-pc-linux-gnu.spec -A x86_64-pc-linux-gnu
#/opt/c2ffi/c2ffi opengl_macros.h -x c -o opengl_macros.x86_64-pc-linux-gnu.spec -A x86_64-pc-linux-gnu --error-limit=10000
mkdir output
/opt/c2ffi/c2ffi opengl.h -x c -M output/opengl_const.h -A x86_64-pc-linux-gnu -D null
awk '$5~/^([A-Z]|_)+;$/' output/opengl_const.h | sort > output/opengl_macros.h
/opt/c2ffi/c2ffi opengl_build.h -x c -I . -o output/opengl.x86_64-pc-linux-gnu.spec -A x86_64-pc-linux-gnu --error-limit=10000
cp output/opengl.x86_64-pc-linux-gnu.spec ..

