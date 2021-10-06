(ql:quickload 'sdl2)
(ql:quickload 'cl-opengl)
(ql:quickload '3d-matrices)

(progn
  (cffi:use-foreign-library "libGLEW.so")
  (cffi:use-foreign-library "libGLU.so")
  (cffi:use-foreign-library "libGL.so")
  (cffi:use-foreign-library "libGLX.so")
  (cffi:use-foreign-library "libSDL_image-1.2.so.0")
  (cffi:use-foreign-library "libstb.so.0"))

(load "../utils/shader")
(load "../utils/3d-cffi")
(load "../utils/stb")
(load "../utils/utils")
