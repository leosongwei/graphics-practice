;; Debian: libdlib-dev libopenblas-dev libsqlite3-dev

(cffi:use-foreign-library "libdlib.so.19")

(setf cxx-jit:*cxx-compiler-link-libs*
      "-ldlib -lpthread -lX11 -lXext -lpng -lz -ljpeg -lopenblas -lsqlite3")

(cxx-jit:from
 '("<dlib/image_processing/frontal_face_detector.h>")
 'import
 '("
[](){
    return dlib::get_frontal_face_detector();
}
" . "dlib-get-frontal-face-detector"))
