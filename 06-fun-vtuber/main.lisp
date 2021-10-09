(load "../load-libs")
(ql:quickload 'cxx-jit)
(use-package :3d-vectors)
(use-package :3d-matrices)

;; --------------------------------------------------

(cffi:use-foreign-library "libopencv_core.so.4.5")
;; Debian: libopencv-dev

;; pkg-config --cflags --libs opencv4
(setf cxx-jit:*cxx-compiler-link-libs* "-I/usr/include/opencv4 -lopencv_stitching -lopencv_alphamat -lopencv_aruco -lopencv_bgsegm -lopencv_bioinspired -lopencv_ccalib -lopencv_dnn_objdetect -lopencv_dnn_superres -lopencv_dpm -lopencv_face -lopencv_freetype -lopencv_fuzzy -lopencv_hdf -lopencv_hfs -lopencv_img_hash -lopencv_intensity_transform -lopencv_line_descriptor -lopencv_mcc -lopencv_quality -lopencv_rapid -lopencv_reg -lopencv_rgbd -lopencv_saliency -lopencv_shape -lopencv_stereo -lopencv_structured_light -lopencv_phase_unwrapping -lopencv_superres -lopencv_optflow -lopencv_surface_matching -lopencv_tracking -lopencv_highgui -lopencv_datasets -lopencv_text -lopencv_plot -lopencv_ml -lopencv_videostab -lopencv_videoio -lopencv_viz -lopencv_ximgproc -lopencv_video -lopencv_dnn -lopencv_xobjdetect -lopencv_objdetect -lopencv_calib3d -lopencv_imgcodecs -lopencv_features2d -lopencv_flann -lopencv_xphoto -lopencv_photo -lopencv_imgproc -lopencv_core")

(cxx-jit:from
 '("<opencv2/videoio.hpp>"
   "<opencv2/imgproc.hpp>")
 'import
 '("
[](){
    cv::VideoCapture* camera = new cv::VideoCapture(0, cv::CAP_V4L2);
    return camera;
}" . "cv-video-capture")
 '("
[](cv::VideoCapture* camera){
    cv::Mat* frame = new cv::Mat();
    camera->read(*frame);
    return frame;
}" . "cv-video-capture-read")
 '("
[](cv::VideoCapture* camera){
    return camera->isOpened() ? (int) 1: (int) 0;
}
" . "cv-video-capture-is-opened")
 '("
[](cv::VideoCapture* camera){
    camera->release();
    delete camera;
}
" . "cv-video-capture-release")
 '("
[](cv::Mat* mat){
    mat->release();
    delete mat;
}" . "cv-mat-release")
 '("
[](cv::Mat* mat){
    return mat->data;
}
" . "cv-mat-data")
 '("
[](cv::Mat* mat, int* const rows, int* const cols){
    *rows = mat->rows;
    *cols = mat->cols;
}
" . "c-cv-mat-shape")
 '("
[](cv::Mat* mat){
    return mat->channels();
}
" . "cv-mat-channels")
 '("
[](cv::Mat* mat){
    return mat->elemSize();
}
" . "cv-mat-elem-size")
 '("
[](cv::Mat* mat){
    return (size_t)(mat->dataend - mat->datastart);
}" . "cv-mat-size-t")
 '("
[](cv::Mat* mat){
    // Surprise!! OpenCV use BGR internally!!
    cv::cvtColor(*mat, *mat, cv::COLOR_BGR2RGBA);
}" . "cv-cvt-color-bgr-rgba")
 '("
[](cv::Mat* mat, int flipCode){
    cv::flip(*mat, *mat, flipCode);
}" . "cv-flip")
 )

(defun cv-mat-shape (mat)
  (cffi:with-foreign-objects ((rows :int) (cols :int))
    (c-cv-mat-shape mat rows cols)
    (values (cffi:mem-ref cols :int) (cffi:mem-ref rows :int))))

(defmacro with-camera-read ((frame-name camera) &body body)
  `(let ((,frame-name (cv-video-capture-read ,camera)))
     (unwind-protect (progn ,@body)
       (cv-mat-release ,frame-name))))

;; (defparameter *camera* (cv-video-capture))
;; (defparameter *frame0* (cv-video-capture-read *camera*))
;; (cv-video-capture-release *camera*)
;; (cv-mat-shape *frame0*)
;; (cv-mat-channels *frame0*)
;; (cv-mat-elem-size *frame0*)
;; (cv-mat-data *frame0*)
;; (cv-mat-data-start *frame0*)
;; (cv-mat-data-end *frame0*)
;; (/ (cv-mat-size-t *frame0*) (* 640 480));; -> 3
;; (cv-cvt-color-rgb-rgba *frame0*)
;; (cv-mat-channels *frame0*)
;; (/ (cv-mat-size-t *frame0*) (* 640 480));; -> 4

;; --------------------------------------------------

(progn
  (sdl2-ffi.functions:sdl-init sdl2-ffi:+sdl-init-video+)
  (sdl2-ffi.functions:sdl-gl-set-attribute sdl2-ffi:+sdl-gl-context-major-version+ 3)
  (sdl2-ffi.functions:sdl-gl-set-attribute sdl2-ffi:+sdl-gl-context-minor-version+ 3)
  (sdl2-ffi.functions:sdl-gl-set-attribute sdl2-ffi:+sdl-gl-context-profile-mask+
                                           sdl2-ffi:+sdl-gl-context-profile-core+))
(progn
  (defparameter *window*
    (cffi:with-foreign-string (title "test")
      (sdl2-ffi.functions:sdl-create-window
       title
       sdl2-ffi:+sdl-windowpos-undefined+ sdl2-ffi:+sdl-windowpos-undefined+
       640 480
       (logior sdl2-ffi:+sdl-window-opengl+ sdl2-ffi:+sdl-window-shown+))))

  (defparameter *glcontext*
    (sdl2-ffi.functions:sdl-gl-create-context *window*))
  (%gl:viewport 0 0 640 480)
  (gl:clear-color 0.2 0.2 0.2 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (sdl2-ffi.functions:sdl-gl-swap-window *window*))

(progn
  (defparameter *vertex-shader*
    "
#version 330 core
layout(location = 0) in vec3 model_coord;
layout(location = 1) in vec3 model_normal;
layout(location = 2) in vec2 tex_coord;
uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
out vec2 frag_tex_coord;

void main(){
    frag_tex_coord = tex_coord;
    gl_Position = projection * view * model * vec4(model_coord, 1.0);
}
")

  (defparameter *fragment-shader*
    "
#version 330 core

uniform sampler2D box_texture;
in vec2 frag_tex_coord;
out vec3 color;

void main(){
    color = vec3(texture(box_texture, frag_tex_coord));
}
")

  (let* ((vs (compile-shader-from-string :vertex-shader *vertex-shader*))
         (fs (compile-shader-from-string :fragment-shader *fragment-shader*)))
    (defparameter *shader-program* (create-program-with-shaders vs fs))
    (gl:delete-shader vs)
    (gl:delete-shader fs)))



(defparameter *camera* (cv-video-capture))
(with-camera-read (frame *camera*)
  (cv-cvt-color-bgr-rgba frame)
  (multiple-value-bind (width height) (cv-mat-shape frame)
    (defparameter *camera-view-texture*
      (make-2d-texture-from-buffer (cv-mat-data frame) width height))))
(defparameter *camera-view-vao* (make-unit-square-vao))


(progn
  (gl:enable :depth-test)
  (gl:bind-vertex-array *camera-view-vao*)
  (gl:use-program *shader-program*)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *camera-view-texture*)

  (let ((proj-loc (gl:get-uniform-location *shader-program* "projection"))
      (view-loc (gl:get-uniform-location *shader-program* "view"))
      (model-loc (gl:get-uniform-location *shader-program* "model"))
      (box-texture-location (gl:get-uniform-location *shader-program* "box_texture")))
  (defun set-shader-uniforms (proj-mat view-mat model-mat)
    (%gl:uniform-1i box-texture-location 0)
    (3d-cffi:with-c-mat (c-proj proj-mat)
      (%gl:uniform-matrix-4fv proj-loc 1 t c-proj))
    (3d-cffi:with-c-mat (c-view view-mat)
      (%gl:uniform-matrix-4fv view-loc 1 t c-view))
    (3d-cffi:with-c-mat (c-model model-mat)
      (%gl:uniform-matrix-4fv model-loc 1 t c-model)))))

(loop
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (set-shader-uniforms (meye 4)
                       (meye 4)
                       (meye 4))
  (with-camera-read (frame *camera*)
    (cv-cvt-color-bgr-rgba frame)
    (cv-flip frame 0)
    (multiple-value-bind (width height) (cv-mat-shape frame)
      (bind-update-2d-texture-from-buffer
       *camera-view-texture*
       (cv-mat-data frame)
       width height)))
  (%gl:draw-elements :triangles 6 :unsigned-int 0)
  (sdl2-ffi.functions:sdl-gl-swap-window *window*))
