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
