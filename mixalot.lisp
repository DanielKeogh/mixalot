;;;; Mixalot audio mixer for OpenAL

;;;; Copyright (c) 2009,2010 Andy Hefner

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sellcopies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject
;;;;  to the following conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(defpackage :mixalot
  (:use :common-lisp :cffi :bordeaux-threads :mixalot-ffi-common)
  (:export #:sample-vector
           #:stereo-sample
           #:mono-sample

           #:streamer-mix-into
           #:streamer-write-into
           #:streamer-cleanup
           #:streamer-pause
           #:streamer-unpause
           #:streamer-paused-p
           #:streamer-seekable-p
           #:streamer-length
           #:streamer-seek
           #:streamer-position
           #:streamer-note-completion

           #:mixer
           #:mixer-stream-lock
           #:mixer-stream-list
           #:mixer-current-time
           #:mixer-rate
           #:mixer-shutdown-flag
           #:mixer-add-streamer
           #:mixer-remove-streamer
           #:mixer-remove-all-streamers
           #:mixer-note-write
           #:create-mixer
           #:destroy-mixer
           #:array-index
           #:with-array-pointer
           #:clamp-sample #:clamp-sample+
           #:mono->stereo #:stereo-left #:stereo-right
           #:stereo->mono
           #:%stereo-left #:%stereo-right
           #:split-sample
           #:mix-stereo-samples #:add-stereo-samples
           #:stereo-incf #:stereo-mixf
           #:make-test-streamer
           #:dummy-mixer
           #:playback-finished
           #:pf-length
           #:signal-playback-finish

           #:vector-streamer
           #:vector-streamer-mono
           #:make-vector-streamer-mono
           #:vector-streamer-interleaved-stereo
           #:make-vector-streamer-interleaved-stereo
           #:vector-streamer-joint-stereo
           #:make-vector-streamer-joint-stereo
           #:fast-vector-streamer-mono
           #:make-fast-vector-streamer-mono
           #:fast-vector-streamer-interleaved-stereo
           #:make-fast-vector-streamer-interleaved-stereo
           #:fast-vector-streamer-joint-stereo
           #:make-fast-vector-streamer-joint-stereo

           #:vector-streamer-mono-single-float
           #:make-vector-streamer-mono-single-float
           #:vector-streamer-mono-double-float
           #:make-vector-streamer-mono-double-float))

(in-package :mixalot)

(deftype array-index ()
  #-sbcl '(integer 0 #.array-dimension-limit)
  #+sbcl 'sb-int:index)

(deftype stereo-sample () '(unsigned-byte 32))

(deftype mono-sample ()
  '(or
    (signed-byte 16)
    (unsigned-byte 16)))

(deftype sample-vector () '(simple-array stereo-sample 1))

;;;; Basic stream protocol

(defgeneric streamer-mix-into (stream mixer buffer offset length time)
  (:documentation
   "Mix 'length' samples of stream output into buffer starting at 'offset'
 measured in samples, at 'time' (measured in samples since the mixer was
 created. The time measurement includes the offset, and is intended for
 synchronizing streams. Called from outside the mixer lock.")
  (:method ((stream function) mixer buffer offset length time)
    (funcall stream stream mixer buffer offset length time)))

(defgeneric streamer-write-into (stream mixer buffer offset length time)
  (:documentation
   "Write 'length' samples of stream output into buffer starting at
   'offset' (measured in samples), at 'time' (measured in samples
   since the mixer was created. The time measurement includes the
   offset, and is intended for synchronizing streams. The differs from
   stream-write-info in that you don't have to mix the data, the
   current contents are expected to be garbage and can be
   overwritten. Implementing this is optional. Called from outside the
   mixer lock.")
  (:method (stream mixer buffer offset length time)
    (declare (type sample-vector buffer)
             (type array-index offset length)
             (optimize (speed 3)))
    (fill buffer 0 :start offset :end (+ offset length))
    (streamer-mix-into stream mixer buffer offset length time)))

(defgeneric streamer-cleanup (stream mixer)
  (:documentation
  "Release resources and perform any other cleanups needed when a
  streamer is destroyed as a result of a call to mixer-remove-streamer.
  Called outside the mixer lock, so it's okay to manipulate the mixer.")
  (:method (stream mixer)
    (declare (ignore stream mixer))))

;;;; Pausing streams: The mixer handles pausing automatically.
;;;; Streamers need not define methods on these functions, unless they
;;;; have a reason to take action on pause/unpause.

(defgeneric streamer-pause (stream mixer)
  (:documentation "Pause playback of the streamer. A method on
  streamer-pause is optional and serves as a notification to the
  streamer that it has been paused; the default method is specialized
  on the mixer and can suspend playback without any special support
  from the streamer."))

(defgeneric streamer-unpause (stream mixer)
  (:documentation "Unpause playback of the streamer. A method on
  streamer-unpause is optional and serves as a notification to the
  streamer that it has been unpaused; the default method is
  specialized on the mixer and can resume playback without any special
  support from the streamer."))

(defgeneric streamer-paused-p (stream mixer)
  (:documentation "Query whether a stream is paused or not."))

;;;; Optional: Seekable stream protocol

(defgeneric streamer-seekable-p (stream mixer)
  (:documentation "Returns non-NIL if the streamer supports seeking.")
  (:method (stream mixer)
    (declare (ignore stream mixer))
    nil))

(defgeneric streamer-length (stream mixer)
  (:documentation "Returns length, in samples, of the audio stream, or
  NIL if it cannot be determined.")
  (:method (stream mixer)
    (declare (ignore stream mixer))
    nil))

(defgeneric streamer-seek (stream mixer position &key &allow-other-keys)
  (:documentation "Seek to position (measured in samples) from the start of stream."))

(defgeneric streamer-position (stream mixer)
  (:documentation "Returns current position within a seekable stream.")
  (:method (stream mixer)
    (declare (ignore stream mixer))
    nil))


;;;; Mixer process

;;; The mixer contains zero or more streamers and pumps samples from
;;; them to mix and send to the audio card. Streamers can be added and
;;; removed at (almost) any time.

;;; Another reasonable design would be that you connect a single
;;; stream to the audio device, and a mixer is just another type of
;;; stream. This would solve some problems for a certain kind of app,
;;; but I haven't pursued simply because I didn't think of it soon
;;; enough, and changing to that approach if/when I have use for it
;;; shouldn't break the API incompatibly.

(defstruct mixer
  (stream-lock (bordeaux-threads:make-lock "Mixer lock"))
  (stream-list  nil)
  (current-time 0)
  (rate         44100)
  (shutdown-flag nil)
  (stream-state (make-hash-table))
  device)

(defmacro with-mixer-lock ((mixer) &body body)
  `(with-lock-held ((mixer-stream-lock ,mixer))
    ,@body))

(defun mixer-add-streamer (mixer streamer)
  (with-mixer-lock (mixer)
    (cond
      ((mixer-shutdown-flag mixer)
       (error "You can't add a stream to a shutdown mixer!"))
      (t (push streamer (mixer-stream-list mixer))
         (values streamer (mixer-current-time mixer))))))

(defun %req-remove-streamer (mixer streamer)
  (setf (gethash streamer (mixer-stream-state mixer)) :remove))

(defgeneric mixer-remove-streamer (mixer streamer))

(defmethod mixer-remove-streamer ((mixer mixer) streamer)
  (with-mixer-lock (mixer)
    (%req-remove-streamer mixer streamer))
  (values))

(defun mixer-remove-all-streamers (mixer)
  (with-mixer-lock (mixer)
    (dolist (streamer (mixer-stream-list mixer))
      (%req-remove-streamer mixer streamer))))

;;; Obtaining a pointer to an array of unboxed data. I used to do this
;;; myself, but recentish CFFI can do it for me.
(defmacro with-array-pointer ((name array) &body body)
  `(cffi-sys:with-pointer-to-vector-data (,name ,array) ,@body))

#+NIL
(defmacro with-array-pointer ((name array) &body body)
  ;; Perhaps does the wrong thing for displaced arrays.
  ;; This will never affect me.
  ;; Also, SBCL gives a very bizarre code deletion warning here
  ;; when compiling the file in SLIME which goes away when I
  ;; compile just the definition.
  `((lambda (arrayoid body)
      (unless (typep arrayoid 'vector)
        (setf arrayoid (sb-kernel:%array-data-vector arrayoid)))
      (sb-sys:with-pinned-objects (arrayoid)
        (funcall body (sb-sys:vector-sap arrayoid))))
    ,array
    (lambda (,name) ,@body)))

(defmethod streamer-pause (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (when (find stream (mixer-stream-list mixer))
      (setf (gethash stream (mixer-stream-state mixer)) :paused))))

(defmethod streamer-unpause (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (when (eql (gethash stream (mixer-stream-state mixer)) :paused)
      (remhash stream (mixer-stream-state mixer)))))

(defmethod streamer-paused-p (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (eql (gethash stream (mixer-stream-state mixer)) :paused)))

(defun update-playable (mixer playable-streams)
  (with-mixer-lock (mixer)
    (setf (fill-pointer playable-streams) 0)
    (dolist (stream (mixer-stream-list mixer))
      (let ((state (gethash stream (mixer-stream-state mixer))))
        (unless (eql :paused state)
          (vector-push-extend stream playable-streams))))))

(defun remove-removable (mixer temp-vector)
  (with-mixer-lock (mixer)
    (let ((state-table (mixer-stream-state mixer)))
      (setf (fill-pointer temp-vector) 0
            (mixer-stream-list mixer)
            (delete-if
             (lambda (streamer)
               (when (eql :remove (gethash streamer state-table))
                 (vector-push-extend streamer temp-vector)
                 (remhash streamer state-table)
                 t))
             (mixer-stream-list mixer)))))
  ;; Run the cleanups outside the lock:
  (loop for removed across temp-vector
        do (streamer-cleanup removed mixer)))

(defconstant +mixer-buffer-size+ 1024)
(defconstant +ring-buffer-count+ 3)
(deftype mixer-buffer-index () `(integer 0 ,+mixer-buffer-size+))

(define-condition playback-finished ()
  ((length :initform nil :initarg :length :reader pf-length))
  (:documentation "Condition, which can be signalled by streamer to indicate, that it had finished its playback."))

(defmacro signal-playback-finish (&optional (length (intern "LENGTH")))
  "Convenience macro, which by default assumes, that LENGTH variable contains the length, that
should be output in STREAMER-MIX-INTO or STREAMER-WRITE-INTO."
  `(error 'playback-finished :length ,length))

(defgeneric mixer-note-write (mixer buffer offset size)
  (:method (mixer buffer offset size)
    (declare (ignore mixer buffer offset size))))

(defun playing-p (source)
  (cffi-c-ref:c-with ((playing %al:int))
    (%al:get-sourcei source %al:+source-state+ (playing &))
    (= playing %al:+playing+)))

(defun wait-for-buffer (source)
  (cffi-c-ref:c-with ((buffers-queued %al:int)
		      (buffers-processed %al:int))
    (%al:get-sourcei source %al:+buffers-queued+ (buffers-queued &))
    (%al:get-sourcei source %al:+buffers-processed+ (buffers-processed &))
    (loop while (and (>= buffers-queued +ring-buffer-count+) (< buffers-processed 1))
	  for i from 0 to 50
	  do
	     (sleep 0.01)
	     (%al:get-sourcei source %al:+buffers-processed+ (buffers-processed &)))))

(defun dequeue-buffer (source)
  (cffi-c-ref:c-with ((buffers-processed %al:int))
    (%al:get-sourcei source %al:+buffers-processed+ (buffers-processed &))
    (when (> buffers-processed 0)
      (static-vectors:with-static-vector (o buffers-processed)
	(%al:source-unqueue-buffers source buffers-processed (static-vectors:static-vector-pointer o))))))

(defmacro with-al-device (&body body)
  `(float-features:with-float-traps-masked ()
     ;; Open default sound device
     (let ((dev (%alc:open-device nil)))
       (when (cffi:null-pointer-p dev)
         (error "Couldn't open sound device"))
       ;; Create OpenAL context for opened device
       (unwind-protect
	    (let ((ctx (%alc:create-context dev nil)))
	      (when (cffi:null-pointer-p ctx)
		(error "Failed to create OpenAL context"))
	      ;; Assign OpenAL context to the application
	      (%alc:make-context-current ctx)
	      (unwind-protect
		   (progn ,@body)
		   (%alc:destroy-context ctx)))
	 (%alc:close-device dev)))))

(defun run-mixer-process (mixer)
  (declare (optimize (speed 3)))
  (with-al-device
    (cffi-c-ref:c-with ((source %al:uint))
      (cffi:with-foreign-array (buffer-ring (make-array +ring-buffer-count+)
					    `(:array %al:uint ,+ring-buffer-count+))
	(static-vectors:with-static-vectors ((buffer +mixer-buffer-size+ :element-type '(unsigned-byte 32)))
	  (%al:gen-buffers +ring-buffer-count+ buffer-ring)
	  (%al:gen-sources 1 (source &))
	  (unwind-protect
	       (loop with ring-index fixnum = 0
		     with time fixnum = 0
		     with buffer-samples = +mixer-buffer-size+

		     with playable-streams = (make-array 0 :adjustable t :fill-pointer 0)
		     with buffer-clear = nil
		     with rate = (mixer-rate mixer)
		     until (mixer-shutdown-flag mixer)
		     do
			;; So that we don't have to hold the lock during the stream
			;; callbacks, use this temporary vector:
			(remove-removable mixer playable-streams)
			(update-playable mixer playable-streams)
			;; Loop through playable streams and generate audio
			(loop for streamer across playable-streams
			      for first = t then nil
			      as offset = 0             ; ...
			      do
				 (setf buffer-clear nil)
				 (restart-case
				     (handler-case
					 (funcall (if first
						      #'streamer-write-into
						      #'streamer-mix-into)
						  streamer
						  mixer
						  buffer
						  offset
						  (- buffer-samples offset)
						  (+ time offset))
				       (playback-finished () (mixer-remove-streamer mixer streamer)))
				   (remove-streamer ()
				     :report "Delete this audio stream"
				     (mixer-remove-streamer mixer streamer))))
			;; If there are no playable streams, we have to clear the buffer ourself.
			(when (and (zerop (length playable-streams))
				   (not buffer-clear))
			  (fill buffer 0)
			  (setf buffer-clear t))
			;; Notification of data written.
			(mixer-note-write mixer buffer 0 buffer-samples)

			;; Play the buffer.
			(dequeue-buffer source)
			(%al:buffer-data (cffi:foreign-aref buffer-ring `(:array %al:uint ,+ring-buffer-count+) ring-index)

					 %al:+format-stereo16+ (static-vectors:static-vector-pointer buffer)
					 (* 4 +mixer-buffer-size+) rate)
			
			(%al:source-queue-buffers source 1 (cffi:inc-pointer buffer-ring (* ring-index 4)))
			(setf ring-index (mod (1+ ring-index) +ring-buffer-count+))
			(unless (playing-p source)
			  (%al:source-play source))
			(wait-for-buffer source)
			(incf time buffer-samples)
			(setf (mixer-current-time mixer) time))
	    ;; Cleanup. After setting the shutdown flag, it is impossible to
	    ;; add additional streamers, so there's no race during the shutdown.
	    (with-mixer-lock (mixer) (setf (mixer-shutdown-flag mixer) t))
	    (dolist (streamer (mixer-stream-list mixer))
	      (streamer-cleanup streamer mixer))
	    (clrhash (mixer-stream-state mixer))
	    (setf (mixer-stream-list mixer) nil)))))))

(defun create-mixer (&key (rate 44100) (constructor 'make-mixer))
  "Create a new mixer at the specified sample rate, running in its own thread."
  (let ((mixer (funcall constructor :rate rate)))
    (bordeaux-threads:make-thread
     (lambda ()
       (run-mixer-process mixer))
     :name (format nil "Mixer thread ~:D Hz" rate))
    mixer))

(defun destroy-mixer (mixer)
  (with-mixer-lock (mixer)
    (setf (mixer-shutdown-flag mixer) t))
  (values))

;;;; Fastish sample manipulation

(declaim (inline stereo-sample sign-extend-16
                 clamp-sample clamp-sample+
                 mono->stereo stereo-left stereo-right
                 stereo->mono %stereo-left %stereo-right
                 split-sample
                 mix-stereo-samples add-stereo-samples
                 scale-sample scale-stereo-sample))

(defun stereo-sample (left right)
  (declare (optimize (speed 3))
           (type mono-sample left right))
  (logior (ldb (byte 16 0) left)
          (ash (ldb (byte 16 0) right) 16)))

(defun mono->stereo (sample)
  (stereo-sample sample sample))

(defun sign-extend-16 (x)
  (declare (optimize (speed 3))
           (type (unsigned-byte 16) x))
  (let ((c (ash -1 15)))
    (logxor (+ x c) c)))

(defun %stereo-left (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  (ldb (byte 16 0)  sample))

(defun %stereo-right (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  (ldb (byte 16 16)  sample))

(defun stereo-left (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  (sign-extend-16 (%stereo-left  sample)))

(defun stereo-right (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  (sign-extend-16 (%stereo-right sample)))

(defun stereo->mono (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  ;; SBCL doesn't do the best job on this on.
  (ash (+ (stereo-left sample) (stereo-right sample)) -1))

(defun split-sample (sample)
  (values (stereo-left sample)
          (stereo-right sample)))

(defun clamp-sample (x) (min 32767 (max -32768 x)))
(defun clamp-sample+ (x y) (clamp-sample (+ x y)))

(defun mix-stereo-samples (x y)
  "Mix two stereo samples by clamped addition"
  (declare (optimize (speed 3)) (type stereo-sample x y))
  (stereo-sample (clamp-sample+ (stereo-left  x) (stereo-left  y))
                 (clamp-sample+ (stereo-right x) (stereo-right y))))

(defun add-stereo-samples (x y)
  "Add two stereo samples, without clipping."
  (declare (optimize (speed 3)) (type stereo-sample x y))
  (logior (logand #xFFFF (+ x y))
          (logand #xFFFF0000 (+ x (logand #xFFFF0000 y))))
  #+NIL ;; Equivalent, slower version:
  (stereo-sample (logand #xFFFF (+ (%stereo-left x) (%stereo-left y)))
                 (logand #xFFFF (+ (%stereo-right x) (%stereo-right y)))))

(defun scale-sample (x y)
  (declare (optimize (speed 3))
           (type mono-sample x y))
  (ash (* x y) -16))

(defun scale-stereo-sample (stereo scale)
  (declare (optimize (speed 3))
           (type stereo-sample stereo)
           (type (signed-byte 16) scale))
  #+NIL
  (logior (logand #xFFFF0000 (* scale (ash stereo -16)))
          (logand #x0000FFFF (ash (* (logand stereo #xFFFF) scale) -16)))

  (stereo-sample (scale-sample (stereo-left  stereo) scale)
                 (scale-sample (stereo-right stereo) scale)))

(define-modify-macro stereo-incf (sample) add-stereo-samples)
(define-modify-macro stereo-mixf (sample) mix-stereo-samples)

;;;; Testing streamer

(defun make-test-streamer ()
  (let ((n 0)
        (phase 0.0))
    (lambda (streamer mixer buffer offset length time)
      (declare (ignore time))
      (loop for index upfrom offset
            repeat length
            with freq = (+ 200 (* n 200))
            with dp = (* 2.0 pi freq 1/44100)
            as sample = (round (* 5000 (sin phase)))
            do
            (stereo-incf (aref buffer index) (mono->stereo sample))
            (incf phase dp))
      (incf n)
      (when (= n 6)
        (mixer-remove-streamer mixer streamer)))))

;;;; Streamers for pre-filled vectors of various formats.

(defmacro meta-vector-streamer (vstreamer modifier type step sample-expr)
  `(with-slots (vector position end) ,vstreamer
     (declare (type array-index end)
              (type ,type vector))
       (loop for vector-index from (the array-index position) by ,step below end
             for output-index from offset below (+ offset length)
             do (,modifier (aref buffer output-index) ,sample-expr)
             finally (setf position vector-index))
       (when (= (the array-index position) end)
         (mixer-remove-streamer mixer ,vstreamer))))

(defclass vector-streamer ()
  ((vector :reader vector-of :initarg :vector)
   (start  :reader start     :initarg :start)
   (end    :reader end       :initarg :end)
   (position :reader position-of :initarg :position)
   (elts-per-sample :reader elts-per-sample :initarg :elts-per-sample)
   (seek-to :accessor seek-to :initform nil)))

(defun vector-stream-do-seek (stream)
  (when (seek-to stream)
    (setf (slot-value stream 'position) (seek-to stream)
          (seek-to stream) nil)))

(defmacro define-vector-streamer (name &key type step sample-expr optimize)
  `(progn
     (defclass ,name (vector-streamer) ())
     (defun ,(intern (format nil "MAKE-~A" (symbol-name name)))
         (vector &optional (start 0) (end (length vector)))
       (make-instance ',name :vector vector
                      :start start :end end
                      :position start
                      :elts-per-sample ,step))
     (defmethod streamer-mix-into ((stream ,name) mixer buffer offset length time)
       (declare (type array-index offset length)
                (type sample-vector buffer)
                ,@(and optimize '((optimize (speed 3))))
                (ignore time))
       (vector-stream-do-seek stream)
       (meta-vector-streamer stream stereo-mixf ,type ,step ,sample-expr))
     (defmethod streamer-write-into ((stream ,name) mixer buffer offset length time)
       (declare (type array-index offset length)
                (type sample-vector buffer)
                ,@(and optimize '((optimize (speed 3))))
                (ignore time))
       (vector-stream-do-seek stream)
       (meta-vector-streamer stream setf ,type ,step ,sample-expr))))

(define-vector-streamer vector-streamer-mono
    :type vector
    :step 1
    :sample-expr (mono->stereo (aref vector vector-index)))

(define-vector-streamer vector-streamer-interleaved-stereo
    :type vector
    :step 2
    :sample-expr (stereo-sample (aref vector vector-index)
                                (aref vector (1+ vector-index))))

(define-vector-streamer vector-streamer-joint-stereo
    :type vector
    :step 1
    :sample-expr (aref vector vector-index))

(define-vector-streamer fast-vector-streamer-mono
    :type (simple-array (signed-byte 16) 1)
    :step 1
    :sample-expr (mono->stereo (aref vector vector-index)))

(define-vector-streamer fast-vector-streamer-interleaved-stereo
    :type (simple-array (signed-byte 16) 1)
    :optimize t
    :step 2
    :sample-expr (stereo-sample (aref vector vector-index)
                                (aref vector (1+ vector-index))))

(define-vector-streamer fast-vector-streamer-joint-stereo
    :type sample-vector
    :optimize t
    :step 1
    :sample-expr (aref vector vector-index))

(define-vector-streamer vector-streamer-mono-single-float
    :type (vector single-float)
    :step 1
    :sample-expr (mono->stereo (clamp-sample (round (* 32767.0f0 (aref vector vector-index))))))

(define-vector-streamer vector-streamer-mono-double-float
    :type (vector double-float)
    :step 1
    :sample-expr (mono->stereo (clamp-sample (round (* 32767.0d0 (aref vector vector-index))))))

(defmethod streamer-seekable-p ((stream vector-streamer) mixer)
  (declare (ignore mixer))
  t)

(defmethod streamer-length ((stream vector-streamer) mixer)
  (declare (ignore mixer))
  (/ (- (end stream) (start stream))
     (elts-per-sample stream)))

;;; There's a race condition here where a seek on a vector stream can,
;;; with very small probability, be ignored if there's already a
;;; previous seek pending, but I don't think it's worth coding around.
(defmethod streamer-seek
    ((stream vector-streamer) mixer position &key &allow-other-keys)
  (declare (ignore mixer))
  (setf (seek-to stream) (min (- (end stream) (elts-per-sample stream))
                              (max (start stream)
                                   (+ (* (elts-per-sample stream)
                                         position)
                                      (start stream))))))

(defmethod streamer-position ((stream vector-streamer) mixer)
  (declare (ignore mixer))
  (floor (- (position-of stream)
            (start stream))
         (elts-per-sample stream)))

(defclass dummy-mixer ()
  ((rate :initform 44100 :initarg :rate)
   (callback :initform (lambda () nil) :initarg :callback-on-streamer-remove :reader dummy-mixer-callback)))

(defmethod mixer-remove-streamer ((mixer dummy-mixer) streamer)
  (declare (ignore streamer))
  (funcall (dummy-mixer-callback mixer)))
