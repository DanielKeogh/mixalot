(asdf:defsystem :mixalot
  :name "Mixalot mixer"
  :description "Mixalot mixer for ALSA/libao"
  :version "0.0.3"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:cffi :bordeaux-threads :alexandria
		     :cffi-c-ref :static-vectors 
		     :bodge-openal :float-features
		     :openal-blob)
  :serial t
  :components ((:file "ffi-common")
               (:file "mixalot")))

