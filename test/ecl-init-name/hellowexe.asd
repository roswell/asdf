(defsystem "hellowexe"
  :components ((:file "helloexe"))
  :class program-system
  :build-operation program-op
  :build-pathname "hellowexe"
  :prologue-code "{extern void init_lib_HELLOW(cl_object);ecl_init_module(NULL, init_lib_HELLOW);}"
  :epilogue-code (progn
                   (format t "~%Good bye sunshine.~%")
                   (ext:quit 0))
  :extra-build-args (:ld-flags #.(list (namestring (compile-file-pathname "hellow" :type :lib)))))
