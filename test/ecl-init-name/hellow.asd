(defsystem "hellow"
  :components ((:file "hello"))
  :class program-system
  :build-operation lib-op
  :extra-build-args (:init-name "init_lib_HELLOW")
  :build-pathname "libhellow")
