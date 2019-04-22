(defsystem "ql2nix"
  :description "Convert quicklisp packages into nix packages"
  :version "1.0.0"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "MIT"
  :components ((:file "main"))
  :entry-point "ql2nix::main")
