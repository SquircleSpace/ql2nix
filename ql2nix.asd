(defsystem "ql2nix"
  :class :package-inferred-system
  :description "Convert quicklisp packages into nix packages"
  :version "1.0.0"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "MIT"
  :depends-on ("ql2nix/main")
  :entry-point "ql2nix/main::main")
