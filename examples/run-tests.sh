#!/bin/bash
#
# Run CLSQL tests on all installed CL implementations
# Need to setup ~/.clsql-tests.config as show in
# tests/test-init.lisp

CMD='(asdf:operate (quote asdf:test-op) :clsql-classic)
  (asdf:operate (quote asdf:test-op) :clsql)
  #+allegro (excl:exit :quiet t)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit)
  #+cmu (ext:quit)
  #+lispworks (lw:quit)
  #+openmcl (ccl:quit)
  #+sbcl (sb-ext:quit))
  #+scl (ext:quit)'



ALLEGRO=alisp
if [ "`which $ALLEGRO`" ]; then
  echo "$CMD" | $ALLEGRO
fi

CMUCL=lisp
if [ "`which $CMUCL`" ]; then
  echo "$CMD" | $CMUCL
fi

LISPWORKS=lw-console
#LISPWORKS=lispworks-4300
if [ "`which $LISPWORKS`" ]; then
  echo "$CMD" | $LISPWORKS
fi

OPENCML=openmcl
if [ "`which $OPENMCL`" ]; then
  echo "$CMD" | $OPENMCL
fi

SBCL=sbcl
if [ "`which $SBCL`" ]; then
  echo "$CMD" | $SBCL
fi
