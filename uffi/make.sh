#!/bin/sh

UNAME=`uname -a`
LINUX=`expr "$UNAME" : '.*Linux.*'`
DARWIN=`expr "$UNAME" : '.*Darwin.*'`
SOLARIS=`expr "$UNAME" : '.*sun4u.*'`
AIX=`expr "$UNAME" : '.*aix.*'`


if [ $LINUX -ne 0 ]; then
    gcc -fPIC -DPIC -c $SOURCE -o $OBJECT
    gcc -shared $OBJECT -o $SHARED_LIB
    #gcc -shared -Wl,-soname,uffi-c-test-lib $OBJECT -o $SHARED_LIB
elif [ $DARWIN -ne 0 ]; then
    cc -dynamic -c $SOURCE -o $OBJECT
    ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o $BASE.dylib $OBJECT
    ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress /usr/lib/libz.dylib -o z.dylib
elif [ $SOLARIS -ne 0 ]; then
    cc -KPIC -c $SOURCE -o $OBJECT
    cc -G $OBJECT -o $SHARED_LIB
elif [ $AIX -ne 0 ]; then
    gcc -c -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void $SOURCE
    make_shared -o $SHARED_LIB $OBJECT
fi


