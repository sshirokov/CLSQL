#!/bin/sh

UNAME=`uname -a`
LINUX=`expr "$UNAME" : '.*Linux.*'`
DARWIN=`expr "$UNAME" : '.*Darwin.*'`
SOLARIS=`expr "$UNAME" : '.*sun4u.*'`
AIX=`expr "$UNAME" : '.*aix.*'`


if [ $LINUX -ne 0 ]; then
    gcc -fPIC -DPIC $CFLAGS -c $SOURCE -o $OBJECT
    gcc -shared $OBJECT $LDFLAGS -o $SHARED_LIB
    #gcc -shared -Wl,-soname,uffi-c-test-lib $OBJECT -o $SHARED_LIB
elif [ $DARWIN -ne 0 ]; then
    cc -dynamic $CFLAGS -c $SOURCE -o $OBJECT
    ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o $BASE.dylib $OBJECT $LDFLAGS
    ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress /usr/lib/libz.dylib -o z.dylib
elif [ $SOLARIS -ne 0 ]; then
    cc -KPIC $CFLAGS -c $SOURCE -o $OBJECT
    cc -G $OBJECT  $LDFLAGS -o $SHARED_LIB
elif [ $AIX -ne 0 ]; then
    gcc $CFLAGS -c -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void $SOURCE
    make_shared  $LDFLAGS -o $SHARED_LIB $OBJECT
fi


