#!/bin/sh

case "`uname`" in
    Linux) os_linux=1 ;;
    Darwin) os_darwin=1 ;;
    SunOs) os_sunos=1 ;;
    AIX) os_aix=1 ;;
    *) echo "Unable to identify uname " `uname`
       exit 1 ;;	
esac
    
if [ "$os_linux" ]; then
    gcc $CFLAGS -fPIC -c $SOURCE -o $OBJECT
    gcc -shared $OBJECT $LDFLAGS -o $SHARED_LIB

elif [ "$os_darwin" ]; then
    cc $CFLAGS -dynamic -c $SOURCE -o $OBJECT
    ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o $BASE.dylib $OBJECT
    ld -bundle /usr/lib/bundle1.o $LDFLAGS -flat_namespace -undefined suppress /usr/lib/libz.dylib -o z.dylib

elif [ "$os_sunos" ]; then
    cc $CFLAGS -KPIC -c $SOURCE -o $OBJECT
    cc -G $OBJECT $LDFLAGS -o $SHARED_LIB

elif [ "$os_aix" ]; then
    gcc $CFLAGS -c -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void $SOURCE
    make_shared $LDFLAGS -o $SHARED_LIB $OBJECT
fi

exit 0
