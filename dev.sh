#!/bin/sh

# Blow up on any error
set -e

while true; do
    /bin/echo -n "Generating ImageSearch.html... "
    pandoc --toc --smart -T "Using the Dropbox API from Haskell" -c ImageSearch.css -t html+lhs -s ImageSearch.lhs > ImageSearch.html;
    echo "done!"

    # wait until file changes for recompile
    case $(uname) in
        Linux) inotifywait -e modify ImageSearch.lhs ;;
        Darwin) cat <<EOF | python2.6 - ImageSearch.lhs ;;
# This should be in haskell
import os
import select
import sys

def block_on_file_write(path):
    O_EVTONLY = 0x8000

    kq = None
    fd = None
    try:
        kq = select.kqueue()
        fd = os.open(path, O_EVTONLY)

        # add file to kqueue and wait for write
        kq.control([select.kevent(ident=fd,
                                  filter=select.KQ_FILTER_VNODE,
                                  flags=(select.KQ_EV_ADD |
                                         select.KQ_EV_CLEAR),
                                  fflags=select.KQ_NOTE_WRITE)], 1)
    finally:
        if kq is not None:
            kq.close()
        if fd is not None:
            os.close(fd)

if __name__ == "__main__":
    block_on_file_write(sys.argv[1])
EOF
        *)
        echo "This system is not supported for watching file events, sleeping 30 seconds" >2
        sleep 30
        ;;
    esac
done
