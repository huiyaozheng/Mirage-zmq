cp ../../../src/mirage_zmq.ml ./mirage_zmq.ml
cp ../../../src/mirage_zmq.mli ./mirage_zmq.mli
mirage configure -t macosx --net socket
make depend
make