import zmq
import time
from threading import Thread
context = zmq.Context()

#  Socket to talk to server
print("Starting server...")
socket = context.socket(zmq.XPUB)
socket.bind("tcp://127.0.0.1:5556")
while True:
    socket.send(b"ABC")
    time.sleep(1)
    try:
        rc = socket.recv(zmq.NOBLOCK)
        print(rc)
    except zmq.ZMQError:
        print("nothing yet")
