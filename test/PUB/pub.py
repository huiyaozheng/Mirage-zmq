import zmq
import time
context = zmq.Context()
socket = context.socket(zmq.PUB)
socket.bind("tcp://127.0.0.1:5556")
while True:
    socket.send(b"ABC")