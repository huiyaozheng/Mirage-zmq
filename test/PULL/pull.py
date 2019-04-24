import zmq
import time
context = zmq.Context()

#  Socket to talk to server
print("Starting server...")
socket = context.socket(zmq.PULL)
socket.connect("tcp://127.0.0.1:5556")
while True:
    work_item = socket.recv_string()
    print(work_item)
    