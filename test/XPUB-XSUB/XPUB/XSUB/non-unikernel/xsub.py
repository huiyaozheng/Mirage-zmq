import zmq
import time
context = zmq.Context()

#  Socket to talk to server
print("Starting server...")
socket = context.socket(zmq.XSUB)
socket.connect("tcp://127.0.0.1:5556")
socket.send_string("message from xsub")
work_item = socket.recv_string()
print(work_item)