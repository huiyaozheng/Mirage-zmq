import zmq

context = zmq.Context()

#  Socket to talk to server
print("Connecting to hello world server…")
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5556")
print("Sending request …")
socket.send(b"Hello")
message = socket.recv()
print("Received reply [ %s ]" % message)