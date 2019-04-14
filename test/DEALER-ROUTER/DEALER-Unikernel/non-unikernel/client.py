import zmq

context = zmq.Context()

#  Socket to talk to server
print("Starting server…")
socket = context.socket(zmq.REP)
socket.connect("tcp://127.0.0.1:5556")
for i in range(10):
    message = socket.recv_string()
    print("Received request: " + message)
    socket.send_string("request")
    