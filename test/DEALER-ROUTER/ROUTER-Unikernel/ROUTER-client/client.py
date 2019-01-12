import zmq

context = zmq.Context()

#  Socket to talk to server
print("Connecting to server…")
socket = context.socket(zmq.ROUTER)
socket.bind("tcp://127.0.0.1:5556")
for i in range(10):
    print("Sending request …" + str(i))
    socket.send_string("12345")
    socket.send_string("Hi")
    message = socket.recv()
    print("Received reply [ %s ]" % message)