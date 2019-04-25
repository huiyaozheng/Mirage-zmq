import zmq
import zmq.auth
from zmq.auth.thread import ThreadAuthenticator
context = zmq.Context()
server = context.socket(zmq.REP)

auth = ThreadAuthenticator(context)
auth.start()
auth.allow('127.0.0.1')
auth.configure_plain(domain='*', passwords={'admin': 'password'})
server.plain_server = True
server.bind('tcp://127.0.0.1:5555')

msg = server.recv_string()
server.send(b'Authenticated')

auth.stop()

