import sys
import matplotlib.pyplot as plt
import csv
import numpy

y = numpy.array([10, 50, 100, 500, 1000, 5000, 10000, 50000])

zmq = numpy.array([8300000, 4900000, 3750000, 1250000, 776000, 210000, 120000, 78000]) * y / 1024 / 1024
jeromq = numpy.array([6100000, 4600000, 3400000, 1200000, 670000, 160000, 75000, 26000]) * y / 1024 / 1024
solo5 = numpy.array([1450000, 810000, 500000, 125000, 82000, 22000, 12000, 2560]) * y / 1024 / 1024
exe = numpy.array([405000, 400000, 390000, 302000, 250000, 100000, 81000, 37000]) * y / 1024 / 1024

plt.xscale('log')
plt.plot(y, zmq)
plt.plot(y, jeromq, linestyle='--')
plt.plot(y, solo5, linestyle='-.')
plt.plot(y, exe, linestyle=':')


plt.legend(['libzmq', 'JeroMQ', 'Mirage-zmq (unikernel)', 'Mirage-zmq (executable)'], loc='upper left')
plt.title('Throughput vs message size')
plt.xlabel('Message size [Byte]')
plt.ylabel('Throughput [MB/s]')
plt.savefig('throughput.pdf')
