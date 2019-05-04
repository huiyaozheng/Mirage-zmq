import sys
import matplotlib.pyplot as plt
import csv
import numpy

title = "Comparison of server memory"
output = "comparison_memory.pdf"
y_label = "Memory (MB)"


def read(input):
    with open(input, 'r') as csvfile:
        plots = csv.reader(csvfile, delimiter=',')
        title = True
        y = []
        x = []
        counter = 0
        for row in plots:
            if title:
                title = False
            else:
                for i in row[1:]:
                    y.append(i)
                x.append(counter * 0.2)
                counter = counter + 1
    return numpy.array(x), numpy.array(y, dtype=float)

x_zmq, y_zmq = read('cpp_mem.csv')
x_jeromq, y_jeromq = read('java_mem.csv')
x_exe, y_exe = read('exe_mem.csv')
x_unikernel, y_unikernel = read('unikernel_mem.csv')
y_unikernel = y_unikernel / 1024.0

plt.plot(x_zmq, y_zmq)
plt.plot(x_jeromq, y_jeromq, linestyle='--')
plt.plot(x_unikernel, y_unikernel, linestyle='-.')
plt.plot(x_exe, y_exe, linestyle=':')


plt.legend(['libzmq', 'JeroMQ', 'Mirage-zmq (unikernel)',
            'Mirage-zmq (executable)'], loc='center right')
plt.title('Total System Memory (single server) vs time')
plt.xlabel('Time [s]')
plt.ylabel('Memory [MB]')
plt.savefig('memory.pdf')
