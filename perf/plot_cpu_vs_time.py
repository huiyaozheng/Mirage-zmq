import sys
import matplotlib.pyplot as plt
import csv
import numpy

title = "Comparison of server CPU usage"
output = "comparison_cpu.pdf"
y_label = "CPU usage (%)"


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

x_zmq, y_zmq = read('cpp_cpu.csv')
x_jeromq, y_jeromq = read('java_cpu.csv')
x_exe, y_exe = read('exe_cpu.csv')
x_unikernel, y_unikernel = read('unikernel_cpu.csv')

plt.ylim(0,100)
plt.xlim(0,150)
plt.plot(x_zmq, y_zmq, color='r')
plt.plot(x_jeromq, y_jeromq, linestyle='--', color='black')
plt.plot(x_unikernel, y_unikernel, linestyle='-.',color='blue')
plt.plot(x_exe, y_exe, linestyle=':',color='g')


plt.legend(['libzmq', 'JeroMQ', 'Mirage-zmq (unikernel)',
            'Mirage-zmq (executable)'], loc='center right')
plt.title('Total System Memory (single server) vs time')
plt.xlabel('Time [s]')
plt.ylabel('CPU [%]')
plt.savefig('cpu.pdf', bbox_inches='tight')
