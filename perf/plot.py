import sys
import matplotlib.pyplot as plt
import csv
title = "Java server latency"

if len(sys.argv) != 3:
    print ('Need to specify input csv and output graph filenames')

input = sys.argv[1]
output = sys.argv[2]

x = []
y = []

with open(input,'r') as csvfile:
    plots = csv.reader(csvfile, delimiter=',')
    title = True
    for row in plots:
        if title:
            title = False
            x_label = row[0]
            y_label = row[1]
        else:
            x.append(int(row[0]))
            y.append(float(row[1]))

x.pop(0)
y.pop(0)

plt.plot(x,y)
plt.xlabel(x_label)
plt.ylabel(y_label)
plt.title(title)
plt.legend()
plt.savefig(output)