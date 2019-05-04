import sys
import matplotlib.pyplot as plt
import csv
import numpy

title = "Server latency vs Message size"
output = "comparison_latency_macos.pdf"
y_label = "Latency (μs)"


def setBoxColors(bp):
    plt.setp(bp['boxes'][0], color='red')
    plt.setp(bp['medians'][0], color='red')
    plt.setp(bp['caps'][0], color='red')
    plt.setp(bp['caps'][1], color='red')
    plt.setp(bp['whiskers'][0], color='red')
    plt.setp(bp['whiskers'][1], color='red')

    plt.setp(bp['boxes'][1], color='black')
    plt.setp(bp['medians'][1], color='black')
    plt.setp(bp['caps'][2], color='black')
    plt.setp(bp['caps'][3], color='black')
    plt.setp(bp['whiskers'][2], color='black')
    plt.setp(bp['whiskers'][3], color='black')

    plt.setp(bp['boxes'][2], color='blue')
    plt.setp(bp['medians'][2], color='blue')
    plt.setp(bp['caps'][4], color='blue')
    plt.setp(bp['caps'][5], color='blue')
    plt.setp(bp['whiskers'][4], color='blue')
    plt.setp(bp['whiskers'][5], color='blue')

def read(input):
    with open(input, 'r') as csvfile:
        plots = csv.reader(csvfile, delimiter=',')
        rows = []
        for row in plots:
            rows.append(numpy.array(row).astype(numpy.float))
    return rows

def plot_box(info_list, colors):
    data = {}
    data['10B'] = []
    data['50B'] = []
    data['100B'] = []
    data['500B'] = []
    data['1KB'] = []
    data['5KB'] = []
    data['10KB'] = []
    data['50KB'] = []
    data['100KB'] = []
    no_of_messages = 50
    server_names = []
    plots = ['10B', '50B', '100B', '500B', '1KB', '5KB', '10KB', '50KB', '100KB']
    for server_name, prefix in info_list:
        prefix = prefix + "_"
        server_names.append(server_name)
        rows = read(prefix+"lat.csv")
        data['10B'].append(rows[0])
        data['50B'].append(rows[1])
        data['100B'].append(rows[2])
        data['500B'].append(rows[3])
        data['1KB'].append(rows[4])
        data['5KB'].append(rows[5])
        data['10KB'].append(rows[6])
        data['50KB'].append(rows[7])
        data['100KB'].append(rows[8])


    fig = plt.figure()
    ax = plt.axes()
    plt.title(title)
    ax.set_ylabel(y_label)
    plt.ylim(bottom=0,top=900)
    number_per_group = len(server_names) #3
    for index, plot in enumerate(plots):
        bp = ax.boxplot(data[plot], positions=range(
            index * (number_per_group + 1) + 1, (index + 1) * (number_per_group + 1) ), widths=0.6, sym='')
        setBoxColors(bp)

    ax.set_xticklabels(plots)
    ax.set_xticks([2 + (number_per_group + 1) * i for i in range(0, len(plots))])
    plt.xlim(0, (number_per_group + 1) * len(plots) + 1)

    plt.plot([], c='red', label='libzmq')
    plt.plot([], c='black', label='JeroMQ')
    plt.plot([], c='blue', label='Mirage-zmq (executable)')
    plt.legend()

    plt.savefig(output)

plot_box(
    [("libzmq", "cpp"), ("JeroMQ", "java"), ("Mirage-zmq (executable)", "exe")],
    {'libzmq': 'red', 'JeroMQ': 'black', 'Mirage-zmq (unikernel)': 'blue'})
