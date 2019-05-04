import sys
import matplotlib.pyplot as plt
import csv
import numpy

title = "Server latency vs Number of clients"
output = "comparison_latency.pdf"
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

    plt.setp(bp['boxes'][3], color='green')
    plt.setp(bp['medians'][3], color='green')
    plt.setp(bp['caps'][6], color='green')
    plt.setp(bp['caps'][7], color='green')
    plt.setp(bp['whiskers'][6], color='green')
    plt.setp(bp['whiskers'][7], color='green')
    
    

def read(input, numThreads):
    with open(input, 'r') as csvfile:
        plots = csv.reader(csvfile, delimiter=',')
        title = True
        y = []
        for row in plots:
            if title:
                title = False
            else:
                sum = 0
                for i in row[1:]:
                    sum = sum + float(row[1])
                y.append(sum / numThreads)
    return numpy.array(y)

def plot_box(info_list, colors):
    data = {}
    data['1 client'] = []
    data['2 clients'] = []
    data['10 clients'] = []
    data['100 clients'] = []
    no_of_messages = 1000
    server_names = []
    plots = ['1 client', '2 clients', '10 clients', '100 clients']
    for server_name, prefix in info_list:
        prefix = prefix + "_"
        server_names.append(server_name)
        
        data['1 client'].append(read(prefix + "single.csv", 1))
        data['2 clients'].append(read(prefix + "two.csv", 1))
        data['10 clients'].append(read(prefix + "ten.csv", 1))
        data['100 clients'].append(read(prefix + "hundred.csv", 1))


    fig = plt.figure()
    ax = plt.axes()
    plt.title(title)
    ax.set_ylabel(y_label)
    ax.set_yscale('log')
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
    plt.plot([], c='blue', label='Mirage-zmq (unikernel)')
    plt.plot([], c='green', label='Mirage-zmq (executable)')
    plt.legend()

    plt.savefig(output)

plot_box(
    [("libzmq", "cpp"), ("JeroMQ", "java"), ("Mirage-zmq (unikernel)",
                                             "unikernel"), ("Mirage-zmq (executable)", "exe")],
    {'libzmq': 'red', 'JeroMQ': 'black', 'Mirage-zmq (unikernel)': 'blue', "Mirage-zmq (executable)": 'green'})
