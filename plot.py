import matplotlib
import numpy

matplotlib.use('PDF')

import matplotlib.pyplot as plt

standard_data = numpy.genfromtxt("timing-mk.tsv", names=True, delimiter="\t")
backjumping_data = numpy.genfromtxt("timing-backjumping2.tsv", names=True, delimiter="\t")


plt.plot(standard_data['quine'],
        standard_data['sincelast'] / 1000,
        label="simple-minikanren",
        linestyle="None",
        marker="o")

plt.plot(backjumping_data['quine'],
        backjumping_data['sincelast'] / 1000,
        label="backjumping",
        linestyle="None",
        marker="o")

plt.axes().set_yscale("log")
plt.axes().set_xlabel("Quine")
plt.axes().set_ylabel("Incremental Seconds")
plt.legend(loc=4, numpoints=1, fontsize="medium")
plt.axes().set_title("Incremental quine generation times")

plt.savefig("plots/perquine")
plt.clf()


size = min(len(standard_data['quine']), len(backjumping_data['quine']))

plt.plot(standard_data['quine'][:size],
        standard_data['time'][:size] / backjumping_data['time'][:size],
        linestyle="None",
        marker="o")

plt.axes().set_xlabel("# Quines")
plt.axes().set_ylabel("standard time / backjumping time")
plt.axes().set_title("Ratio of total time to generate n quines")
plt.savefig("plots/totalratio")



plt.clf()


def moving_average(a, n=10) :
    ret = numpy.cumsum(a, dtype=float)
    ret[n:] = ret[n:] - ret[:-n]
    return ret[n - 1:] / n


plt.plot(standard_data['quine'][9:size],
        moving_average(standard_data['sincelast'][:size]) / moving_average(backjumping_data['sincelast'][:size]),
        linestyle="None",
        marker="o")

plt.axes().set_yscale("log")
plt.axes().set_xlabel("Quine")
plt.axes().set_ylabel("standard time / backjumping time")
plt.axes().set_title("Ratios of incremental quine generation times (moving average of 10)")
plt.savefig("plots/incratiomovavg")
plt.clf()


plt.plot(standard_data['quine'][:size],
        (standard_data['sincelast'][:size]) / (backjumping_data['sincelast'][:size]),
        linestyle="None",
        marker="o")

plt.axes().set_yscale("log")
plt.axes().set_xlabel("Quine")
plt.axes().set_ylabel("standard time / backjumping time")
plt.axes().set_title("Ratios of incremental quine generation times")
plt.savefig("plots/incratio")
plt.clf()




plt.plot(standard_data['quine'][9:],
        moving_average(standard_data['sincelast'] / 1000),
        label="simple-minikanren",
        linestyle="None",
        marker="o")

plt.plot(backjumping_data['quine'][9:],
        moving_average(backjumping_data['sincelast'] / 1000),
        label="backjumping",
        linestyle="None",
        marker="o")

plt.axes().set_yscale("log")
plt.axes().set_xlabel("Quine")
plt.axes().set_ylabel("Incremental Seconds")
plt.legend(loc=4, numpoints=1, fontsize="medium")
plt.axes().set_title("Incremental quine generation times (moving average of 10)")

plt.savefig("plots/perquinemovavg")
plt.clf()




#data2 = numpy.genfromtxt("libsoft-vs-nebo.tsv", names=True, delimiter="\t")

#print data.dtype.names

#plt.plot(data2['Problem_Size'],
        #(data2['Nebo__GCC'] / data2['Libsoft__GCC']),
        #label="GCC Prototype Speedup",
        #linestyle="None",
        #marker="o")

#plt.plot(data2['Problem_Size'],
        #(data2['Nebo__ICC'] / data2['Libsoft__ICC']),
        #label="ICC Prototype Speedup",
        #linestyle="None",
        #marker="o")

#plt.axes().set_xlabel("Data Size")
#plt.axes().set_ylabel("Speedup - Prototype vs existing SpatialOps")
#plt.legend(loc=3, numpoints=1, fontsize="medium")
#plt.axes().set_title("Prototype Reimplementation Speedup")
#plt.xlim(90, 1010)
#plt.ylim(0, 10)

#plt.savefig('libsoft-vs-nebo')

