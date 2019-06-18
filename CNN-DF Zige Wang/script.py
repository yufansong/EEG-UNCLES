import numpy as np
from scipy.io import loadmat
from matplotlib import pyplot as plt


def average_insub(s, band):
    datafile = '../bandpassdata/' + band + '/CLAS_VP' + str(s+1).zfill(2) + '_' + band + '.mat'
    rawfile = loadmat(datafile)
    rawdata = rawfile['onefilter'][:]
    label = rawfile['label'][:]
    glabel = rawfile['glabel'][:]

    averagedata = np.zeros((2, 2, 128, 4000))
    for con in range(2):
        condata = rawdata[con, :, :, :]
        conlabel = label[con, :]
        conglabel = glabel[con, :]
        filterdata = condata[:, :, conglabel == 1]
        filterlabel = conlabel[conglabel == 1]

        classdata1 = filterdata[:, :, filterlabel == 13]
        classave1 = np.average(classdata1, axis=2)
        averagedata[con, 0, :, :] = classave1

        classdata2 = filterdata[:, :, filterlabel == 23]
        classave2 = np.average(classdata2, axis=2)
        averagedata[con, 1, :, :] = classave2

    return averagedata


a = average_insub(7, 'theta')
onedata = a[0, 0, :, :]

# b = np.reshape(np.repeat(np.arange(0, 20, 4), 4000), [5, 4000])
one = onedata[47, :]

plt.figure()
# for i in range(5):
#     plt.plot(one[i, :])

plt.plot(one, linewidth=0.5, color='blue')
plt.title("Theta")
plt.xlabel("Time")
plt.show()
