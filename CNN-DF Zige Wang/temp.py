import numpy as np
from matplotlib import pyplot as plt
from sklearn import preprocessing
from scipy import stats

BAND = 'theta'
NORMLIZE = 0

min_max_scaler = preprocessing.MinMaxScaler()

data2 = np.load("./tw500_step100/SVM_Sub02_tw500_step100_" + BAND + ".npz")
mean2 = data2['acc'][:]
if NORMLIZE:
    mean2 = np.transpose(min_max_scaler.fit_transform(np.transpose(mean2)))
#mean2[mean2 < 0.495] = 0

data6 = np.load("./tw500_step100/SVM_Sub06_tw500_step100_" + BAND + ".npz")
mean6 = data6['acc'][:]
if NORMLIZE:
    mean6 = np.transpose(min_max_scaler.fit_transform(np.transpose(mean6)))
#mean6[mean6 < 0.495] = 0

data8 = np.load("./tw500_step100/SVM_Sub08_tw500_step100_" + BAND + ".npz")
mean8 = data8['acc'][:]
if NORMLIZE:
    mean8 = np.transpose(min_max_scaler.fit_transform(np.transpose(mean8)))
#mean8[mean8 < 0.495] = 0

data9 = np.load("./tw500_step100/SVM_Sub09_tw500_step100_" + BAND + ".npz")
mean9 = data9['acc'][:]
if NORMLIZE:
    mean9 = np.transpose(min_max_scaler.fit_transform(np.transpose(mean9)))
#mean9[mean9 < 0.495] = 0

data13 = np.load("./tw500_step100/SVM_Sub13_tw500_step100_" + BAND + ".npz")
mean13 = data13['acc'][:]
if NORMLIZE:
    mean13 = np.transpose(min_max_scaler.fit_transform(np.transpose(mean13)))
#mean13[mean13 < 0.495] = 0

data16 = np.load("./tw500_step100/SVM_Sub16_tw500_step100_" + BAND + ".npz")
mean16 = data16['acc'][:]
if NORMLIZE:
    mean16 = np.transpose(min_max_scaler.fit_transform(np.transpose(mean16)))
#mean16[mean16 < 0.495] = 0

x = range(0, 36)
# fig1 = plt.figure()
# plot21, = plt.plot(x, mean2[0, :], '')
# plot61, = plt.plot(x, mean6[0, :], '')
# plot81, = plt.plot(x, mean8[0, :], '')
# plot91, = plt.plot(x, mean9[0, :], '')
# plot131, = plt.plot(x, mean13[0, :], '')
# plot161, = plt.plot(x, mean16[0, :], '')
# plt.xlabel("Windows")
# plt.legend([plot21, plot61, plot81, plot91, plot131, plot161], ['Sub02', 'Sub06', 'Sub08', 'Sub09', 'Sub13', 'Sub16'])
# #plt.show()
# if NORMLIZE:
#     plt.title("Normalized condition 1 in " + BAND + " band")
#     plt.ylabel("Normalized mean accuracy")
#     fig1.savefig("plot_norm_con1_" + BAND + ".png")
# else:
#     plt.title("Condition 1 in " + BAND + " band")
#     plt.ylabel("Mean accuracy")
#     fig1.savefig("plot_mean_con1_" + BAND + ".png")

# fig2 = plt.figure()
# plot22, = plt.plot(x, mean2[1, :], '')
# plot62, = plt.plot(x, mean6[1, :], '')
# plot82, = plt.plot(x, mean8[1, :], '')
# plot92, = plt.plot(x, mean9[1, :], '')
# plot132, = plt.plot(x, mean13[1, :], '')
# plot162, = plt.plot(x, mean16[1, :], '')
# plt.xlabel("Windows")
# plt.legend([plot22, plot62, plot82, plot92, plot132, plot162], ['Sub02', 'Sub06', 'Sub08', 'Sub09', 'Sub13', 'Sub16'])
# #plt.show()
# if NORMLIZE:
#     plt.title("Normalized condition 2 in " + BAND + " band")
#     plt.ylabel("Normalized mean accuracy")
#     fig2.savefig("plot_norm_con2_" + BAND + ".png")
# else:
#     plt.title("Condition 2 in " + BAND + " band")
#     plt.ylabel("Mean accuracy")
#     fig2.savefig("plot_mean_con2_" + BAND + ".png")

conmean1 = np.vstack((mean2[0, :], mean6[0, :], mean8[0, :], mean9[0, :], mean13[0, :], mean16[0, :]))
conmean2 = np.vstack((mean2[1, :], mean6[1, :], mean8[1, :], mean9[1, :], mean13[1, :], mean16[1, :]))
avemean1 = np.average(conmean1, axis=0)
avemean2 = np.average(conmean2, axis=0)
fig3 = plt.figure()
plotave1, = plt.plot(x, avemean1, '')
plotave2, = plt.plot(x, avemean2, '')
plt.xlabel("Windows")
plt.legend([plotave1, plotave2], ['Condition 1', 'Condition 2'])
if NORMLIZE:
    plt.title("Normalized average across subjects in " + BAND + " band")
    plt.ylabel("Normalized mean accuracy")
    fig3.savefig("plot_norm_ave_" + BAND + ".png")
else:
    plt.axhline(0.5, linestyle='dashed', color='red')
    plt.title("Average across subjects in " + BAND + " band")
    plt.ylabel("Mean accuracy")
    fig3.savefig("plot_mean_ave_" + BAND + ".png")
plt.show()




