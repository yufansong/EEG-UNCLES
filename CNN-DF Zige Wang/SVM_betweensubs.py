import numpy as np
from sklearn.svm import SVC
from scipy.io import loadmat
import random

REPEAT_TIME = 100
# BAND = {'theta': [[0, 13, 23, 30, 33], [0, 10, 16, 24, 28]], 'alpha': [[0, 12, 15, 19, 30], [0, 6, 10, 14, 21]],
#         'lowbeta': [[0, 3, 17, 19, 29], [3, 13, 18, 23, 34]]}
BAND = ['lowgamma', 'highgamma']


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


def main():
    for band in BAND:
        print("band test %s" % band)
        resultfile = './SVM_betweensubs/SVM_Betweensubs_' + band + '.npz'
        # windows = BAND[band]

        subavedata = np.zeros((2, 20, 2, 128, 4000))
        for s in range(20):
            subave = average_insub(s, band)
            subavedata[0, s, :, :, :] = subave[0, :, :, :]
            subavedata[1, s, :, :, :] = subave[1, :, :, :]
        subavelabel = np.zeros((20, 2))
        subavelabel[:, 1] = 1

        accuracy = np.zeros((2, 100))
        for con in range(2):
            # conwindow = windows[con]
           #  for w in conwindow:
                windowdata = subavedata[con, :, :, :, :]
                for i in range(REPEAT_TIME):
                    testsub = random.sample(range(0, 20), 2)
                    testdata = np.reshape(windowdata[testsub, :, :, :], [4, 128*4000])
                    testlabel = np.reshape(subavelabel[testsub, :], [4])
                    traindata = np.reshape(np.delete(windowdata, testsub, axis=0), [36, 128*4000])
                    trainlabel = np.reshape(np.delete(subavelabel, testsub, axis=0), [36])

                    clf = SVC()
                    clf.fit(traindata, trainlabel)

                    predict = clf.predict(testdata)
                    correct_num = np.sum(np.equal(predict, testlabel)).astype('uint32')
                    accuracy[con, i] = correct_num/4
                mean = np.mean(accuracy[con, :])
                std = np.std(accuracy[con, :])
                print("Band %s Con %d Mean %f Std %f" % (band, con, mean, std))
        np.save(resultfile, accuracy)


if __name__ == "__main__":
    main()
