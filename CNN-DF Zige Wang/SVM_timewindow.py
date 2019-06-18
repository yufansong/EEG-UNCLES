import numpy as np
from scipy.io import loadmat
from sklearn.svm import SVC
import random

REPEAT_TIME = 100
SUB = 8
BAND = ['theta', 'alpha', 'lowbeta', 'highbeta', 'lowgamma', 'highgamma']


def main():
    for band in BAND:
        print("band test %s" % band)
        resultfile = 'SVM_Sub' + str(SUB).zfill(2) + '_tw500_step100_' + band + '.npz'
        datafile = '../bandpassdata/' + band + '/CLAS_VP' + str(SUB).zfill(2) + '_' + band + '.mat'
        rawfile = loadmat(datafile)
        rawdata = rawfile['onefilter'][:]
        label = rawfile['label'][:]
        glabel = rawfile['glabel'][:]

        acc = np.zeros([2, 36])
        std = np.zeros([2, 36])
        for con in range(2):
            condata = rawdata[con, :, :, :]
            conlabel = label[con, :]
            conglabel = glabel[con, :]
            filterdata = condata[:, :, conglabel == 1]
            filterlabel = conlabel[conglabel == 1]
            numsample = np.size(filterlabel)

            for t in range(0, 3600, 100):
                windowdata = filterdata[:, t:(t+500), :]
                inputdata = np.reshape(windowdata, [128*500, numsample])
                inputdata = np.transpose(inputdata)
                numtrain = int(numsample * 0.8)
                numtest = int(numsample - numtrain)

                windowacc = np.zeros((REPEAT_TIME,))
                for i in range(REPEAT_TIME):
                    testslice = random.sample(range(0, numsample), numtest)
                    testdata = inputdata[testslice, :]
                    testlabel = filterlabel[testslice]
                    traindata = np.delete(inputdata, testslice, axis=0)
                    trainlabel = np.delete(filterlabel, testslice)

                    clf = SVC()
                    clf.fit(traindata, trainlabel)

                    predict = clf.predict(testdata)
                    correct_num = np.sum(np.equal(predict, testlabel)).astype('uint32')
                    windowacc[i] = correct_num/numtest

                acc[con, int(t/100)] = np.mean(windowacc)
                std[con, int(t/100)] = np.std(windowacc)
                print("Sub %d Con %d Time window %d Accuracy %f" % (SUB, con, int(t/100), acc[con, int(t/100)]))
        print(acc)
        np.savez(resultfile, acc=acc, std=std)



if __name__ == '__main__':
    main()
