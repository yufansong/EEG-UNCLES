import numpy as np
import scipy.io
from sklearn.svm import SVC
import random
import h5py

REPEAT_TIME = 100
SUB = 10
METHOD = 'esc'
IS_SPECIFY = 1


def main():
    DATA_FILE1 = '../Coupling/num' + str(SUB).zfill(2) + '_' + METHOD + '_con1.mat'
    DATA_NAME1 = 'n' + str(SUB).zfill(2) + 'e1'
    DATA_FILE2 = '../Coupling/num' + str(SUB).zfill(2) + "_" + METHOD + '_con2.mat'
    DATA_NAME2 = 'n' + str(SUB).zfill(2) + 'e2'
    if IS_SPECIFY:
        LABEL_FILE = '../rawdata/CLAS_VP' + str(SUB).zfill(2) + '_onedata_STBFH_MNT.mat'


    datafile1 = scipy.io.loadmat(DATA_FILE1)
    data1 = datafile1[DATA_NAME1][:]
    datafile2 = scipy.io.loadmat(DATA_FILE2)
    data2 = datafile2[DATA_NAME2][:]
    if IS_SPECIFY:
        labelfile = h5py.File(LABEL_FILE)
        rawlabel = labelfile['allevents'][:]
        glabel = labelfile['allgoodevents'][:]

    input1 = np.reshape(data1, (np.size(data1, axis=0), 128*25*29))
    input2 = np.reshape(data2, (np.size(data2, axis=0), 128*25*29))
    if IS_SPECIFY:
        label1 = rawlabel[glabel[:, 0] == 1, 0]
        label2 = rawlabel[glabel[:, 1] == 1, 1]
        input1 = input1[glabel[:, 0] == 1, :]
        input2 = input2[glabel[:, 1] == 1, :]
    else:
        label1 = np.zeros((80,))
        label2 = np.ones((80,))

    acc = np.zeros((100, 2))

    for i in range(REPEAT_TIME):
        samplenum1 = np.size(input1, axis=0)
        samplenum2 = np.size(input2, axis=0)
        testnum1 = int(samplenum1 * 0.2)
        testnum2 = int(samplenum2 * 0.2)
        test_slice1 = random.sample(range(0, samplenum1), testnum1)
        test_slice2 = random.sample(range(0, samplenum2), testnum2)
        test1 = input1[test_slice1, :]
        test_label1 = label1[test_slice1]
        test2 = input2[test_slice2, :]
        test_label2 = label2[test_slice2]
        train1 = np.delete(input1, test_slice1, axis=0)
        train_label1 = np.delete(label1, test_slice1, axis=0)
        train2 = np.delete(input2, test_slice2, axis=0)
        train_label2 = np.delete(label2, test_slice2, axis=0)

        if IS_SPECIFY:
            clf1 = SVC()
            clf1.fit(train1, train_label1)
            predict1 = clf1.predict(test1)
            correct_num1 = np.sum(np.equal(predict1, test_label1)).astype('uint32')
            acc[i, 0] = correct_num1 / testnum1
            print("Sub %d, Con %d, Repeat %d, Accuracy %f" % (SUB, 1, i, acc[i, 0]))

            clf2 = SVC()
            clf2.fit(train2, train_label2)
            predict2 = clf2.predict(test2)
            correct_num2 = np.sum(np.equal(predict2, test_label2)).astype('uint32')
            acc[i, 1] = correct_num2 / testnum2
            print("Sub %d, Con %d, Repeat %d, Accuracy %f" % (SUB, 2, i, acc[i, 1]))
        else:
            train = np.concatenate((train1, train2), axis=0)
            train_label = np.concatenate((train_label1, train_label2), axis=0)
            test = np.concatenate((test1, test2), axis=0)
            test_label = np.concatenate((test_label1, test_label2), axis=0)

            train_zip = list(zip(train, train_label))
            test_zip = list(zip(test, test_label))
            random.shuffle(train_zip)
            random.shuffle(test_zip)
            train, train_label = zip(*train_zip)
            test, test_label = zip(*test_zip)

            clf = SVC()
            clf.fit(train, train_label)

            predict = clf.predict(test)
            correct_num = np.sum(np.equal(predict, test_label)).astype('uint32')
            acc[i, 0] = correct_num / (testnum1 + testnum2)

            print("Sub %d, Repeat %d, Accuracy %f" % (SUB, i, acc[i, 0]))

    mean = np.mean(acc, axis=0)
    std = np.std(acc, axis=0)
    if IS_SPECIFY:
        print("Sub %d, Con 1, Mean %f, Std %f" % (SUB, mean[0], std[0]))
        print("Sub %d, Con 2, Mean %f, Std %f" % (SUB, mean[1], std[1]))
    else:
        print("Sub %d, Mean %f, Std %f" % (SUB, mean[:, 0], std[:, 0]))


if __name__ == '__main__':
    main()
