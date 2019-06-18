__author__ = 'WangZige'

from sklearn import tree
import numpy as np
import h5py
from sklearn import preprocessing
import random
import time

TEST_SUB1 = 10
TEST_SUB2 = 20
VAL_SAMPLE = 1000
CONDITION = 2

SUB_LIST = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]


def main():
    data = h5py.File('../CNN_Dataset_180_con.mat')
    eeg = data['CNNinterp_con' + str(CONDITION)][:]
    num_train = len(eeg)
    label = data['CNNlabel_con' + str(CONDITION)][:, 1]
    onsetnum = data['Onsetnumfilter'][:]

    # normalization and reshape
    max_min_scaler = preprocessing.MinMaxScaler()
    for i in range(num_train):
        eeg[i, :, :] = max_min_scaler.fit_transform(eeg[i, :, :])
    eeg = np.reshape(eeg, [num_train, 180*128])

    # generate train and test slice (leave-two-out)
    '''
    test_num1 = int(onsetnum[TEST_SUB1 - 1][CONDITION - 1])
    test_num2 = int(onsetnum[TEST_SUB2 - 1][CONDITION - 1])
    num_test = test_num1 + test_num2
    test_slice1 = np.arange(test_num1).astype('uint32')
    test_slice2 = np.arange(test_num2).astype('uint32')
    test_begin1 = np.sum(onsetnum[:(TEST_SUB1 - 1), CONDITION - 1]).astype('uint32')
    test_begin2 = np.sum(onsetnum[:(TEST_SUB2 - 1), CONDITION - 1]).astype('uint32')
    test_slice1 += test_begin1
    test_slice2 += test_begin2
    test_slice = np.hstack((test_slice1, test_slice2))
    test_set = eeg[test_slice, :]
    # test_label = label[test_slice, :]
    test_label = label[test_slice]
    train_set = np.delete(eeg, test_slice, axis=0)
    train_label = np.delete(label, test_slice, axis=0)
    '''

    subject_begin = 0
    correct_set = np.zeros((20, 100), dtype='float')
    total = 0

    for s in SUB_LIST:
        s = s - 1
        subject_num = int(onsetnum[s][CONDITION - 1])
        num_train = int(subject_num * 0.8)
        num_test = subject_num - num_train
        subject_set = eeg[subject_begin:(subject_begin + subject_num), :]
        subject_label = label[subject_begin:(subject_begin + subject_num)]
        subject_begin += subject_num

        for i in range(100):
            test_slice = random.sample(range(0, subject_num), num_test)
            test_set = subject_set[test_slice, :]
            test_label = subject_label[test_slice]
            train_set = np.delete(subject_set, test_slice, axis=0)
            train_label = np.delete(subject_label, test_slice, axis=0)

            # train
            # clf = tree.DecisionTreeRegressor(splitter="random")
            tic = time.time()
            clf = tree.DecisionTreeClassifier()
            clf.fit(train_set, train_label)

            # test
            test_result = clf.predict(test_set)
            correct = np.equal(test_label, test_result)
            toc = time.time()
            computing = toc - tic
            total += computing
            print("Repeat %d using %g seconds" % (i, computing))
            correct_num = np.sum(correct).astype('uint32')
            if i % 10 == 0:
                print("subject %d: step %d, correct %d over %d" % (s + 1, i, correct_num, num_test))
            correct_set[s, i] = correct_num / num_test

    average = total/100
    print("Average time %g" % average)
    np.save("DT_100_INSUB_INTERP_CON" + str(CONDITION) + ".npy", correct_set)


if __name__ == '__main__':
    main()
