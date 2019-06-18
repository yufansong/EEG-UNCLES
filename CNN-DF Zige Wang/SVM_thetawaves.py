import numpy as np
from sklearn.svm import SVC
from scipy.io import loadmat
import random
import h5py
from sklearn import preprocessing

REPEAT_TIME = 100
SUB_LIST = [2, 3, 4, 6, 8, 9, 11, 13, 14, 16, 19, 20]


def main():
    data = h5py.File('../CNN_Dataset_180_con_gamma.mat')
    for con in range(2):
        CONDITION = con + 1
        eeg = data['CNNinterp_con' + str(CONDITION)][:]
        num_train = len(eeg)
        label = data['CNNlabel_con' + str(CONDITION)][:, 1]
        onsetnum = data['Onsetnumfilter'][:]

        # normalization and reshape
        max_min_scaler = preprocessing.MinMaxScaler()
        for i in range(num_train):
            eeg[i, :, :] = max_min_scaler.fit_transform(eeg[i, :, :])
        eeg = np.reshape(eeg, [num_train, 180, 128, 1])

        subject_begin = np.zeros((20,), dtype='uint32')
        start_point = 0
        for s in range(20):
            subject_num = int(onsetnum[s][CONDITION - 1])
            subject_begin[s] = start_point
            start_point += subject_num
        correct_set = np.zeros((20, 100))

        for s in SUB_LIST:
            subject_num = int(onsetnum[s - 1][CONDITION - 1])
            start_point = subject_begin[s - 1]
            num_train = int(subject_num * 0.8)
            num_test = subject_num - num_train
            subject_set = eeg[start_point:(start_point + subject_num), :, :, :]
            subject_label = label[start_point:(start_point + subject_num)]

            for n in range(REPEAT_TIME):
                test_slice = random.sample(range(0, subject_num), num_test)
                test_set = subject_set[test_slice, :, :, :]
                test_label = subject_label[test_slice]
                train_set = np.delete(subject_set, test_slice, axis=0)
                train_label = np.delete(subject_label, test_slice, axis=0)
                train_set = np.reshape(train_set, [num_train, 180*128])
                test_set = np.reshape(test_set, [num_test, 180*128])

                clf = SVC()
                clf.fit(train_set, train_label)

                predict = clf.predict(test_set)
                correct_num = np.sum(np.equal(predict, test_label)).astype('uint32')
                correct_set[s-1, n] = correct_num/num_test
                print("Sub %d Con %d Repeat %d: Accuracy on test data: %g" % (
                s, CONDITION, (n + 1), correct_set[s-1, n],))
        np.save("experiments_insub/SVM/SVM_100_INSUB_INTERP_CON" + str(CONDITION) + ".npy", correct_set)


if __name__ == "__main__":
    main()
