__author__ = 'Wang Zige'

import tensorflow as tf
import numpy as np
import h5py
import random
from sklearn import preprocessing
from scipy.io import loadmat

MAX_ITERATION = 50
REPEAT_TIME = 100
BAND = ['theta', 'alpha', 'lowbeta', 'highbeta', 'lowgamma', 'highgamma']


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


def weight_variable(shape):
    initial = tf.truncated_normal(shape, stddev=0.1)  # 截断正态分布，此函数原型为尺寸、均值、标准差
    return tf.Variable(initial)


def bias_variable(shape):
    initial = tf.constant(0.1, shape=shape)
    return tf.Variable(initial)


def conv2d(x, W):
    return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='VALID')  # strides第0位和第3为一定为1，剩下的是卷积的横向和纵向步长


def max_pool_3x1(x):
    return tf.nn.max_pool(x, ksize=[1, 3, 1, 1], strides=[1, 2, 1, 1], padding='SAME')  # 参数同上，ksize是池化块的大小


def max_pool_1x3(x):
    return tf.nn.max_pool(x, ksize=[1, 1, 3, 1], strides=[1, 1, 2, 1], padding='SAME')  # 参数同上，ksize是池化块的大小


def main():
    # The input matrix(128*180)
    x = tf.placeholder(tf.float32, [None, 128*4000])

    # y_ is the true label in the form of one-hot vector
    y_ = tf.placeholder(tf.float32, [None, 2])

    # 图像转化为一个四维张量，第一个参数代表样本数量，-1表示不定，第二三参数代表二位信号
    # reshape the matrix into a four dimensional matrix, 1st parameter is the number of samples, -1 means uncertain
    x_eeg = tf.reshape(x, [-1, 128, 4000, 1])

    # 第一层卷积加池化
    # the first convolution + pooling
    w_conv1 = weight_variable([128, 1, 1, 96])  # 第一二参数值得卷积核尺寸大小，即patch，第三个参数是图像通道数，第四个参数是卷积核的数目，代表会出现多少个卷积特征
    b_conv1 = bias_variable([96])

    h_conv1 = tf.nn.relu(conv2d(x_eeg, w_conv1) + b_conv1)
    h_pool1 = max_pool_1x3(h_conv1)

    # 第二层卷积加池化
    # the second convolution + pooling
    w_conv2 = weight_variable([1, 6, 96, 128])  # 多通道卷积，卷积出128个特征
    b_conv2 = bias_variable([128])

    h_conv2 = tf.nn.relu(conv2d(h_pool1, w_conv2) + b_conv2)
    h_pool2 = max_pool_1x3(h_conv2)

    # 第三层卷积加池化
    # the third convolution + pooling
    w_conv3 = weight_variable([1, 6, 128, 128])  # 多通道卷积，卷积出128个特征
    b_conv3 = bias_variable([128])

    h_conv3 = tf.nn.relu(conv2d(h_pool2, w_conv3) + b_conv3)
    # h_pool3 = max_pool_3x1(h_conv3)

    # 第四层卷积加池化
    # the fourth convolution + pooling
    # w_conv4 = weight_variable([6, 1, 128, 128])  # 多通道卷积，卷积出128个特征
    # b_conv4 = bias_variable([128])

    # h_conv4 = tf.nn.relu(conv2d(h_pool3, w_conv4) + b_conv4)

    # 第一层全连接层
    # the fist fully-connected layer
    w_fc1 = weight_variable([993 * 128, 2048])
    b_fc1 = bias_variable([2048])

    h_conv3_flat = tf.reshape(h_conv3, [-1, 993 * 128])  # 展开，第一个参数为样本数量，-1未知
    h_fc1 = tf.nn.relu(tf.matmul(h_conv3_flat, w_fc1) + b_fc1)

    # 第二层全连接层
    # the second fully-connected layer
    w_fc2 = weight_variable([2048, 4096])
    b_fc2 = bias_variable([4096])

    h_fc2 = tf.nn.relu(tf.matmul(h_fc1, w_fc2) + b_fc2)

    # dropout
    keep_prob = tf.placeholder("float")
    h_fc2_drop = tf.nn.dropout(h_fc2, keep_prob)

    # 输出层
    # output layer
    w_fc3 = weight_variable([4096, 2])
    b_fc3 = bias_variable([2])

    y_conv = tf.matmul(h_fc2_drop, w_fc3) + b_fc3
    y_predict = tf.nn.softmax(y_conv)

    #  训练和评估模型
    # training model
    # cross_entropy = -tf.reduce_sum(y_ * tf.log(tf.clip_by_value(y_conv, 1e-10, 1.0)))
    cross_entropy = tf.nn.softmax_cross_entropy_with_logits(labels=y_, logits=y_conv)
    loss = tf.reduce_mean(cross_entropy)
    train_step = tf.train.AdamOptimizer(learning_rate=0.0001).minimize(loss)

    # visualization
    tf.summary.scalar("loss", loss)

    # tf.arg_max 返回某个tensor在某一维上最大值的索引
    # calculate the accuracy
    correct_prediction = tf.equal(tf.argmax(y_predict, 1), tf.argmax(y_, 1))
    accuracy = tf.reduce_mean(tf.cast(correct_prediction, "float"))

    for band in BAND:
        print("band test %s" % band)
        resultfile = './CNN_betweensubs/CNN_Betweensubs_' + band + '.npz'
        # windows = BAND[band]

        subavedata = np.zeros((2, 20, 2, 128, 4000))
        for s in range(20):
            subave = average_insub(s, band)
            subavedata[0, s, :, :, :] = subave[0, :, :, :]
            subavedata[1, s, :, :, :] = subave[1, :, :, :]
        subavelabel = np.zeros((20, 2))
        subavelabel[:, 1] = 1

        correct_set = np.zeros((2, 100))
        for con in range(2):
            # conwindow = windows[con]
            # for w in conwindow:
            windowdata = subavedata[con, :, :, :, :]

            for n in range(REPEAT_TIME):
                testsub = random.sample(range(0, 20), 2)
                testdata = np.reshape(windowdata[testsub, :, :, :], [4, 128, 4000, 1])
                testsublabel = np.reshape(subavelabel[testsub, :], [4])
                testlabel = np.zeros((4, 2))
                testlabel[:, 1] = testsublabel
                testlabel[testlabel[:, 1] == 0, 0] = 1
                traindata = np.reshape(np.delete(windowdata, testsub, axis=0), [36, 128, 4000, 1])
                trainsublabel = np.reshape(np.delete(subavelabel, testsub, axis=0), [36])
                trainlabel = np.zeros((36, 2))
                trainlabel[:, 1] = trainsublabel
                trainlabel[trainlabel[:, 1] == 0, 0] = 1

                with tf.Session() as sess:
                    sess.run(tf.global_variables_initializer())

                    # train network
                    for i in range(MAX_ITERATION):
                        sess.run(train_step,
                                 feed_dict={x_eeg: traindata[:, :, :, :], y_: trainlabel[:, :],
                                            keep_prob: 0.5})

                    test_accuracy = accuracy.eval(
                        feed_dict={x_eeg: testdata[:, :, :, :], y_: testlabel[:, :], keep_prob: 1.0})

                print("Con %d Repeat %d Accuracy %f" % (con, (n + 1), test_accuracy))
                correct_set[con, n] = test_accuracy

                if n % 50 == 0:
                    np.save(resultfile, correct_set)

            mean = np.mean(correct_set[con, :])
            std = np.std(correct_set[con, :])
            print("Con %d Mean %f Std %f" % (con, mean, std))

        np.save(resultfile, correct_set)


if __name__ == '__main__':
    main()




