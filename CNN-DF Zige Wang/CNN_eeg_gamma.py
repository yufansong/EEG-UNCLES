__author__ = 'Wang Zige'

import tensorflow as tf
import numpy as np
import h5py
import random
from sklearn import preprocessing

# TEST_SAMPLE = 500
# TEST_SUB1 = 10
# TEST_SUB2 = 20
# VAL_SAMPLE = 1000
MAX_ITERATION = 100
REPEAT_TIME = 100
EARLY_STOP = 0
EARLY_STOP_PARAM = 10
CONDITION = 1
SUB_LIST = [3, 4, 5, 12, 15, 17, 19]


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


def main():
    # The input matrix(128*180)
    x = tf.placeholder(tf.float32, [None, 128*36])

    # y_ is the true label in the form of one-hot vector
    y_ = tf.placeholder(tf.float32, [None, 2])

    # 图像转化为一个四维张量，第一个参数代表样本数量，-1表示不定，第二三参数代表二位信号
    # reshape the matrix into a four dimensional matrix, 1st parameter is the number of samples, -1 means uncertain
    x_eeg = tf.reshape(x, [-1, 36, 128, 1])

    # 第一层卷积加池化
    # the first convolution + pooling
    w_conv1 = weight_variable([1, 128, 1, 96])  # 第一二参数值得卷积核尺寸大小，即patch，第三个参数是图像通道数，第四个参数是卷积核的数目，代表会出现多少个卷积特征
    b_conv1 = bias_variable([96])

    h_conv1 = tf.nn.relu(conv2d(x_eeg, w_conv1) + b_conv1)
    h_pool1 = max_pool_3x1(h_conv1)

    # 第二层卷积加池化
    # the second convolution + pooling
    w_conv2 = weight_variable([3, 1, 96, 128])  # 多通道卷积，卷积出128个特征
    b_conv2 = bias_variable([128])

    h_conv2 = tf.nn.relu(conv2d(h_pool1, w_conv2) + b_conv2)
    h_pool2 = max_pool_3x1(h_conv2)

    # 第三层卷积加池化
    # the third convolution + pooling
    w_conv3 = weight_variable([3, 1, 128, 128])  # 多通道卷积，卷积出128个特征
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
    w_fc1 = weight_variable([6 * 128, 2048])
    b_fc1 = bias_variable([2048])

    h_conv3_flat = tf.reshape(h_conv3, [-1, 6 * 128])  # 展开，第一个参数为样本数量，-1未知
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

    data = h5py.File('../CNN_Dataset_180_con_gamma.mat')
    eeg = data['CNNgamma_con' + str(CONDITION)][:]
    num_data = len(eeg)
    label = data['CNNlabel_con' + str(CONDITION)][:]
    onsetnum = data['Onsetnumfilter'][:]

    # normalization and reshape
    max_min_scaler = preprocessing.MinMaxScaler()
    for i in range(num_data):
        eeg[i, :, :] = max_min_scaler.fit_transform(eeg[i, :, :])
    eeg = np.reshape(eeg, [num_data, 36, 128, 1])

    '''
    # generate test slice(leave two out)
    # test_slice = random.sample(range(0, num_train), TEST_SAMPLE)
    test_num1 = int(onsetnum[TEST_SUB1 - 1][CONDITION - 1])
    test_num2 = int(onsetnum[TEST_SUB2 - 1][CONDITION - 1])
    test_slice1 = np.arange(test_num1).astype('uint32')
    test_slice2 = np.arange(test_num2).astype('uint32')
    test_begin1 = np.sum(onsetnum[:(TEST_SUB1 - 1), CONDITION - 1]).astype('uint32')
    test_begin2 = np.sum(onsetnum[:(TEST_SUB2 - 1), CONDITION - 1]).astype('uint32')
    test_slice1 += test_begin1
    test_slice2 += test_begin2
    test_slice = np.hstack((test_slice1, test_slice2))
    test_set = eeg[test_slice, :, :, :]
    test_label = label[test_slice, :]
    train_set = np.delete(eeg, test_slice, axis=0)
    train_label = np.delete(label, test_slice, axis=0)

    # generate validate slice
    val_slice = random.sample(range(0, num_train - test_num1 - test_num2), VAL_SAMPLE)
    val_set = train_set[val_slice, :, :, :]
    val_label = train_label[val_slice, :]
    '''

    subject_begin = np.zeros((20,), dtype='uint32')
    start_point = 0
    for s in range(20):
        subject_num = int(onsetnum[s][CONDITION - 1])
        subject_begin[s] = start_point
        start_point += subject_num
    # correct_set = np.load("experiments_insub/CNN/CNN_1000_INSUB_CON" + str(CONDITION) + ".npy")
    correct_set = np.zeros((20, 100))

    for s in SUB_LIST:
        subject_num = int(onsetnum[s - 1][CONDITION - 1])
        start_point = subject_begin[s - 1]
        num_train = int(subject_num * 0.8)
        num_test = subject_num - num_train
        subject_set = eeg[start_point:(start_point + subject_num), :, :, :]
        subject_label = label[start_point:(start_point + subject_num), :]

        for n in range(REPEAT_TIME):
            test_slice = random.sample(range(0, subject_num), num_test)
            test_set = subject_set[test_slice, :, :, :]
            test_label = subject_label[test_slice, :]
            train_set = np.delete(subject_set, test_slice, axis=0)
            train_label = np.delete(subject_label, test_slice, axis=0)

            if EARLY_STOP:
                val_slice = random.sample(range(0, num_train), num_test)
                val_set = train_set[val_slice, :, :, :]
                val_label = train_label[val_slice, :]
                train_wo_val_set = np.delete(train_set, val_slice, axis=0)
                train_wo_val_label = np.delete(train_label, val_slice, axis=0)

                saver = tf.train.Saver()
                save_file = "experiments_gamma/CNN/models/gamma_con" + str(CONDITION) + ".ckpt"
            with tf.Session() as sess:
                sess.run(tf.global_variables_initializer())
                # writer = tf.summary.FileWriter('log_cnn_con' + str(CONDITION) + '/', sess.graph)
                # merged_sum = tf.summary.merge_all()
                # saver.restore(sess, "models_con_4n/cnn_eeg_iter_con1_100.ckpt")

                # train network
                best_val_loss = 1000000.0
                best_val_step = 0
                for i in range(MAX_ITERATION):
                    # if (i + 1) % 10 == 0:
                    #    summary = sess.run(merged_sum, feed_dict={x_eeg: train_set[:, :, :, :], y_: train_label[:, :], keep_prob: 1.0})
                    #    writer.add_summary(summary, i + 1)
                    # validate
                    if EARLY_STOP:
                        sess.run(train_step,
                                 feed_dict={x_eeg: train_wo_val_set[:, :, :, :], y_: train_wo_val_label[:, :],
                                            keep_prob: 0.5})
                        val_loss = loss.eval(
                            feed_dict={x_eeg: val_set[:, :, :, :], y_: val_label[:, :], keep_prob: 1.0})
                        # print("step %d, validation loss %g" % (i + 1, val_loss))
                        if val_loss < best_val_loss:
                            best_val_loss = val_loss
                            best_val_step = i
                            saver.save(sess, save_file)
                        elif (i - best_val_step) >= EARLY_STOP_PARAM:
                            print("Early stop at step %d, best validation loss %f" % (i + 1, best_val_loss))
                            break
                    else:
                        sess.run(train_step,
                                 feed_dict={x_eeg: train_set[:, :, :, :], y_: train_label[:, :],
                                            keep_prob: 0.5})
                    # if (i + 1) % 100 == 0:
                    #     save_file = "models_con_3n/cnn_eeg_iter_con" + str(CONDITION) + "_" + str(i + 1) + ".ckpt"
                    #     saver_path = saver.save(sess, save_file)
                    #     print("model saved in " + saver_path)
                if EARLY_STOP:
                    saver.restore(sess, save_file)
                test_accuracy = accuracy.eval(
                    feed_dict={x_eeg: test_set[:, :, :, :], y_: test_label[:, :], keep_prob: 1.0})
                print("Sub %d Con %d Repeat %d: Accuracy on test data: %g" % (s, CONDITION, (n + 1), test_accuracy))
                correct_set[s - 1, n] = test_accuracy

            if n % 20 == 0:
                np.save("experiments_gamma/CNN/CNN_100_GAMMA_CON" + str(CONDITION) + ".npy", correct_set)

    np.save("experiments_gamma/CNN/CNN_100_GAMMA_CON" + str(CONDITION) + ".npy", correct_set)


if __name__ == '__main__':
    main()




