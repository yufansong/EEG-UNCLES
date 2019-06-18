import numpy as np
from scipy.signal import butter, lfilter
from scipy.io import savemat
import h5py

BAND = 'highgamma'
SUBLIST = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]


def butterworth_pass(lowcut, highcut, fs, order=5):
    nyq = 0.5 * fs
    low = lowcut / nyq
    high = highcut / nyq
    b, a = butter(order, [low, high], btype='bandpass')
    return b, a


def butterworth_bandpass_filter(data, lowcut, highcut, fs, order=5):
    b, a = butterworth_pass(lowcut, highcut, fs, order=order)
    y = lfilter(b, a, data)
    return y


def main():
    if BAND == 'theta':
        lowcut = 4
        highcut = 7
    elif BAND == 'alpha':
        lowcut = 8
        highcut = 12
    elif BAND == 'lowbeta':
        lowcut = 14
        highcut = 20
    elif BAND == 'highbeta':
        lowcut = 20
        highcut = 30
    elif BAND == 'lowgamma':
        lowcut = 30
        highcut = 60
    elif BAND == 'highgamma':
        lowcut = 60
        highcut = 100
    else:
        print("Band invalid!")
        return

    for s in SUBLIST:
        rawfile = '../rawdata/CLAS_VP' + str(s).zfill(2) + '_onedata_STBFH_MNT.mat'
        filterfile = '../bandpassdata/' + BAND + '/CLAS_VP' + str(s).zfill(2) + '_' + BAND + '.mat'

        rawdata = h5py.File(rawfile, 'r')
        raw = rawdata['onedata'][:]
        label = rawdata['allevents'][:]
        glabel = rawdata['allgoodevents'][:]
        raw = np.transpose(raw)
        label = np.transpose(label)
        glabel = np.transpose(glabel)
        srate = 1000

        filterdata = np.zeros([2, 128, 4000, 80])
        for con in range(2):
            for chan in range(128):
                for trial in range(80):
                    data = raw[con, chan, :, trial]
                    filter = butterworth_bandpass_filter(data, lowcut, highcut, srate, order=4)
                    filterdata[con, chan, :, trial] = filter

        savemat(filterfile, {'onefilter': filterdata, 'label': label, 'glabel': glabel})


if __name__ == '__main__':
    main()
