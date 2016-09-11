#!/usr/bin/env python

from mnist import MNIST
mndata = MNIST('./data')
train_imgs, train_labels = mndata.load_training()
test_imgs, test_labels = mndata.load_testing()

import cPickle, numpy
f = open('mnist.pkl', 'w')
cPickle.dump((train_imgs, train_labels, test_imgs, test_labels), f)
f.close()
