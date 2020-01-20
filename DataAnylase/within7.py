import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import csv
import statistics

colnames = ["Entil", "S1", "S2", "ReC", "StateC", "TimeC", "ReM", "StateM", "TimeM"]
df = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_100.csv', names=colnames, header=None)

#always hold

listX = []
listM = []
listC = []
i = 0
while i < 6400:
    if (df['ReC'][i] == 0 and df['ReM'][i] == df['ReC'][i] and df['S1'][i] + df['S2'][i] > 40 and df['S1'][i] + df['S2'][i] <60):

        listM.append(df['TimeM'][i])
        listC.append(df['TimeC'][i])
    i += 1

print statistics.median(listM)
print statistics.median(listC)

plt.scatter(listX, listM,  s=50, label = '$Antimirov$', c = 'red', marker='.', alpha = None, edgecolors= 'white')

plt.scatter(listX, listC,  s=50, label = '$Antichain$', c = 'blue', marker='.', alpha = None, edgecolors= 'white')

plt.legend()
plt.ylabel(u"TIME.")
plt.xlabel(u"State(A) + State(B)")
plt.savefig('/Users/mac/Desktop/hg/CAV2020/plots/ALWAYSHOLD.png')
plt.show()

