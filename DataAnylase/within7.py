import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import csv
import statistics

def autolabel(rects):
    for rect in rects:
        height = rect.get_height()
        plt.text(rect.get_x()+rect.get_width()/2.-0.2, 1.03*height, '%0.1f' % (height))



colnames = ["Entil", "S1", "S2", "ReC", "StateC", "TimeC", "ReM", "StateM", "TimeM"]
df1 = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_1.csv', names=colnames, header=None)
df2 = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_2.csv', names=colnames, header=None)
df3 = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_3.csv', names=colnames, header=None)
df4 = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_4.csv', names=colnames, header=None)
df5 = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_5.csv', names=colnames, header=None)
df6 = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_6.csv', names=colnames, header=None)
df7 = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_7.csv', names=colnames, header=None)
#always hold



a = "TimeC"
b = "TimeM"

name_list = ['1', '2', '3', '4', '5', '6', '7']
num_listC = [df1[a].median(),
             df2[a].median(),
             df3[a].median(),
             df4[a].median(),
             df5[a].median(),
             df6[a].median(),
             df7[a].median()]

num_listM = [df1[b].median(),
             df2[b].median(),
             df3[b].median(),
             df4[b].median(),
             df5[b].median(),
             df6[b].median(),
             df7[b].median()]

print num_listC
print num_listM

x = list(range(len(num_listC)))
total_width, n = 0.8, 2
width = total_width / n


a =plt.bar(x, num_listC, width=width, label='ANTICHAIN', fc='c')
for i in range(len(x)):
    x[i] = x[i] + width
b= plt.bar(x, num_listM, width=width, label='ANTIMIROV', tick_label=name_list, fc='royalblue')
autolabel(a)
autolabel(b)
plt.legend()
plt.savefig('/Users/mac/Desktop/hg/CAV2020/plots/height-state.png')

plt.show()
