import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import csv
import statistics

def autolabel(rects):
    for rect in rects:
        height = rect.get_height()
        plt.text(rect.get_x()+rect.get_width()/2.-0.2, 1.03*height, '%0.1f' % (height))


def preProcess (df) :
    return df[ df['ReC']  == df['ReM']]

def preProcess1 (df) :
    return df#[ df['ReC'] == 1]

colnames = ["Entil", "S1", "S2", "ReC", "StateC", "TimeC", "TimeNFA", "ReM", "StateM", "TimeM"]
df1_ = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_1.csv', names=colnames, header=None)
df2_ = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_2.csv', names=colnames, header=None)
df3_ = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_3.csv', names=colnames, header=None)
df4_ = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_4.csv', names=colnames, header=None)
df5_ = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_5.csv', names=colnames, header=None)
df6_ = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_6.csv', names=colnames, header=None)
df7_ = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_7_cp.csv', names=colnames, header=None)
df8_ = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_8_cp.csv', names=colnames, header=None)

colnames103 = ["Entil", "S1", "S2", "ReC", "StateC", "TimeC", "ReM", "StateM", "TimeM"]
df103 = pd.read_csv(r'/Users/mac/Desktop/hg/EFFECTS/DataAnylase/data/result_height_103.csv', names=colnames103, header=None)

#always hold
df1 = preProcess1 (preProcess (df1_))
df2 = preProcess1 (preProcess (df2_))
df3 = preProcess1 (preProcess (df3_))
df4 = preProcess1 (preProcess (df4_))
df5 = preProcess1 (preProcess (df5_))
df6 = preProcess1 (preProcess (df6_))
df7 = preProcess1 (preProcess (df7_))
df8 = preProcess1 (preProcess (df8_))


a = "TimeC"
b = "TimeM"
c = "StateC"
d = "StateM"
e = "TimeNFA"

items = [len(df1),
            len(df2), len(df3),len(df4), len(df5),len(df6),len(df7),len(df8)]

print items

num_STATE = [(df1['S1'] + df1['S2']).max(),
            (df2['S1'] + df2['S2']).max(),
    (df3['S1'] + df3['S2']).max(),
    (df4['S1'] + df4['S2']).max(),
    (df5['S1'] + df5['S2']).max(),
    (df6['S1'] + df6['S2']).max(),
    (df7['S1'] + df7['S2']).max(),
    (df8['S1'] + df8['S2']).max()
             ]

num_STATEmin = [(df1['S1'] + df1['S2']).min(),
            (df2['S1'] + df2['S2']).min(),
    (df3['S1'] + df3['S2']).min(),
    (df4['S1'] + df4['S2']).min(),
    (df5['S1'] + df5['S2']).min(),
    (df6['S1'] + df6['S2']).min(),
    (df7['S1'] + df7['S2']).min(),
    (df8['S1'] + df8['S2']).min()]

name_list = ['1', '2', '3', '4', '5', '6', '7', '8']
num_listC = [df1[a].mean(),
             df2[a].mean(),
             df3[a].mean(),
             df4[a].mean(),
             df5[a].mean(),
             df6[a].mean(),
             df7[a].mean(),
             df8[a].mean()
             ]

num_listM = [df1[b].mean(),
             df2[b].mean(),
             df3[b].mean(),
             df4[b].mean(),
             df5[b].mean(),
             df6[b].mean(),
             df7[b].mean(),
             df8[b].mean()
             ]

num_listCS = [df1[c].mean(),
             df2[c].mean(),
             df3[c].mean(),
             df4[c].mean(),
             df5[c].mean(),
             df6[c].mean(),
             df7[c].mean(),
             df8[c].mean()
              ]

num_listMS = [df1[c].mean(),
             df2[d].mean(),
             df3[d].mean(),
             df4[d].mean(),
             df5[d].mean(),
             df6[d].mean(),
             df7[d].mean(),
             df8[d].mean()
              ]


num_listCNFA = [df1[e].mean(),
             df2[e].mean(),
             df3[e].mean(),
             df4[e].mean(),
             df5[e].mean(),
             df6[e].mean(),
             df7[e].mean(),
             df8[e].mean()
                ]



x = list(range(len(num_listC)))
total_width, n = 0.8, 2
width = total_width / n

'''
a =plt.bar(x, num_listC, width=width, label='ANTICHAIN', fc='c')
for i in range(len(x)):
    x[i] = x[i] + width
b= plt.bar(x, num_listM, width=width, label='ANTIMIROV', tick_label=name_list, fc='royalblue')
autolabel(a)
autolabel(b)
plt.legend()
plt.show()
'''




def compute_ratio (df) :
    i = 0
    valid = 0.0
    invalid = 0.0
    while i < len(df) :
        if (df['ReC'][i] ==df['ReM'][i] and df['ReC'][i] == 1) : valid = valid + 1.0
        else : invalid = invalid + 1.0
        i = i +1
    return float (valid/(valid + invalid)) * 100


ratio  = [
compute_ratio(df1_),
compute_ratio(df2_),
compute_ratio(df3_),
compute_ratio(df4_),
compute_ratio(df5_),
compute_ratio(df6_),
compute_ratio(df7_),
compute_ratio(df8_)

          ]

print ratio


i = 0
while i < 8:
    print("%d & %d $\\sim$ %d & %.3f (%.2f+%.2f) & %.3f    \\\\ \\hline \n " % (i+1, num_STATEmin[i], num_STATE[i], (num_listC[i]+  num_listCNFA[i]),num_listCNFA[i], num_listC[i],   num_listM[i]))
    i = i + 1



listX = []
listM = []
listC = []

def addition (df) :
    i = 0
    while i < len(df) :
        if (df['ReM'][i] == df['ReC'][i]  and df['StateC'][i] < 175 and df['S1'][i] + df['S2'][i] < 100):
            listX.append(df['S1'][i] + df['S2'][i])
            listM.append(df['StateM'][i])
            listC.append(df['StateC'][i])
        i += 1


addition(df1_)
addition(df2_)
addition(df3_)
addition(df4_)
addition(df5_)
addition(df6_)
addition(df7_)
addition(df8_)
addition(df103)

plt.scatter(listX, listC,  s=70, label = '$Antichain$', c = 'tomato', marker='.', alpha = None, edgecolors= 'white')

plt.scatter(listX, listM,  s=70, label = '$T.r.s$', c = 'c', marker='.', alpha = None, edgecolors= 'white')



plt.legend()
plt.ylabel(u"Search Steps (No.Processed/Hypotheses).")
plt.xlabel(u"States(A) + States(B)")
plt.grid(linestyle="-", alpha=0.1)
plt.savefig('/Users/mac/Desktop/hg/CAV2020/plots/Explore_State.png')
plt.show()
