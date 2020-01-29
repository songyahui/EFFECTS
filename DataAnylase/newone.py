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




listX = []
listM = []
listC = []
listMS = []
listCS = []
listCNFA = []

def addition (df) :
    i = 0
    while i < len(df) :
        if (df['ReM'][i] == df['ReC'][i]  and df['StateC'][i] < 175 and df['S1'][i] + df['S2'][i] < 120):
            listX.append(df['S1'][i] + df['S2'][i])
            listM.append(df['TimeM'][i])
            listC.append(df['TimeC'][i])
            listMS.append(df['StateM'][i])
            listCS.append(df['StateC'][i])
            listCNFA.append(df['TimeNFA'][i])
        i += 1


addition(df1_)
addition(df2_)
addition(df3_)
addition(df4_)
addition(df5_)
addition(df6_)
addition(df7_)
addition(df8_)
#addition(df103)

temp = [[],[],[],[],[],[],[],[]]

_listX = temp
_listM = temp
_listC = temp
_listMS = temp
_listCS = temp
_listCNFA = temp

i = 0
while i < len(listX) :
    if listX[i]< 30:
        index = 0
        _listX[index].append(listX[i])
        _listM[index].append(listM[i])
        _listC[index].append(listC[i])
        _listMS[index].append(listMS[i])
        _listCS[index].append(listCS[i])
        _listCNFA[index].append(listCNFA[i])
    elif listX[i]< 60:
        index = 1
        _listX[index].append(listX[i])
        _listM[index].append(listM[i])
        _listC[index].append(listC[i])
        _listMS[index].append(listMS[i])
        _listCS[index].append(listCS[i])
        _listCNFA[index].append(listCNFA[i])
    elif listX[i]< 90:
        index = 2
        _listX[index].append(listX[i])
        _listM[index].append(listM[i])
        _listC[index].append(listC[i])
        _listMS[index].append(listMS[i])
        _listCS[index].append(listCS[i])
        _listCNFA[index].append(listCNFA[i])
    elif listX[i]< 120:
        index = 3
        _listX[index].append(listX[i])
        _listM[index].append(listM[i])
        _listC[index].append(listC[i])
        _listMS[index].append(listMS[i])
        _listCS[index].append(listCS[i])
        _listCNFA[index].append(listCNFA[i])
    elif listX[i]< 150:
        index = 4
        _listX[index].append(listX[i])
        _listM[index].append(listM[i])
        _listC[index].append(listC[i])
        _listMS[index].append(listMS[i])
        _listCS[index].append(listCS[i])
        _listCNFA[index].append(listCNFA[i])
    elif listX[i]< 180:
        index = 5
        _listX[index].append(listX[i])
        _listM[index].append(listM[i])
        _listC[index].append(listC[i])
        _listMS[index].append(listMS[i])
        _listCS[index].append(listCS[i])
        _listCNFA[index].append(listCNFA[i])
    elif listX[i]< 210:
        index = 6
        _listX[index].append(listX[i])
        _listM[index].append(listM[i])
        _listC[index].append(listC[i])
        _listMS[index].append(listMS[i])
        _listCS[index].append(listCS[i])
        _listCNFA[index].append(listCNFA[i])
    else:
        index = 7
        _listX[index].append(listX[i])
        _listM[index].append(listM[i])
        _listC[index].append(listC[i])
        _listMS[index].append(listMS[i])
        _listCS[index].append(listCS[i])
        _listCNFA[index].append(listCNFA[i])
    i =  i + 1


i = 0
while i < len(_listX):
    print  len(_listX)
    a = statistics.mean(map(float,_listC[i]))
    b = statistics.mean(map(float,_listCNFA[i]))
    c = b - a
    d = statistics.mean(map(float,_listM[i]))

    print("%d $\\sim$ %d & %f     \\\\ \\hline \n " % (i, (i+1)*30,a ))
    i = i + 1




