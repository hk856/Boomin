import csv
import time
import os

columns = {"cid","fid", "pth", "ref", "sttp", "uri", "uuid", "us", "ts", "ip", "r", "cuuid", "ae", "ancestors", "from", "mid", "mnis", "sid", "ti", "path", "action", "cnt", "dewey", "drop", "cf", "lmap", "mnid", "pid", "ua"}


os.chdir('..')
starttime = time.time()
print('starttime: %d' %starttime)
with open('Data/scratch_2016_06_17_kavula.log.txt','r') as fin:

    # reader = csv.reader(fin)
    reader = fin.readlines()

    with open('testout.csv','w') as output:
        dict_writer = csv.DictWriter(output, columns, extrasaction='ignore')
        dict_writer.writeheader()
        for item in reader:
            item = item.replace('null', 'None')
            # print(item)
            # print(eval(item))
            # print('\n')
            dict_writer.writerow(eval(item))

endtime = time.time()
print('endtime: %d' %endtime)
print('time used: %d' %(endtime-starttime))