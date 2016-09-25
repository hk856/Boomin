import csv
import time
import os

columns = {"fid", "ref", "sttp", "uri", "ts", "ip", "ancestors", "mid", "ti", "ua"}


os.chdir('..')
starttime = time.time()
print('starttime: %d' %starttime)
with open('desktop/data/data.log','r') as fin:

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