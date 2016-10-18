import csv
import time
import os

columns = {"fid", "ref", "sttp", "uri", "ts", "ip", "ancestors", "mid", "ti", "ua", "uuid"}


os.chdir('..')
starttime = time.time()
print('starttime: %d' %starttime)
with open('Desktop/data/top500.txt','r') as fin:

    reader = fin.readlines()

    with open('data1.csv','w') as output:
        dict_writer = csv.DictWriter(output, columns, extrasaction='ignore')
        dict_writer.writeheader()
        for item in reader:
            item = item.replace('null', 'None')
            newitem = eval(item)
            if 'uri' in newitem:
                newitem['uri'] = 'https://www.skroutz.gr' + newitem['uri']
            # print(newitem)
            dict_writer.writerow(newitem)

endtime = time.time()
print('endtime: %d' %endtime)
print('time used: %d' %(endtime-starttime))