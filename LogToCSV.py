
import csv

columns = {"cid","fid", "pth", "ref", "sttp", "uri", "uuid", "us", "ts", "ip", "r", "cuuid", "ae", "ancestors", "from", "mid", "mnis", "sid", "ti", "path", "action", "cnt", "dewey", "drop", "cf", "lmap", "mnid", "pid", "ua"}


with open('Data/test.txt','r') as fin:

    # reader = csv.reader(fin)
    reader = fin.readlines()

    with open('testout.csv,','w') as output:
        dict_writer = csv.DictWriter(output, columns, extrasaction='ignore')
        dict_writer.writeheader()
        for item in reader:
            print(eval(item))
            print('\n')
            dict_writer.writerow(eval(item))

