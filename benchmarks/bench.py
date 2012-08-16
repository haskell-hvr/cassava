# Run from root of repo e.g.
#   python benchmarks/bench.py
#
# Sample CSV taken from:
#
# presidents.csv: See People Software
# (http://seepeoplesoftware.com/downloads/older-versions/11-sample-csv-file-of-us-presidents.html)

import csv
import time

def bench(data):
    r = csv.reader(data.splitlines())
    res = []
    for (presidency, president, wikipedia_entry, took_office, left_office, party, home_state) in r:
        res.append((int(presidency), unicode(president, 'utf-8'),
                    wikipedia_entry, took_office, left_office, unicode(party),
                    unicode(home_state)))
    return res

csv_file = open('benchmarks/presidents.csv', 'rb')
csv_data = csv_file.read()
csv_file.close()

iters = 10000

print 'Without type conversions:'
start = time.time()
for i in xrange(iters):
    list(csv.reader(csv_data.splitlines()))
print "%f us" % (1000000 * (time.time() - start) / iters)

print 'Including type conversions:'
start = time.time()
for i in xrange(iters):
    bench(csv_data)
print "%f us" % (1000000 * (time.time() - start) / iters)
