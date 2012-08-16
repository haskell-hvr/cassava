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

def bench_named(data):
    r = csv.DictReader(data.splitlines())
    res = []
    for d in r:
        res.append((int(d['Presidency']), unicode(d['President'], 'utf-8'),
                    d['Wikipedia Entry'], d['Took office'], d['Left office'],
                    unicode(d['Party']), unicode(d['Home State'])))
    return res

csv_file = open('benchmarks/presidents.csv', 'rb')
csv_data = csv_file.read()
csv_file.close()

csv_file_named = open('benchmarks/presidents_with_header.csv', 'rb')
csv_data_named = csv_file_named.read()
csv_file_named.close()

iters = 10000

print 'indexed'

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

print 'named'

print 'Without type conversions:'
start = time.time()
for i in xrange(iters):
    list(csv.DictReader(csv_data.splitlines()))
print "%f us" % (1000000 * (time.time() - start) / iters)

print 'Including type conversions:'
start = time.time()
for i in xrange(iters):
    bench_named(csv_data_named)
print "%f us" % (1000000 * (time.time() - start) / iters)
