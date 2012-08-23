# Run from root of repo e.g.
#   python benchmarks/bench.py
#
# Sample CSV taken from:
#
# presidents.csv: See People Software
# (http://seepeoplesoftware.com/downloads/older-versions/11-sample-csv-file-of-us-presidents.html)

import csv
import cStringIO
import time

def decode(data):
    r = csv.reader(data.splitlines())
    res = []
    for (presidency, president, wikipedia_entry, took_office, left_office,
         party, home_state) in r:
        res.append((int(presidency), unicode(president, 'utf-8'),
                    wikipedia_entry, took_office, left_office,
                    unicode(party, 'utf-8'), unicode(home_state, 'utf-8')))
    return res

def encode(data):
    buf = cStringIO.StringIO()
    r = csv.writer(buf)
    for (presidency, president, wikipedia_entry, took_office, left_office,
         party, home_state) in data:
        # str is automatically called
        r.writerow([presidency, president.encode('utf-8'),
                    wikipedia_entry, took_office, left_office,
                    party.encode('utf-8'), home_state.encode('utf-8')])
    res = buf.getvalue()
    buf.close()
    return res

def decode_named(data):
    r = csv.DictReader(data.splitlines())
    res = []
    for d in r:
        res.append((int(d['Presidency']), unicode(d['President'], 'utf-8'),
                    d['Wikipedia Entry'], d['Took office'], d['Left office'],
                    unicode(d['Party'], 'utf-8'),
                    unicode(d['Home State'], 'utf-8')))
    return res

def encode_named(data):
    buf = cStringIO.StringIO()
    r = csv.DictWriter(buf, ['Presidency', 'President', 'Wikipedia Entry',
                             'Took office', 'Left office', 'Party',
                             'Home State'])
    for d in data:
        # This does less work than the Haskell code as unicode fields
        # are not utf-8 encoded but rather ascii encoded since string
        # is called (unless someone change the locale)
        r.writerow(d)
    res = buf.getvalue()
    buf.close()
    return res

def as_typed_dicts(data):
    type_map = {'Presidency': int, 'President': unicode, 'Wikipedia Entry': str,
                'Took office': str, 'Left office': str, 'Party': unicode,
                'Home State': unicode}
    r = csv.DictReader(data.splitlines())
    res = []
    for d in r:
        for k in d:
            typ = type_map[k]
            if typ == unicode:
                d[k] = unicode(d[k], 'utf-8')
            else:
                d[k] = typ(d[k])
        res.append(d)
    return res

csv_file = open('benchmarks/presidents.csv', 'rb')
csv_data = csv_file.read()
csv_file.close()

presidents = decode(csv_data)

csv_file_named = open('benchmarks/presidents_with_header.csv', 'rb')
csv_data_named = csv_file_named.read()
csv_file_named.close()

presidents_named = as_typed_dicts(csv_data_named)

iters = 10000

# Positional

print 'positional'

print '  decode'

print '    without conversion:',
start = time.time()
for i in xrange(iters):
    list(csv.reader(csv_data.splitlines()))
print "%f us" % (1000000 * (time.time() - start) / iters)

print '    with conversion:',
start = time.time()
for i in xrange(iters):
    decode(csv_data)
print "%f us" % (1000000 * (time.time() - start) / iters)

print '  encode'

print '    with conversion:',
start = time.time()
for i in xrange(iters):
    encode(presidents)
print "%f us" % (1000000 * (time.time() - start) / iters)

# Named

print 'named'

print '  decode'

print '    without conversion:',
start = time.time()
for i in xrange(iters):
    list(csv.DictReader(csv_data.splitlines()))
print "%f us" % (1000000 * (time.time() - start) / iters)

print '    with conversion:',
start = time.time()
for i in xrange(iters):
    decode_named(csv_data_named)
print "%f us" % (1000000 * (time.time() - start) / iters)

print '  encode'

print '    with conversion:',
start = time.time()
for i in xrange(iters):
    encode_named(presidents_named)
print "%f us" % (1000000 * (time.time() - start) / iters)
