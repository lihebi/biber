#!/usr/bin/env python3

import os
import hashlib
import requests
import re
import time

def hash_string(s):
    m = hashlib.sha256()
    m.update(s.encode('utf8'))
    # m.update(b" the spammish repetition")
    return m.hexdigest()

def gen_hash_filename(link):
    # FIXME use system tmp dir
    return 'tmp/' + hash_string(link)

def download_to_hash(url):
    """Download url to tmp/HASHNAME. Return the local hash file.
    """
    f = gen_hash_filename(url)
    if not os.path.exists(f):
        download_file(url, f)
    return f
    

def download_file(url, filename):
    """Download url into file.

    All the internet connection should go through this function, and
    this funciton contain rate limit. It maintain a count variable. If
    the count variable reaches 5, it sleep for 5 seconds.

    """
    if not os.path.exists(filename):
        if not os.path.exists(os.path.dirname(filename)):
            os.makedirs(os.path.dirname(filename))
        download_file.count += 1
        # sleep every 5 downloads
        if download_file.count == 5:
            print('-- sleeping for 10 sec')
            download_file.count = 0
            time.sleep(10)
        print('-- downloading ' + url + ' into ' + filename + ' ..')
        r = requests.get(url)
        with open(filename, 'wb') as f:
            f.write(r.content)
download_file.count = 0
            
##############################
## Utility functions
##############################

stop_words = set(["you", "the", "and", "can", "where", "when", "how",
                  "what", "who", "why", "does", "for", "are", "don",
                  "from"])


def title_first_word(title):
    lst = re.split(r'[\s:{}\-?()/,\'*"#$+“”’]', title.lower())
    lst = list(filter(lambda s: len(s) > 2, lst))
    lst = list(filter(lambda s: s not in stop_words, lst))
    return lst[0].title() if lst else 'Title'


def gen_id(year, conf, authors, title):
    return '-'.join([str(year), str(conf),
                     authors[0].split(' ')[-1], title_first_word(title)])

def authors_str2lst(str):
    res = list(map(lambda s: s.strip(), re.split(r',|;|\*| and ', str)))
    return list(filter(lambda s: s, res))

def clean_string(str):
    return re.sub(r'\s+', ' ', str).strip()

def gen_single_bib(id, title, author, pdflink, year, booktitle):
    return ('@inproceedings{' + id + ",\n"
            + "  title={" + clean_string(title) + "},\n"
            + "  author={" + clean_string(author) + "},\n"
            + "  year={" + str(year) + "},\n"
            + "  booktitle={" + booktitle + "},\n"
            + "  pdflink={" + pdflink + "}\n}\n")


def path2year(path):
    return re.search(r'/\w+-(\d{4})', path)[1]

def path2conf(filename):
    return re.search('/(\w+)-\d{4}', filename)[1]
