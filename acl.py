#!/usr/bin/env python3

from utils import download_to_hash, download_file, gen_id, gen_single_bib
from bs4 import BeautifulSoup
import os

# https://aclanthology.info/
# https://aclanthology.info/events/acl-1997

def acl_conference_bib(year, conf, link):
    # link = 'https://aclanthology.info/events/acl-2018'
    # year = 2018
    # conf = 'ACL'
    html_file = download_to_hash(link)
    soup = BeautifulSoup(open(html_file), 'lxml')
    res = ''
    # soup.select('#content p')[3].select('a[href^=/people]')
    
    # len(soup.select('#content p'))
    for p in soup.select('#content p'):
        strong = p.strong
        title = strong.a.get_text()
        authors = [a.get_text() for a in p.select('a[href^=/people]')]
        if authors:
            pdflink = p.a['href']
            id = gen_id(year, conf, authors, title)
            bib = gen_single_bib(id, title, ' and '.join(authors), pdflink, year, conf)
            res += bib
    return res

def acl_bib(year):
    link = 'https://aclanthology.info/events/acl-' + str(year)
    return acl_conference_bib(year, 'ACL', link)

def cl_bib(year):
    link = 'https://aclanthology.info/events/cl-' + str(year)
    return acl_conference_bib(year, 'CL', link)

def naacl_bib(year):
    link = 'https://aclanthology.info/events/naacl-' + str(year)
    return acl_conference_bib(year, 'NAACL', link)

def eacl_bib(year):
    link = 'https://aclanthology.info/events/eacl-' + str(year)
    return acl_conference_bib(year, 'EACL', link)

def emnlp_bib(year):
    link = 'https://aclanthology.info/events/emnlp-' + str(year)
    return acl_conference_bib(year, 'EMNLP', link)


if __name__ == '__hebi__':
    bib = acl_conference_bib(2018, 'ACL', 'https://aclanthology.info/events/acl-2018')
