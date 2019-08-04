#!/usr/bin/env python3

# ECCV

# https://link.springer.com/conference/eccv

# 1. parse this index link, get all the links for each year, without workshop.
# 2. for each year, get all volumes (together with this one)
# 3. for each volume, parse the papers with pagination

from bs4 import BeautifulSoup
import os
import requests
import re
import time
import math

from utils import download_to_hash, download_file, gen_id, gen_single_bib, authors_str2lst

def normalize_year(year):
    """Year is a 2-digit or 4-digit string. Return 4-digit year of
    type(int).

    """
    if len(year) == 4:
        return int(year)
    if len(year) == 2:
        if year[0] in {'0', '1', '2'}:
            return int('20' + year)
        else:
            return int('19' + year)

def springer_parse_index(url):
    """Return titles, links
    """
    html_file = download_to_hash(url)
    soup = BeautifulSoup(open(html_file), 'lxml')
    titles = []
    links = []
    for a in soup.select('.c-card a'):
        link = a['href']
        if not link.startswith('http'):
            link = 'https://link.springer.com/' + link
        title = a.get_text().strip()
        titles.append(title)
        links.append(link)
    return titles, links

def springer_get_volumes(link):
    """Return [volume_link], including this one.
    """
    html_file = download_to_hash(link)
    soup = BeautifulSoup(open(html_file), 'lxml')
    res = [link]
    for a in soup.select('.other-volumes-container li a'):
        href = a['href']
        if not href.startswith('http'):
            href = 'https://link.springer.com/' + href
        res.append(href)
    return res

def springer_get_pagination(link):
    """Return a list of links for each pagination
    """
    html_file = download_to_hash(link)
    soup = BeautifulSoup(open(html_file), 'lxml')
    if soup.select('.test-maxpagenum'):
        pagenum = int(soup.select('.test-maxpagenum')[0].get_text())
    else:
        pagenum = 1
    res = []
    for p in range(pagenum):
        res.append(link + '?page=' + str(p+1))
    return res
    
def springer_bib_with_pagination(year, conf, link):
    """
    """
    # '.test-maxpagenum'
    pages = springer_get_pagination(link)
    res = ''
    for page in pages:
        res += springer_bib(year, conf, page)
    return res

def springer_bib(year, conf, link):
    """Return bib for this page only.
    """
    html_file = download_to_hash(link)
    soup = BeautifulSoup(open(html_file), 'lxml')
    res = ''
    for paper in soup.select('.chapter-item'):
        meta = paper.select('.content-type-list__meta')[0]
        title = meta.select('div')[0].get_text()
        authors_str = meta.select('div')[1].get_text()
        authors = authors_str2lst(authors_str)
        pdflink_a = paper.select('a.test-book-toc-download-link')
        pdflink = ''
        # some conference may not have a pdflink, e.g.
        # https://link.springer.com//book/10.1007/BFb0015518
        if pdflink_a:
            pdflink = pdflink_a[0]['href']
        if not pdflink.startswith('http'):
            pdflink = 'https://link.springer.com/' + pdflink
        id = gen_id(year, conf, authors, title)
        bib = gen_single_bib(id, title, ' and '.join(authors), pdflink, year, conf)
        res += bib
    return res
    

class eccv():
    def __init__(self):
        self.name = 'ECCV'
        self.index = {}
        index_link = 'https://link.springer.com/conference/eccv'
        titles, links = springer_parse_index(index_link)
        for title, link in zip(titles, links):
            if 'ECCV' in title and 'Workshop' not in title:
                year = re.findall(r"ECCV\s*'?(\d+)", title)
                if year:
                    year = normalize_year(year[0])
                    self.index[year] = link
    def years(self):
        return self.index.keys()
    def bib(self, year):
        res = ''
        if year in self.index:
            link = self.index[year]
            volumes = springer_get_volumes(link)
            for volume in volumes:
                res += springer_bib_with_pagination(year, self.name, volume)
        return res

if __name__ == '__test__':
    e = eccv()
    bib = e.bib(2018)
    e.index[2018]
    springer_get_volumes(e.index[2018])
