#!/usr/bin/env python3

from bs4 import BeautifulSoup
import os
import requests
import re
import time
import math

from utils import download_file, gen_id, gen_single_bib, download_to_hash, authors_str2lst

def iclr_bib_2014():
    link = 'https://iclr.cc/archive/2014/conference-proceedings/'
    year = 2014
    conf = 'ICLR'
    html_file = download_to_hash(link)
    soup = BeautifulSoup(open(html_file), 'lxml')
    # all blocks
    ps = soup.select('#sites-canvas-main-content p')
    # remove the empty blocks
    ps = [p for p in ps if p.get_text().strip()]
    # remove the conference title: Listed below are the conference papers ..
    ps = ps[1:]
    # titles
    ptitles = [p for p in ps if p.find('a')]
    # authors
    pauthors = [p for p in ps if not p.find('a')]
    [p.get_text().strip() for p in pauthors]
    [p.get_text().strip() for p in ptitles]
    res = ''
    for ptitle,pauthor in zip(ptitles, pauthors):
        title = ptitle.get_text().strip()
        authors_str = pauthor.get_text().strip()
        authors = authors_str2lst(authors_str)
        # actually arxiv
        pdflink = ptitle.find('a')['href']
        id = gen_id(year, conf, authors, title)
        bib = gen_single_bib(id, title, ' and '.join(authors), pdflink, year, conf)
        res += bib
    return res

def iclr_bib_2015_2016(year):
    assert(year == 2015 or year == 2016)
    # 15 first workshop: 
    first_workshop_15 = 'Learning Non-deterministic Representations with Energy-based Ensembles'
    link = ('https://iclr.cc/archive/www/doku.php%3Fid=iclr'
            + str(year) + ':accepted-main.html')
    conf = 'ICLR'
    html_file = download_to_hash(link)
    soup = BeautifulSoup(open(html_file), 'lxml')
    res = ''
    for div in soup.select('li.level1 div'):
        title = div.a.get_text()
        pdflink = div.a['href']
        div.a.decompose()
        authors_str = div.get_text()
        authors = authors_str2lst(authors_str)
        if authors:
            # change title to workshop
            if year == 2015 and first_workshop_15 in title:
                conf = 'ICLRWorkshop'
            id = gen_id(year, conf, authors, title)
            bib = gen_single_bib(id, title, ' and '.join(authors), pdflink, year, conf)
            res += bib
    return res

# 13, 17
def open_review_bib_iclr_2013_2017(conf, year, localfile):
    # need to download manually
    first_reject_13 = 'Heteroscedastic Conditional Ordinal Random'
    first_reject_17 = 'Energy-Based Spherical Sparse Coding'
    first_workshop_13 = 'Why Size Matters: Feature Coding as Nystrom Sampling'
    first_workshop_17 = 'Learning Continuous Semantic Representations'
    assert(year in [2013, 2017])
    if year == 2013:
        first_reject = first_reject_13
        first_workshop = first_workshop_13
    else:
        first_reject = first_reject_17
        first_workshop = first_workshop_17
        
    soup = BeautifulSoup(open(localfile), 'lxml')
    res = ''
    for paper in soup.select('#notes .note.panel'):
        title = paper.select('h2')[0].get_text().strip()
        pdflink = paper.select('a.note_content_pdf')[0]['href']
        if first_reject in title:
            break
        if first_workshop in title:
            conf = 'ICLRWorkshop'
        if not pdflink.startswith('http'):
            pdflink = 'https://openreview.net/' + pdflink
        authors_str = paper.select('.meta_row')[0].get_text()
        authors = authors_str2lst(authors_str)
        id = gen_id(year, conf, authors, title)
        bib = gen_single_bib(id, title, ' and '.join(authors), pdflink, year, conf)
        res += bib
    return res

def open_review_bib_iclr_2018_2019(year, localfile):
    # this is different from others ..
    # year = 2018
    soup = BeautifulSoup(open(localfile), 'lxml')
    res = ''
    sections = ['#accepted-oral-papers li.note',
                '#accepted-poster-papers li.note',
                '#workshop-papers li.note']
    confs = ['ICLR', 'ICLR', 'ICLRWorkshop']
    for section, conf in zip(sections, confs):
        for paper in soup.select(section):
            title = paper.h4.a.get_text().strip()
            # there is one error in 2018
            if title == 'No Title': continue
            pdflink = paper.select('.pdf-link')[0]['href']
            if not pdflink.startswith('http'):
                pdflink = 'https://openreview.net/' + pdflink
            authors_str = paper.select('.note-authors')[0].get_text()
            authors = authors_str2lst(authors_str)
            id = gen_id(year, conf, authors, title)
            bib = gen_single_bib(id, title, ' and '.join(authors), pdflink, year, conf)
            res += bib
    return res


iclr_years = list(range(2013, 2020))
# NOTE: ICLR 2013,2017,2018 requires to download html manually,
# because they are generated by javascript. see iclr_xxxx_html
# variables.
def iclr_bib(year):
    if year == 2013:
        iclr_2013_html = './html/ICLR-2013-OpenReview.html'
        bib = open_review_bib_iclr_2013_2017('ICLR', 2013, iclr_2013_html)
        return bib
    elif year == 2014:
        return iclr_bib_2014()
    elif year == 2015 or year == 2016:
        return iclr_bib_2015_2016(year)
    elif year == 2017:
        iclr_2017_html = './html/ICLR-2017-OpenReview.html'
        bib = open_review_bib_iclr_2013_2017('ICLR', 2017, iclr_2017_html)
        return bib
    elif year == 2018:
        iclr_2018_html = './html/ICLR-2018-OpenReview.html'
        bib = open_review_bib_iclr_2018_2019(year, iclr_2018_html)
        return bib
    elif year == 2019:
        iclr_2019_html = './html/ICLR-2019-OpenReview.html'
        bib = open_review_bib_iclr_2018_2019(year, iclr_2019_html)
        return bib
        
        
if __name__ == '__test__':
    iclr_bib(2014)
    bib = iclr_bib(2018)
    bib = iclr_bib(2019)
