#!/usr/bin/env python3

# IEEE paper download

# https://ieeexploreapi.ieee.org/api/v1/search/articles?parameter&apikey=
# https://ieeexploreapi.ieee.org/api/v1/search/articles?publication_year=2018

from bs4 import BeautifulSoup
import os
import requests
import re
import time
import math

from utils import download_file, gen_id, gen_single_bib, download_to_hash

def ieee_index_page(punumber):
    """Return a list [title] and [link] for each conference year. The
    year,link pair should be determined outside.

    """
    # if not exist, download to a tmp file
    # https://ieeexplore.ieee.org/xpl/conhome.jsp?punumber=1000639
    # parse the file
    # get the list of punumber for each year, and return
    url = 'https://ieeexplore.ieee.org/xpl/conhome.jsp?punumber=' + str(punumber)
    html_file = download_to_hash(url)
    if not os.path.exists(html_file):
        download_file(url, html_file)
    soup = BeautifulSoup(open(html_file), 'lxml')
    texts = []
    links = []
    for a in soup.select('.detail li a'):
        text = a.get_text().strip()
        link = a['href']
        if not link.startswith('http'):
            link = 'https://ieeexplore.ieee.org/xpl/' + link
        texts.append(text)
        links.append(link)
    return list(reversed(texts)), list(reversed(links))

def ieee_journal_index_page(punumber):
    """Return a {year: [links]}
    """
    # https://ieeexplore.ieee.org/xpl/RecentIssue.jsp?punumber=8860
    # punumber=8860
    url = 'https://ieeexplore.ieee.org/xpl/RecentIssue.jsp?punumber=' + str(punumber)
    html_file = download_to_hash(url)
    soup = BeautifulSoup(open(html_file), 'lxml')
    volumes = {}
    for ul in soup.select('.volumes ul'):
        # pi-2012
        year = int(ul['id'].split('-')[1])
        if not year in volumes:
            volumes[year] = []
        for a in ul.select('li a'):
            link = a['href']
            if not link.startswith('http'):
                link = 'https://ieeexplore.ieee.org/' + link
            volumes[year].append(link)
    return volumes
    
def ieee_conference_get_pagination(year, conf, link):
    """Return [link] for all pagination of this conference
    """
    html_file = download_to_hash(link)
    soup = BeautifulSoup(open(html_file), 'lxml')
    res = []
    # if have pagination
    hidden = soup.select('input#oqs')[0]['value']
    text = soup.select('.results-display')[0].get_text()
    # Displaying Results 801 - 824 of 824
    total = int(re.findall(r'Displaying Results .* of (\d+)', text)[0])
    # rowsPerPage=100
    pagenum = math.ceil(total / 100)
    for page in range(0, pagenum):
        # https://ieeexplore.ieee.org/xpl/mostRecentIssue.jsp?punumber=8449910&filter=issueId%20EQ%20%228460178%22&pageNumber=2
        link += ('&' + hidden
                 + '&rowsPerPage=100'
                 + '&pageNumber=' + str(page+1))
        res.append(link)
    return res

def ieee_conference_bib_with_pagination(year, conf, link):
    res = ''
    pages = ieee_conference_get_pagination(year, conf, link)
    print('total pages to process: ', len(pages))
    for page in pages:
        res += ieee_conference_bib(year, conf, page)
    return res

if __name__ == '__test__':
    ieee_conference_get_pagination(2018, 'ICRA', 'https://ieeexplore.ieee.org/xpl/mostRecentIssue.jsp?punumber=8449910')
    
def ieee_conference_bib(year, conf, link):
    html_file = download_to_hash(link)
    soup = BeautifulSoup(open(html_file), 'lxml')
    res = ''
    for div in soup.select('.txt'):
        if div.h3.a and div.select('.authors a'):
            for f in div.select('formula'):
                f.decompose()
            title = div.h3.get_text()
            # "/document/8461262/"
            arnumber = re.findall(r'/(\d+)/', div.h3.a['href'])[0]
            pdflink = 'https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=' + arnumber
            authors = [author.get_text().strip() for author in div.select('.authors a')]
            # print(authors)
            id = gen_id(year, conf, authors, title)
            bib = gen_single_bib(id, title, ' and '.join(authors), pdflink, year, conf)
            res += bib
    return res

icra_years = list(range(1984, 2019))
def icra_bib(year):
    icra_index_punumber = '1000639'
    _, links = ieee_index_page(icra_index_punumber)
    link = links[year - icra_years[0]]
    return ieee_conference_bib_with_pagination(year, 'ICRA', link)

tro_years = list(range(2004, 2019))
def tro_bib(year):
    tro_index_punumber = 8860
    # TODO TRO has two more titles. This one only cover [2004,]
    volumes = ieee_journal_index_page(tro_index_punumber)
    res = ''
    if year in volumes:
        links = volumes[year]
        for link in links:
            res += ieee_conference_bib_with_pagination(year, 'TRO', link)
            # sleep for journal
            # print('sleeping 3 sec ..')
            # time.sleep(3)
    return res


iros_years = list(range(1988, 2018))
def iros_bib(year):
    iros_index_punumber = '1000393'
    _, links = ieee_index_page(iros_index_punumber)
    link = links[year - iros_years[0]]
    return ieee_conference_bib_with_pagination(year, 'IROS', link)


# missing 1990
cvpr_years = list(range(1988, 1990))
# missing 1995
cvpr_years += list(range(1991, 1995))
# missing 2002
cvpr_years += list(range(1996, 2002))
# 2013-after can be downloaded via http://openaccess.thecvf.com
cvpr_years += list(range(2003, 2018))
def cvpr_bib(year):
    # https://ieeexplore.ieee.org/xpl/conhome.jsp?punumber=1000147
    punumber = '1000147'
    _, links = ieee_index_page(punumber)
    d = {year:link for year,link in zip(cvpr_years, links)}
    link = d[year]
    return ieee_conference_bib_with_pagination(year, 'CVPR', link)

iccv_years = [1988, 1990, 1993, 1995, 1998, 1999, 2001, 2003, 2005,
              2007, 2009, 2011, 2013, 2015, 2017]
def iccv_bib(year):
    # https://ieeexplore.ieee.org/xpl/conhome.jsp?punumber=1000149
    punumber = '1000149'
    _, links = ieee_index_page(punumber)
    d = {year:link for year,link in zip(iccv_years, links)}
    link = d[year]
    return ieee_conference_bib_with_pagination(year, 'ICCV', link)
