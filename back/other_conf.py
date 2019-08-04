from bs4 import BeautifulSoup
import os
import requests
import re
import time
import math
from utils import download_file, gen_id, gen_single_bib, download_to_hash, authors_str2lst


nips_years = list(range(1988, 2019))
def nips_bib(year):
    ID = year - 1988 + 1
    conf = 'NIPS'
    link = 'https://papers.nips.cc/book/advances-in-neural-information-processing-systems-' + str(ID) + '-' + str(year)
    html_file = download_to_hash(link)
    NIPS_pdf_prefix = 'https://papers.nips.cc/'
    res = '\n'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        for li in soup.select("div.main")[0].ul.find_all('li'):
            href = li.a['href']
            title = li.a.string
            authors = list(map(lambda author: author.string, li.select('.author')))
            pdflink = NIPS_pdf_prefix + href + '.pdf'
            if title and authors:
                id = gen_id(year, conf, authors, title)
                bib = gen_single_bib(id, title, ' and '.join(authors),
                                     pdflink, year, conf)
                res += bib
    return res

def test():
    bib = nips_bib(2017)
    bib = nips_bib(2018)
