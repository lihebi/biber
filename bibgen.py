#!/usr/bin/env python3

from bs4 import BeautifulSoup
import os
import re
import requests
import time
import json
import argparse
import itertools


##############################
## HTML download for ML
##
## because we need to parse the file to get the link
##############################

def download_html_ml():
    url = 'https://link.springer.com/journal/volumesAndIssues/10994'
    # download this index
    # parse for all issues
    # download html for each issues
    r = requests.get(url)
    with open('test.html', 'wb') as f:
        f.write(r.content)
    with open('test.html') as f:
        soup = BeautifulSoup(f, 'lxml')
        issues = [('https://link.springer.com' + issue.a['href'],
                   re.search('([0-9]{4}), Issue',
                             issue.get_text().strip())
                   .groups()[0])
                  for issue in soup.select('.issue-item')]
        for year, items in itertools.groupby(reversed(issues),
                                             lambda x: x[1]):
            for idx, (url, _) in enumerate(items, start=1):
                # print(url)
                download_file(url, './html/ML/ML-' + year + '-' + str(idx) + '.html')


def download_file(url, filename):
    print('downloading ' + url + ' into ' + filename + ' ..')
    if os.path.exists(filename):
        print('exists. Skip')
    else:
        r = requests.get(url)
        with open(filename, 'wb') as f:
            f.write(r.content)
        print('sleeping 5 sec ..')
        time.sleep(5)


##############################
## Download pdf
##############################

def download_pdf(url, filename):
    print('downloading to ' + filename + ' ..')
    r = requests.get(url)
    with open(filename, 'wb') as f:
        f.write(r.content)
                

def download_bib_pdfs(bib_file):
    ids = []
    urls = []
    conf = path2conf(bib_file)
    outdir = './pdf/auto/' + conf
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    with open(bib_file) as f:
        for line in f:
            line = line.strip()
            if line.startswith('@'):
                ids.append(line[line.find('{')+1:-1])
            elif line.startswith('pdflink'):
                urls.append(line[line.find('{')+1:-1])
            else:
                pass
    for id, url in zip(ids, urls):
        filename = os.path.join(outdir, id + '.pdf')
        download_pdf(url, filename)
        print('sleeping 7 sec ..')
        time.sleep(7)

        
##############################
## Gen bib by conference
##############################

def gen_aaai_bib(html_file):
    AAAI_pdf_prefix = 'https://aaai.org/Papers/AAAI/'
    year = path2year(html_file)
    # https://aaai.org/Library/AAAI/2007/aaai07-003.php
    res = '\n'
    with open(html_file) as f:
        soup = BeautifulSoup(f, "html.parser")
        if int(year) < 2010:
            for p in soup.select('#content')[0]('p'):
                if p.a and p.i and p.a.string and p.i.string:
                    href = p.a['href']
                    title = p.a.string.replace('\n', '')
                    authors_str = p.i.string
                    authors = authors_str2lst(authors_str)
                    first_author_last_name = authors[0].split(' ')[-1]
                    pdflink = (AAAI_pdf_prefix
                               + href.replace('.php', '.pdf').replace('aaai', 'AAAI'))
                    id = (year + '-AAAI-'
                          + first_author_last_name
                          + '-' + title_first_word(title))
                    res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                          year, "AAAI")
        else:
            # 10-18
            for paper in soup.select('#content table'):
                tds = paper.find_all('td')
                if tds[1].a:
                    title = tds[0].a.get_text()
                    href = tds[1].a['href']
                    authors_str = tds[2].string
                    authors = authors_str2lst(authors_str)
                    first_author_last_name = authors[0].split(' ')[-1]
                    pdflink = href.replace('view', 'download')
                    id = (year + '-AAAI-'
                          + first_author_last_name
                          + '-' + title_first_word(title))
                    res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                          year, "AAAI")
    return res

def gen_nips_bib(html_file):
    # https://papers.nips.cc/book/advances-in-neural-information-processing-systems-30-2017
    # https://papers.nips.cc/paper/6606-wider-and-deeper-cheaper-and-faster-tensorized-lstms-for-sequence-learning
    # https://papers.nips.cc/paper/6606-wider-and-deeper-cheaper-and-faster-tensorized-lstms-for-sequence-learning.pdf
    NIPS_pdf_prefix = 'https://papers.nips.cc/'
    res = '\n'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        year = path2year(html_file)
        for li in soup.select("div.main")[0].ul.find_all('li'):
            href = li.a['href']
            title = li.a.string
            authors = list(map(lambda author: author.string, li.select('.author')))
            pdflink = NIPS_pdf_prefix + href + '.pdf'
            if title and authors:
                id = (year + '-NIPS-'
                      + authors[0].split(' ')[-1]
                      + '-' + title_first_word(title))
                res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                      year, "NIPS")
    return res


def gen_mlr_bib(html_file):
    # ACML
    # COLT
    # AISTATS
    year = path2year(html_file)
    conf = path2conf(html_file)
    res = '\n'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        for paper in soup.select('.paper'):
            pdflink = paper.find_all('a')[1]['href']
            title = paper.p.string
            authors_str = paper.select('.authors')[0].string
            authors = authors_str2lst(authors_str)
            id = gen_id(year, conf, authors, title)
            res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                  year, conf)
    return res

def gen_ijcai_bib(html_file):
    year = path2year(html_file)
    res = '\n'
    pdflink_prefix = 'https://www.ijcai.org'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'lxml')
        # https://www.ijcai.org/proceedings/2017/0005.pdf
        if year == "2017":
            # main track
            for paper in soup.select('#section1')[0].select('.paper_wrapper'):
                title = paper.div.string
                authors_str = paper.select('.authors')[0].string
                authors = authors_str2lst(authors_str)
                # 0005.pdf
                href = paper.find('a')['href']
                pdflink = pdflink_prefix + '/proceedings/' + year + '/' + href
                id = gen_id(year, 'IJCAI', authors, title)
                res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                      year, "IJCAI")
        else:
            query_str = '.content .field-items .field-item p'
            # if year == "2011": query_str = 'p'
            for paper in soup.select(query_str):
                title = ''
                authors_str = ''
                pdflink = ''
                if year == "2016":
                    if paper.i and paper.i.string and paper.a:
                        title = list(paper.strings)[0]
                        title = title[:title.rfind('/')].strip()
                        authors_str = paper.i.string
                        href = paper.a['href']
                        pdflink = pdflink_prefix + href
                elif year == "2015":
                    if paper.em and paper.em.string and paper.a:
                        title = list(paper.strings)[0]
                        title = title[:title.rfind('/')].strip()
                        # diff 1: <em> is used for author
                        authors_str = paper.em.string
                        # diff 2: second link is pdf
                        href = paper.select('a')[1]['href']
                        pdflink = pdflink_prefix + href
                elif year == "2013" or year == "2011":
                    if paper.i and paper.i.string and paper.a:
                        # diff 1: title is inside <a> tag, and no trailing "/ 27"
                        title = paper.a.get_text()
                        authors_str = paper.i.string
                        # diff 2: the title link itself is pdf
                        pdflink = paper.a['href']
                elif year == "2007" or year == "2009":
                    if paper.i and paper.i.string and paper.a:
                        # second <a> is title
                        atags = paper.select('a')
                        if len(atags) > 1:
                            title_a = paper.select('a')[1]
                            title = title_a.get_text()
                            authors_str = paper.i.string
                            pdflink = title_a['href']
                elif year == "2005":
                    if paper.a:
                        title = paper.a.string
                        pdflink = paper.a['href']
                        # the author in in next element <p>
                        authors_str = paper.find_next_sibling('p').string
                elif year == "2003":
                    if paper.a:
                        title = paper.a.string
                        pdflink = pdflink_prefix + paper.a['href']
                        # the author in in next element <p>
                        authors_p = paper.find_next_sibling('p')
                        if authors_p:
                            authors_str = authors_p.string
                            authors_str = authors_str[:authors_str.find('.')]
                if title and authors_str and pdflink:
                    authors = authors_str2lst(authors_str)
                    id = gen_id(year, 'IJCAI', authors, title)
                    res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                          year, "IJCAI")
    return res

def gen_ijcai_all():
    outdir = './bib/auto/IJCAI'
    htmldir = './html/IJCAI'
    if not os.path.exists(outdir):
        os.makedirs(outdir)

    for root, dirs, files in os.walk(htmldir):
        for f in files:
            p = os.path.join(root, f)
            year = int(path2year(p))
            if year >= 2003:
                print(p)
                outfile = os.path.join(outdir, p[p.rfind('/')+1:p.rfind('.')]+'.bib')
                with open(outfile, 'w') as f:
                    f.write(gen_ijcai_bib(p))


def uai_pdflink(proceeding_id, paper_id):
    # https://dslpitt.org/uai/displayArticleDetails.jsp?mmnu=1&smnu=2&article_id=988&proceeding_id=1
    url = ('https://dslpitt.org/uai/displayArticleDetails.jsp?mmnu=1&smnu=2'
           + '&article_id=' + str(paper_id)
           + '&proceeding_id=' + str(proceeding_id))
    r = requests.get(url)
    soup = BeautifulSoup(r.content, 'html.parser')
    pdflink_prefix = 'https://dslpitt.org'
    href = soup.select('a[href^="/papers"]')[0]['href']
    pdflink = pdflink_prefix + href
    return pdflink

def uai_pdflink_local(proceeding_id, paper_id):
    '''
    get pdf link from local file
    '''

def gen_uai_pdflink(html_file):
    year = path2year(html_file)
    proceeding_id = int(year) - 1984
    outfile = html_file.replace('.html', '.json')
    res = {}
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        all_paper_ids = [re.search(r'article_id=(\d+)', td.a['href'])[1]
                         for td in soup.select('.cArticlesLstTitle')]
        for paper_id in all_paper_ids:
            pdflink = uai_pdflink(proceeding_id, paper_id)
            res[paper_id] = pdflink
            print(pdflink)
            # print('sleeping 5 sec')
            time.sleep(5)
    with open(outfile, 'w') as f:
        json.dump(res, f, indent=2)

        
def gen_uai_pdflink_all():
    for root, dirs, files in os.walk('./html/UAI'):
        for f in files:
            if f.endswith('.html'):
                p = os.path.join(root, f)
                # do this only for 1985-2014
                year = int(path2year(p))
                if year < 2015:
                    print(p)
                    print('---------')
                    gen_uai_pdflink(p)
                

def gen_uai_bib(html_file):
    # proceeding id: 1-30
    # year: 1985-2014
    year = path2year(html_file)
    proceeding_id = int(year) - 1984
    json_file = html_file.replace('.html', '.json')
    res = '\n'
    id2pdflink = json.load(open(json_file))
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        all_titles = [td.a.string for td in soup.select('.cArticlesLstTitle')]
        all_authors = [', '.join(list(map(lambda x: x.string, td.find_all('a'))))
                       for td in soup.select('.cArticlesLstAuthors')]
        all_paper_ids = [re.search(r'article_id=(\d+)', td.a['href'])[1]
                         for td in soup.select('.cArticlesLstTitle')]

        assert len(all_titles) == len(all_authors) == len(all_paper_ids)
        for title, authors_str, paper_id in zip(all_titles, all_authors, all_paper_ids):
            authors = authors_str2lst(authors_str)
            id = gen_id(year, 'UAI', authors, title)
            # pdflink = uai_pdflink(proceeding_id, paper_id)
            pdflink = id2pdflink[paper_id]
            res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                  year, "UAI")
    return res

def gen_uai_bib_2015(html_file):
    # 15-17
    # http://auai.org/uai2015/proceedings/papers/9.pdf
    # href="proceedings/papers/9.pdf"
    year = path2year(html_file)
    pdflink_prefix = 'http://auai.org/uai2015/'
    res = '\n'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        for paper in soup.select('#mytable tr'):
            tds = paper.find_all('td')
            if (len(tds) == 2):
                href = paper.td.a['href']
                pdflink = pdflink_prefix + href
                td = paper.find_all('td')[1]
                title = td.div.b.get_text()
                authors_str = td.i.string
                authors = authors_str2lst(authors_str)
                id = gen_id(year, 'UAI', authors, title)
                res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                      year, "UAI")
    return res

def gen_uai_bib_2016(html_file):
    pdflink_prefix = 'http://auai.org/uai2016/'
    year = path2year(html_file)
    res = '\n'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        tds = soup.select('#mytable td')
        odd_tds = tds[::2]
        even_tds = tds[1::2]
        for odd_td, even_td in zip(odd_tds, even_tds):
            href = odd_td.a['href']
            pdflink = pdflink_prefix + href
            title = even_td.div.b.get_text()
            authors_str = even_td.i.string
            authors = authors_str2lst(authors_str)
            id = gen_id(year, 'UAI', authors, title)
            res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                  year, "UAI")
            pass
        pass
    return res

def gen_uai_bib_2017(html_file):
    res = '\n'
    year = path2year(html_file)
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        tds = soup.select('td')
        odd_tds = tds[::2]
        even_tds = tds[1::2]
        for odd_td, even_td in zip(odd_tds, even_tds):
            href = odd_td.h5.a['href']
            pdflink = href
            title_h4 = even_td.find_all('h4')[-1]
            title = title_h4.get_text()
            authors_str = title_h4.next_sibling
            authors = authors_str2lst(authors_str)
            id = gen_id(year, 'UAI', authors, title)
            res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                  year, "UAI")
            pass
        pass
    return res

if __name__ == '__hebi__':
    gen_uai_bib('./html/UAI/UAI-2004.html')
    gen_all_by_year('UAI', range(1985, 2015), gen_uai_bib)
    gen_uai_bib_2015('./html/UAI/UAI-2015.html')
    gen_uai_bib_2016('./html/UAI/UAI-2016.html')
    gen_uai_bib_2017('./html/UAI/UAI-2017.html')
    gen_all_by_year('UAI', [2015], gen_uai_bib_2015)
    gen_all_by_year('UAI', [2016], gen_uai_bib_2016)
    gen_all_by_year('UAI', [2017], gen_uai_bib_2017)

    s = BeautifulSoup(open('./html/UAI/UAI-2017.html'))
    s.select('td')[1]


def gen_uai_all():
    gen_all('./bib/auto/UAI',
            './html/UAI',
            gen_uai_bib)

def gen_icml_2010(html_file):
    year = path2year(html_file)
    res = '\n'
    pdflink_prefix = 'https://icml.cc/Conferences/2010/'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        for h3 in soup.select('table td h3'):
            author_p = h3.find_next_sibling('p')
            abstract_p = author_p.find_next_sibling('p')
            discussion_p = abstract_p.find_next_sibling('p')
            if author_p.em:
                title = h3.get_text()
                href = discussion_p.a['href']
                # https://icml.cc/Conferences/2010/papers/16.pdf
                # papers/26.pdf
                pdflink = pdflink_prefix + href
                authors_str = author_p.em.string
                # this author string is
                # Mark Cummins (University of Oxford); Paul Newman (University of Oxford)
                # remove the universities
                # re.sub(r'\([^\(]*\)', '',
                #        'Mark Cummins (University of Oxford); Paul Newman (University of Oxford)')
                authors_str = re.sub(r'\([^\(]*\)', '', authors_str)
                authors = authors_str2lst(authors_str)
                id = gen_id(year, 'ICML', authors, title)
                res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                      year, "ICML")
                pass
            pass
        pass
    return res

def gen_icml_2011(html_file):
    year = path2year(html_file)
    res = '\n'
    pdflink_prefix = 'https://icml.cc/Conferences/2011/'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        for paper in soup.select('a[name]'):
            if paper.h3:
                title = paper.h3.get_text()
                authors_str = paper.span.string
                link_p = paper.p.find_next_sibling('p')
                if link_p and link_p.a:
                    href = link_p.a['href']
                    # https://icml.cc/Conferences/2011/papers/9_icmlpaper.pdf
                    # papers/9_icmlpaper.pdf
                    pdflink = pdflink_prefix + href
                    authors = authors_str2lst(authors_str)
                    id = gen_id(year, 'ICML', authors, title)
                    res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                          year, "ICML")
                    pass
                pass
            pass
        pass
    return res

def gen_icml_2012(html_file):
    year = path2year(html_file)
    res = '\n'
    pdflink_prefix = 'https://icml.cc/Conferences/2012/'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        for paper in soup.select('.paper'):
            title = paper.h2.get_text()
            authors_str = list(paper.p.strings)[0]
            if paper.a and paper.a.find_next_sibling('a'):
                href = paper.a.find_next_sibling('a')['href']
                # https://icml.cc/Conferences/2012/papers/917.pdf
                # papers/113.pdf
                pdflink = pdflink_prefix + href
                authors = authors_str2lst(authors_str)
                id = gen_id(year, 'ICML', authors, title)
                res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                      year, "ICML")
                pass
            pass
        pass
    return res


def gen_jmlr_bib(html_file):
    year = path2year(html_file)
    res = "\n"
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'lxml')
        for paper in soup.select('dl'):
            title = paper.dt.get_text()
            authors_str = paper.dd.b.i.string
            pdflink = list(filter(lambda x: "pdf" in x.string.strip(),
                                  paper.select('a')))[0]['href']
            if not pdflink.startswith('http'):
                pdflink = 'http://www.jmlr.org' + pdflink
            # pdflink = paper.select('a')[1]['href']
            # http://www.jmlr.org/papers/volume18/16-002/16-002.pdf
            authors = authors_str2lst(authors_str)
            id = gen_id(year, 'JMLR', authors, title)
            res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                  year, "JMLR")
            pass
        pass
    return res

def gen_jmlr_all():
    gen_all('./bib/auto/JMLR',
            './html/JMLR',
            gen_jmlr_bib)




def gen_ml_bib(html_file):
    year = path2year(html_file)
    res = '\n'
    pdflink_prefix = 'https://link.springer.com'
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'lxml')
        for paper in soup.select('.toc-item'):
            title = paper.h3.a.get_text().strip()
            href = paper.h3.a['href']
            # FIXME this is not PDF link yet
            pdflink = pdflink_prefix + href
            # change to real pdf link
            # https://link.springer.com/article/10.1007/s10994-006-6889-7
            # https://link.springer.com/content/pdf/10.1007/s10994-006-6889-7.pdf
            pdflink = pdflink.replace('article', 'content/pdf')
            pdflink += '.pdf'
            authors_str = paper.span.get_text().strip()
            authors = authors_str2lst(authors_str)
            if authors:
                id = gen_id(year, 'ML', authors, title)
                res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                      year, "Machine Learning")
    return res

def gen_ml_bib_all():
    # gen_all('./bib/auto/ML', './html/ML', gen_ml_bib)
    # merge ml bibs
    for year, files in itertools.groupby(sorted(os.listdir('./html/ML')),
                                         lambda x:
                                         re.search('ML-([0-9]{4})',
                                                   x).groups()[0]):
        bib = ''
        for f in files:
            html_file = os.path.join('./html/ML', f)
            bib += gen_ml_bib(html_file)
        bib_file = os.path.join('./bib/auto/ML', 'ML-' + str(year) + '.bib')
        print(bib_file)
        with open(bib_file, 'w') as f:
            f.write(bib)

def gen_cvpr_bib(html_file):
    "Generate CVPR bib as string from html"
    year = path2year(html_file)
    res = "\n"
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        for paper in soup.select('.ptitle'):
            title = paper.a.get_text()
            dd = paper.find_next_sibling('dd')
            authors = [a.get_text() for a in dd.select('a')]
            dd = dd.find_next_sibling('dd')
            ahref = list(filter(lambda x: 'pdf' in x.get_text().strip(),
                                dd.select('a')))[0]
            pdflink = 'http://openaccess.thecvf.com/' + ahref['href']
            id = gen_id(year, 'CVPR', authors, title)
            res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                  year, "CVPR")
    return res

def gen_iccv_bib(html_file):
    # TODO this is the same as above, refactor it
    "Generate ICCV bib as string from html"
    year = path2year(html_file)
    res = "\n"
    with open(html_file) as f:
        soup = BeautifulSoup(f, 'html.parser')
        for paper in soup.select('.ptitle'):
            title = paper.a.get_text()
            dd = paper.find_next_sibling('dd')
            authors = [a.get_text() for a in dd.select('a')]
            dd = dd.find_next_sibling('dd')
            ahref = list(filter(lambda x: 'pdf' in x.get_text().strip(),
                                dd.select('a')))[0]
            pdflink = 'http://openaccess.thecvf.com/' + ahref['href']
            id = gen_id(year, 'ICCV', authors, title)
            res += gen_single_bib(id, title, ' and '.join(authors), pdflink,
                                  year, "ICCV")
    return res

smart_scholar_html_dir = '/home/hebi/github/smart-scholar-dist/html'
smart_scholar_bib_dir = '/home/hebi/github/smart-scholar-dist/bib'

# gen_iccv_bib(os.path.join(smart_scholar_html_dir, 'ICCV/ICCV-2015.html'))


##############################
## Gen All
##############################

def gen_cvpr_all():
    gen_all(os.path.join(smart_scholar_bib_dir, 'CVPR'),
            os.path.join(smart_scholar_html_dir, 'CVPR'),
            gen_cvpr_bib)

def gen_iccv_all():
    gen_all(os.path.join(smart_scholar_bib_dir, 'ICCV'),
            os.path.join(smart_scholar_html_dir, 'ICCV'),
            gen_iccv_bib)

def gen_aaai_all():
    gen_all('./bib/auto/AAAI',
            './html/AAAI',
            gen_aaai_bib)

    
def gen_nips_all():
    gen_all('./bib/auto/NIPS',
            './html/NIPS',
            gen_nips_bib)

def gen_acml_all():
    gen_all('./bib/auto/ACML',
            './html/ACML',
            gen_mlr_bib)

def gen_colt_all():
    gen_all('./bib/auto/COLT',
            './html/COLT',
            gen_mlr_bib)

def gen_aistats_all():
    gen_all('./bib/auto/AISTATS',
            './html/AISTATS',
            gen_mlr_bib)
    
def gen_all(outdir, htmldir, gen_bib_func):
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    for root, dirs, files in os.walk(htmldir):
        for f in files:
            p = os.path.join(root, f)
            print(p)
            outfile = os.path.join(outdir, p[p.rfind('/')+1:p.rfind('.')]+'.bib')
            with open(outfile, 'w') as f:
                f.write(gen_bib_func(p))
                
def gen_all_by_year(conf, years, gen_bib_func):
    outdir = './bib/auto/' + conf
    htmldir = './html/' + conf
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    for root, dirs, files in os.walk(htmldir):
        for f in files:
            p = os.path.join(root, f)
            year = int(path2year(p))
            if p.endswith('.html') and year in years:
                print(p)
                outfile = os.path.join(outdir, p[p.rfind('/')+1:p.rfind('.')]+'.bib')
                with open(outfile, 'w') as f:
                    f.write(gen_bib_func(p))
        
    
if __name__ == '__main__':
    # TODO the interface should be
    #
    # bibgen.py /path/to/htmls /path/to/bibs
    #
    # the script scan htmls and bibs, for each conf/conf-2000.html, if
    # the bib file is not there in the bib directory, generate the bib
    # by calling gen_bib(/path/to/html/file, /path/to/bib/file)
    
    parser = argparse.ArgumentParser()
    parser.add_argument('files', nargs='+')
    args = parser.parse_args()
    print(len(args.files))
    for f in args.files:
        download_bib_pdfs(f)
                
if __name__ == '__hebi__':
    print(gen_ml_bib('./html/ML/ML-1986-1.html'))

    gen_jmlr_bib('./html/JMLR/JMLR-2001.html')
    s = BeautifulSoup(open('./html/JMLR/JMLR-2008.html'), 'lxml')
    s.select('dl')[0]

    gen_icml_2010('./html/ICML/ICML-2010.html')
    gen_icml_2011('./html/ICML/ICML-2011.html')
    gen_icml_2012('./html/ICML/ICML-2012.html')
    gen_all_by_year('ICML', [2010], gen_icml_2010)
    gen_all_by_year('ICML', [2011], gen_icml_2011)
    gen_all_by_year('ICML', [2012], gen_icml_2012)

    
    'https://www.aaai.org/ocs/index.php/AAAI/AAAI10/paper/view/1811/1912'
    'https://www.aaai.org/ocs/index.php/AAAI/AAAI10/paper/view/1893/1938'
    'https://www.aaai.org/ocs/index.php/AAAI/AAAI10/paper/view/1876/1942'
    download_pdf(url, 'test.pdf')

    read_bib('./bib/auto/NIPS/NIPS-2017.bib')
    download_bib_pdfs('./bib/auto/NIPS/NIPS-2017.bib')

    path2year("./html/AAAI/AAAI-2005.html")
    path2conf("./html/AAAI/AAAI-2005.html")

    gen_all_by_year('ICML', range(2013, 2019), gen_mlr_bib)

    gen_pdflink('./html/UAI/UAI-2013.html')

    r = requests.get('https://dslpitt.org/uai/displayArticleDetails.jsp?mmnu=1&smnu=2&article_id=988&proceeding_id=1')

    uai_pdflink(1, 988)
    uai_pdflink(29, 2398)

    print(gen_uai_bib('./html/UAI/UAI-2013.html'))

    print(gen_nips_bib('./html/NIPS/NIPS-2010.html'))
    print(gen_mlr_bib('./html/ACML/ACML-2017.html'))

    len(s.select('.cArticlesLstTitle'))
    len(s.select('.cArticlesLstAuthors'))

    

    with open('test.html') as f:
        s = BeautifulSoup(f, 'lxml')


            
        
    with open('test.bib', 'w') as f:
        f.write(gen_aaai_bib('./html/AAAI/AAAI-1998.html'))


