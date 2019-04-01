#!/usr/bin/env python3

from ieee import tro_bib, icra_bib, iros_bib, iccv_bib, cvpr_bib
from ieee import tro_years, icra_years, iros_years, iccv_years, cvpr_years
from acl import acl_bib, cl_bib, naacl_bib, eacl_bib, emnlp_bib
from iclr import iclr_bib, iclr_years
from other_conf import nips_bib, nips_years
from springer import eccv
import time

from config import *

def gen_bib(conf, bib_func, year):
    """bib_func should be (lambda (year))
    """
    bib_dir = BIB_DIR + conf
    if not os.path.exists(bib_dir):
        os.makedirs(bib_dir)
    bib_file = os.path.join(bib_dir, conf + '-' + str(year) + '.bib')
    if not os.path.exists(bib_file):
        bib = bib_func(year)
        with open(bib_file, 'w') as f:
            f.write(bib)

   
conference_metadata = {
    'CVPR': [cvpr_bib, cvpr_years],
    'ICCV': [iccv_bib, iccv_years],
    'TRO': [tro_bib, tro_years],
    'ICRA': [icra_bib, icra_years],
    'IROS': [iros_bib, iros_years],
    'ICLR': [iclr_bib, iclr_years],
    "NIPS": [nips_bib, nips_years]
}

def gen_by_conf_class(conf_class, year):
    conf_obj = conf_class()
    gen_bib(conf_obj.name, conf_obj.bib, year)

def gen_by_conf_class_all(conf_class):
    """Generate for all years
    """
    conf_obj = conf_class()
    for year in conf_obj.years():
        print('--', year)
        gen_by_conf_class(conf_class, year)

def gen_by_conf(conf, year=None):
    """If year is not given, download all
    """
    bib_func, years = conference_metadata[conf]
    if year:
        gen_bib(conf, bib_func, year)
    else:
        for year in years:
            print('--', year)
            gen_by_conf(conf, year)

def gen_acl(year):
    gen_bib('ACL', acl_bib, year)
    
def gen_cl(year):
    gen_bib('CL', cl_bib, year)
def gen_eacl(year):
    gen_bib('EACL', eacl_bib, year)
def gen_naacl(year):
    gen_bib('NAACL', naacl_bib, year)
def gen_emnlp(year):
    gen_bib('EMNLP', emnlp_bib, year)


def gen_acl_all():
    # TODO before 2000
    for year in range(2000, 2019):
        print('--', year)
        gen_acl(year)
def gen_cl_all():
    # TODO before 2000
    for year in range(2000, 2019):
        print('--', year)
        gen_cl(year)
def gen_naacl_all():
    # 18 16 15 13 12 10 09 07 06 04 03 01 00
    # ALL
    for year in [2000, 2001, 2003, 2004, 2006, 2007, 2009, 2010, 2012,
                 2013, 2015, 2016, 2018]:
        print('--', year)
        gen_naacl(year)
def gen_eacl_all():
    # 17 14 12 09 06 03
    # TODO before 2000
    for year in [2003, 2006, 2009, 2012, 2014, 2017]:
        print('--', year)
        gen_eacl(year)
def gen_emnlp_all():
    # ALL
    for year in range(1996, 2018):
        print('--', year)
        gen_emnlp(year)

if __name__ == '__hebi__':
    # download conference index
    # extract conference pnumber by year
    # download conference html by year
    # parse for bib
    # ICRA: https://ieeexplore.ieee.org/xpl/conhome.jsp?punumber=1000639
    # https://ieeexplore.ieee.org/servlet/opac?punumber=1000639
    ieee_index_page('1000639')
    bib = icra_bib(2018)
    
    gen_by_conf('ICRA', 2018)
    gen_by_conf('IROS', 2017)
    gen_by_conf('TRO')
    gen_by_conf('ICCV')
    gen_by_conf('CVPR')
    gen_by_conf('ICLR')
    gen_by_conf('NIPS')

    gen_by_conf_class(eccv, 2018)
    gen_by_conf_class_all(eccv)
    
    bib = tro_bib(2018)
    gen_tro(2007)
    gen_tro_all()
    gen_acl(2000)
    gen_acl_all()
    gen_cl_all()
    gen_naacl_all()
    gen_eacl_all()
    gen_emnlp_all()
    bib = tro_bib(2007)
    # gen_id(1028, 'hfh', 'he', 'a new')
    gen_iccv_all()
    gen_cvpr_all()
    pass
