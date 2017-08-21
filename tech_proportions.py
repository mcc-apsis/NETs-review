
# coding: utf-8

# In[1]:
from bs4 import BeautifulSoup
import os, sys, time, resource, re, gc, shutil
from multiprocess import Pool
from functools import partial
from tabulate import tabulate
from urllib.parse import urlparse, parse_qsl

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from mongoengine.queryset.visitor import Q
import django
from django.db.models import Count, Sum
sys.path.append('/home/galm/software/django/tmv/BasicBrowser/')
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from django.forms.models import model_to_dict

from scoping.models import *
from tmv_app.models import *
from scipy import stats
import ipy_table as tbl
from itertools import islice

qid = 1498


# In[20]:

fnames = [
    'bib_data/pre_1998 [Nodes].csv',
    'bib_data/pre_2006 [Nodes].csv',
    'bib_data/pre_2013 [Nodes].csv'
]

for fname in fnames:

    df = pd.read_csv(fname)

    cols = df.columns

    df.head()

    q = Query.objects.get(pk=1558)


    # In[28]:

    p = Project.objects.get(title="NETs")
    techs = Technology.objects.filter(project = p).values_list('name',flat=True)

    ndf = df

    def istech(x,t):
        doi = str(x['url']).replace('http://dx.doi.org/','').strip()

        soup = BeautifulSoup(x['description'])
        rows = soup.find_all('tr')
        arow = [r for r in rows if r.find_all(text=re.compile('Authors'))]
        trow = [r for r in rows if r.find_all(text=re.compile('Title'))]
        title = trow[0].find_all('td')[1].text

        try:
            doc = Doc.objects.get(
                    wosarticle__di=doi
                )
        except:
            try:
                doc = Doc.objects.get(
                    query=q,
                    title__icontains=title
                )
            except:
                print(doi)
                return(np.nan)

        if doc.technology.name==t:
            return(1)
        elif DocOwnership.objects.filter(
                doc=doc,
                query__technology__name=t,
                relevant=1
            ).count() > 0:
            return(1)
        else:
            return(0)

    for t in techs:
        ndf[t] = ndf.apply(lambda x: istech(x,t),axis=1)

    ndf.head()


    # In[29]:

    ndf.head()

    len(ndf)

    len(ndf[ndf['BECCS'].isnull()])


    # In[77]:

    melted_ndf = pd.melt(ndf,id_vars=list(cols),var_name="Technology")
    melted_ndf.head()


    # In[78]:

    ct_sum = melted_ndf.groupby(['cluster', 'Technology']).agg({
        'value': 'sum'
    })

    ct_sum = ct_sum.reset_index().sort_values('cluster')

    ct_totals = ct_sum.groupby(['cluster']).sum().rename(columns={'value':'total'}).reset_index()

    ct_sum = ct_sum.merge(ct_totals)

    ct_sum['prop'] = ct_sum['value'] / ct_sum['total'] * 100

    ct_sum = ct_sum.sort_values(['cluster','prop'], ascending=[True, False])

    ct_sum.to_excel(fname+'_cluster_totals.xlsx')



# In[ ]:
