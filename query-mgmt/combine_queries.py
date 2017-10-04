import re, sys, os, time, subprocess
import django
from django.utils import timezone

# import file for easy access to browser database
sys.path.append('/home/galm/software/tmv/BasicBrowser/')

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from scoping.models import *

qids = list(Query.objects.filter(id__gt=1418).order_by('id').values_list('id',flat=True))

for q in Query.objects.filter(id__in=qids):
    if q.id % 2 == 1:
        q1 = q
        q2 = Query.objects.get(pk=q.id+1)
    else:
        continue
    nq = Query(
        title=q1.title.replace('_a_wos','_extra'),
        type="default",
        text="{} OR {}".format(q1.id,q2.id),
        creator = User.objects.get(username="galm"),
        date = timezone.now(),
        database = "intern"
    )
    nq.save()
    docs = Doc.objects.filter(query=q1) | Doc.objects.filter(query=q2)
    for d in docs.distinct('UT'):
        d.query.add(nq)
    nq.r_count = len(docs.distinct('UT'))
    nq.save()
