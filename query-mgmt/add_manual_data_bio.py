import csv

import django, sys, os

sys.stdout.flush()

# import file for easy access to browser database
sys.path.append('/home/galm/software/django/tmv/BasicBrowser/')

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from scoping.models import *

tid = 481
qid = 1556

q = Query.objects.get(pk=qid)
t = Tag.objects.get(pk=tid)
u = User.objects.get(username="delm")

with open('manual_data/bioenergy.csv', "r") as csvfile:
    reader = csv.reader(csvfile,delimiter=';')
    for row in reader:
        #print(row)

        #break

        if len(row[0]) > 0:
            try:
                doc = Doc.objects.get(
                    query=q,
                    title=row[0],
                    PY=int(row[1])#,
                    #duplicated=False
                )
                do, created = DocOwnership.objects.get_or_create(
                    tag=t,
                    query=q,
                    user=u,
                    doc=doc
                )
                r = int(row[4])
                if r ==0:
                    r = 2
                do.relevant = r
                do.save()
            except:
                print("nodoc!")
                print(row[0])
                print(row[4])
