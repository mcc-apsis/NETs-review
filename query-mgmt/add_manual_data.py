import csv

import django, sys, os

sys.stdout.flush()

# import file for easy access to browser database
sys.path.append('/home/galm/software/tmv/BasicBrowser/')

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from scoping.models import *

tid = 418
qid = 1481

q = Query.objects.get(pk=qid)
t = Tag.objects.get(pk=tid)
u = User.objects.get(username="cref")

with open('manual_data/combined all.csv', "r") as csvfile:
    reader = csv.DictReader(csvfile,delimiter=';')
    for row in reader:
        #print(row)

        if len(row["wosarticle__ti"]) > 0:
            try:
                doc = Doc.objects.get(
                    query=q,
                    title=row["wosarticle__ti"],
                    PY=int(row["wosarticle__py"])#,
                    #duplicated=False
                )
                do = DocOwnership.objects.get(
                    tag=t,
                    query=q,
                    user=u,
                    doc=doc
                )
                r = int(row["To include"])
                if r ==0:
                    r = 2
                do.relevant = r
                do.save()
            except:
                print("nodoc!")
                print(row["wosarticle__ti"])
                print(row["To include"])
