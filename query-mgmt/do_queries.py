import re, sys, os, time, subprocess
import django
from django.utils import timezone

# import file for easy access to browser database
sys.path.append('/home/galm/software/tmv/BasicBrowser/')

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from scoping.models import *

files = os.listdir('queries')
files.sort()
for fn in files:
    print(fn)
    with open('queries/{}'.format(fn),"r") as f:
        text=f.read().replace('\n', '')
    if "wos" in fn:
        db = "WoS"
    else:
        db = "scopus"

    ti = fn.split('.')[0]
    q = Query(
        title=ti,
        type="default",
        text=text,
        creator = User.objects.get(username="galm"),
        date = timezone.now(),
        database = db
    )

    q.save()

    fname = "/queries/"+str(q.id)+".txt"
    with open(fname,encoding='utf-8',mode="w") as qfile:
        qfile.write(text.encode("utf-8").decode("utf-8"))

    time.sleep(1)

    subprocess.Popen([
        "python3",
        "/home/galm/software/scrapewos/bin/scrapeQuery.py",
        "-s", db,
        fname
    ]).wait()

    if db=="scopus":
        upload = '/home/galm/software/tmv/BasicBrowser/upload_scopus_docs.py'
    else:
        upload = '/home/galm/software/tmv/BasicBrowser/upload_docs.py'

    subprocess.Popen([
        "python3",
        upload,
        str(q.id)
    ]).wait()
