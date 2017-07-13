import re, sys, os
import django

# import file for easy access to browser database
sys.path.append('/home/galm/software/tmv/BasicBrowser/')

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from scoping.models import *
fnames = ["au","py","ti","section","db","full-text","found"]

queries = Query.objects.filter(id__gt=1416)
alldocs = Doc.objects.filter(query__in=queries)

tagpat = re.compile(r'<.*?>')

with open("reviewed_results.tsv","w") as new_results:
    new_results.write("{}\t{}\t{}\t{}\t{}\t{}\t{}\n".format(*fnames))
    with open("results.tsv") as results:
        for line in results:
            fields = line.split("\t")
            if len(fields) > 1:
                if fields[0] == "au":
                    continue
                fields[3] = fields[3].replace("/","_")
                if fields[4] == "not found": #look
                    au = fields[0]
                    py = fields[1]
                    ti = fields[2]
                    try:
                        docs = alldocs.filter(
                            docauthinst__AU__icontains=au.split()[0],
                            PY=py,
                            title__icontains=ti.split()[0]
                        ).filter(title__icontains=ti.split()[1]).filter(
                            title__icontains=ti.split()[2]
                        ).distinct()
                    except:
                        docs = alldocs.filter(UT="blabla")
                    if docs.count()>0:
                        loc = "https://apsis.mcc-berlin.net/scoping/document/"+docs.first().UT
                        fstage = 2
                    else:
                        loc = "not found"
                        fstage = 0
                else: #
                    loc = fields[4]
                    fstage = 1

                fields[4] = loc
                fields[5] = tagpat.sub("",fields[5]).strip()
                fields.append(fstage)
                new_results.write("{}\t{}\t{}\t{}\t{}\t{}\t{}\n".format(*fields))
