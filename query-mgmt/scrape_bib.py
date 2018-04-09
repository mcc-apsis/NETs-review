import re, sys, os
import django

# import file for easy access to browser database
sys.path.append('/home/galm/software/tmv/BasicBrowser/')

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from scoping.models import *

fpath = "Bibliography for GGR 2014-16 (v2) 20 Oct 2016s.html"

section = None

tagpat = re.compile(r'<.*?>')
refpat = "(.*)\(([2|1]{1}[0-9]{3})\) *(?:<i>)*(.*)"

alldocs = Doc.objects.filter(
    technology__isnull=False
) | Doc.objects.filter(query__technology__isnull=False)



with open(fpath,"r") as f:
    with open("results.tsv","w") as w:
        w.write("{}\t{}\t{}\t{}\t{}\t{}\n".format("au","py","ti","section","db","full-text"))
        for line in f:
            lines = re.split('<br/>',line)
            for l in lines:
                l = l.replace("&#160;"," ")
                if "<b>" in l:
                    if "GENERAL CONSIDERATIONS" in l:
                        section = "GENERAL"
                        continue
                    if "<b>Bioenergy with carbon capture and storage (BECCS)</b>" in l:
                        section = "BECCS"
                    if "<b>2.2    Micro-algal biofuels" in l:
                        section = "Micro-algal biofuels"
                    if "<b>Afforestation and reforestation" in l:
                        section = "Afforestation and Reforestation"
                    if "<b> Soils and biochar </b>" in l:
                        section = "Soils/Biochar"
                    if "<b>Enhanced weathering and ocean alkalinisation" in l:
                        section = "Enhanced weathering/ocean alkalinisation"
                    if "<b>Ocean fertilization and enhanced upwel" in l:
                        section = "Ocean fertilization/Enhanced upwelling"
                    if "<b>2.7   Direct air capture" in l:
                        section = "DAC"
                    if "<b>Methane and other non-CO2 greenhouse" in l:
                        section = "Methane and other non-CO2 greenhouse gases"
                    if "<b>LONG-TERM CO2 STORAGE" in l:
                        section = "Long-term CO2 storage"
                    if "<b>General considerations" in l:
                        section = "Socio-economic/general considerations"
                    if "<b>Ethics, framing and discourse" in l:
                        section = "Ethics, framing and discourse analysis"
                    if "<b>Public perceptions" in l:
                        section = "Public perceptions"
                    if "<b>Governance, policy and regulation" in l:
                        section = "Governance, policy and regulation"
                    if "<b>5. CLIMATE CHANGE CONTEXT" in l:
                        section = "Climate change context"
                if section==None:
                    continue
                ref = re.match(refpat,l)
                if ref is not None:
                    au = tagpat.sub("",ref.group(1))
                    py = tagpat.sub("",ref.group(2))
                    ti = tagpat.sub("",ref.group(3).split("<")[0])
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
                        doc = "https://apsis.mcc-berlin.net/scoping/document/"+docs.first().UT
                    else:
                        doc = "not found"
                    if docs.count() > 1:
                        print("\n###################")
                        print(ti)
                        for d in docs:
                            print(d.title)
                    w.write("{}\t{}\t{}\t{}\t{}\t{}\n".format(au,py,ti,section,doc,l))
                #else:
                    #print(l)
