resfiles = []
with open("results.tsv") as results:
    for line in results:
        fields = line.split("\t")
        if len(fields) > 1:
            fields[3] = fields[3].replace("/","_")
            if fields[4] == "not found":
                if fields[3] in resfiles:
                    ftype="a"
                else:
                    ftype="w"
                    resfiles.append(fields[3])
                ti = fields[2].split(".")[0]
                if len(ti)==0:
                    continue
                au = fields[0].split(",")[0]
                for source in ["wos","scopus"]:
                    with open("queries/{}_{}.txt".format(fields[3],source),ftype) as w:
                        if ftype =="a":
                            w.write(' OR ')
                        if source=="wos":
                            w.write('(TI=("{}") AND AU=("{}") AND PY=({}))'.format(
                                ti ,au, fields[1]
                            ))
                        else:
                            w.write('(TITLE("{}") AND AUTH("{}") AND PUBYEAR({}))'.format(
                                ti ,au, fields[1]
                            ))
