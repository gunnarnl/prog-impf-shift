import re
import os
import datetime

#Regex matches verbs tagged with VAG after present tense BE.
regxProg1 = '(BE.)\n([a-zA-Z\-]+)/VAG'
regxProg = '(BE.)\n(?:.*(?:ADV|CONJ|VAG)\n)*(.*)/VAG'
regxImpf = '(.*)/VBP'

#Third param is the file being written to.
def writeProgToken(fileloc, filename, string, wfile):
    pattern = r'('+string+')'
    regex = re.compile(pattern)
    f = open(fileloc, 'r')
    f2 = open(wfile, 'a')
    for match in regex.finditer(f.read()):
        f2.write("{}, {}, {}\n".format(filename, match.group(3), match.group(2)))
    f.close()
    f2.close()

def writeImpfToken(fileloc, filename, string, wfile):
    pattern = r'('+string+')'
    regex = re.compile(pattern)
    f = open(fileloc, 'r')
    f2 = open(wfile, 'a')
    for match in regex.finditer(f.read()):
        f2.write("{}, {}\n".format(filename, match.group(2)))
    f.close()
    f2.close()

#This guy will operate over a number of folders/files. Very useful for heavily segmented corpora.
#folder is the folder you're searching through, fextension is the file extension, fxn is whatever search function above being used and the args (minus the file arg).
#e.g. multiFileHelper("My Corpus", ".cha", writeReLoc, "themselves", "themselves-locations.txt")

def multiFileHelper(folder, fextension, fxn, *fargs):
    for root, dirs, files in os.walk(folder):
        for file in files:
            if file.endswith(fextension):
                fxn(os.path.join(root, file), file, *fargs)

writefilenameProg = "prog-tokens"+datetime.datetime.now().strftime("%Y%m%d%H%M")+".csv"
writefilenameImpf = "impf-tokens"+datetime.datetime.now().strftime("%Y%m%d%H%M")+".csv"
multiFileHelper("D:/Penn Historical Corpora v3.1/PPCHE/PENN-CORPORA/corpora", ".pos", writeProgToken, regxProg, writefilenameProg)
multiFileHelper("D:/Penn Historical Corpora v3.1/PPCHE/PENN-CORPORA/corpora", ".pos", writeImpfToken, regxImpf, writefilenameImpf)
