# -*- coding: utf-8 -*-

#######################################################################
# Copyright (C) 2020 Hannah Mülbaier & Vinh Tran
#
#  This file is part of phylosophy.
#
#  phylosophy is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  phylosophy is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with phylosophy.  If not, see <http://www.gnu.org/licenses/>.
#
#  Contact: hannah.muelbaier@gmail.com or tran@bio.uni-frankfurt.de
#
#######################################################################

import sys
from Bio import SeqIO
import os
from pyfaidx import Fasta
import time
import json
import argparse
import subprocess
from pathlib import Path
import errno

def subprocess_cmd(commands):
    for cmd in commands:
        subprocess.call(cmd, shell = True)

def checkFileExist(file):
    try:
        my_abs_path = Path(file).resolve(strict=True)
    except FileNotFoundError:
        sys.exit("%s not found" % file)

def checkFileEmpty(file):
    flag = 0
    try:
        if os.path.getsize(file) == 0:
            flag = 1
    except OSError as e:
            flag = 1
    return(flag)

def is_tool(name):
	try:
		devnull = open(os.devnull)
		subprocess.Popen([name], stdout=devnull, stderr=devnull).communicate()
	except OSError as e:
		if e.errno == errno.ENOENT:
			print('\x1b[6;30;42m' + '*** tool \'' + name + '\' not found"' + '\x1b[0m')
			return False
	return True

def openFileToRead(location):
    file = open(location, "r")
    return file

def openFileToWrite(location):
    file = open(location, "w")
    return file

def openFileToAppend(location):
    file = open(location, "a+")
    return file

def makeOneSeqSpeciesName(code,TaxId):
    name = code + "@" + TaxId + "@" + "2"
    return name

def createHeaderCoreFasta(protId, speciesHeader, omaGroupId):
    header = str(omaGroupId) + "|" + speciesHeader + "|" + protId[0:10]
    return(header)

# get gene set and save to genome_dir
# NOTE: speciesCode and speciesTaxId are lists, not single string
def getGeneset(dataPath, speciesCode, speciesTaxId, outPath):
    Path(outPath).mkdir(parents = True, exist_ok = True)
    Path(outPath+"/genome_dir").mkdir(parents = True, exist_ok = True)

    toDo = []
    with open(dataPath + "/oma-seqs-dic.fa") as f:
        sequence_dic = json.load(f)

    for i in range(0,len(speciesCode)):
        name = makeOneSeqSpeciesName(speciesCode[i], speciesTaxId[i])
        Path(outPath+"/genome_dir/"+name).mkdir(parents = True, exist_ok = True)
        if not Path(outPath+"/genome_dir/"+name+"/"+name+".fa").exists():
            toDo.append(i)
        else:
            print(("Gene set for %s found " % speciesCode[i]).format(err))

    if toDo != []:
        allProteins = openFileToRead(dataPath + "/oma-seqs.fa")
        allProteinsLines = allProteins.readlines()
        allProteins.close()

    for j in range(0,len(toDo)):
        name = makeOneSeqSpeciesName(speciesCode[toDo[j]], speciesTaxId[toDo[j]])
        newFile = openFileToWrite(outPath + "/genome_dir/" + name + "/" + name + ".fa")
        startLine = sequence_dic[speciesCode[toDo[j]]][0]
        endLine = sequence_dic[speciesCode[toDo[j]]][1]

        for z in range(startLine, endLine + 1):
            if allProteinsLines[z] == allProteinsLines[startLine]:
                newLine = allProteinsLines[z].replace(" ", "")
                newFile.write(newLine)
            elif allProteinsLines[z][0] != ">":
                newLine = allProteinsLines[z].replace("\n", "")
                newLine = newLine.replace(" ", "")
                newFile.write(newLine)
            else:
                newLine = allProteinsLines[z].replace(" ", "")
                newFile.write("\n" + newLine)
        newFile.close()

def getOGseq(args):
    (proteinIds, omaGroupId, outPath, allFasta) = args
    ogFasta = outPath + "/core_orthologs/" + omaGroupId + "/" + omaGroupId
    flag = 1
    if Path(ogFasta + ".fa").exists():
        tmp = SeqIO.to_dict(SeqIO.parse(open(ogFasta + ".fa"),'fasta'))
        if len(tmp) == len(proteinIds):
            flag = 0
    if flag == 1:
        with open(ogFasta + ".fa", "w") as myfile:
            for protId in proteinIds:
                spec = protId[0:5]
                try:
                    seq = str(allFasta[spec][protId].seq)
                    myfile.write(">" + omaGroupId + "|" + spec + "|" + protId + "\n" + seq + "\n")
                except:
                    print(("%s not found in %s gene set" % (protId, spec)).format(err))


def runBlast(args):
    (specName, specFile, outPath) = args
    blastCmd = 'makeblastdb -dbtype prot -in %s -out %s/blast_dir/%s/%s' % (specFile, outPath, specName, specName)
    subprocess.call([blastCmd], shell = True)
    fileInGenome = "%s/genome_dir/%s/%s.fa" % (outPath, specName, specName)
    fileInBlast = "%s/blast_dir/%s/%s.fa" % (outPath, specName, specName)
    if not Path(fileInBlast).exists():
        lnCmd = 'ln -fs %s %s' % (fileInGenome, fileInBlast)
        subprocess.call([lnCmd], shell = True)

def runHmm(args):
    (hmmFile, fastaFile, id) = args
    hmmCmd = 'hmmbuild --amino -o %s.tmp %s  %s.aln' % (id, hmmFile, fastaFile)
    subprocess.call([hmmCmd], shell = True)
    subprocess.call(['rm ' + id + '.tmp'], shell = True)
    print(id + ".hmm")

# NOTE: fastaFile MUST exclude the extension (i.e. without .fa, .fasta,...)
def runMsa(args):
    (fastaFile, aligTool, id) = args
    if aligTool == "mafft":
        alignCmd = 'mafft --quiet --localpair --maxiterate 1000 %s.fa > %s.aln' % (fastaFile, fastaFile)
    elif aligTool == "muscle":
        alignCmd = 'muscle -quiet -in %s.fa -out %s.aln' % (fastaFile, fastaFile)
    else:
        sys.exit("Invalid alignment tool given!".format(err))
    if not Path(fastaFile + ".aln").exists():
        subprocess.call([alignCmd], shell = True)
    print(id + ".aln")

def calcAnnoFas(args):
	(specName, specFile, outPath) = args
	annoCmd = 'annoFAS --fasta %s --path %s/weight_dir --name %s' % (specFile, outPath, specName) #  --cores 4
	subprocess.call([annoCmd], shell = True)
