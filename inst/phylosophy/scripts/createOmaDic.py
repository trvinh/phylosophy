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

from Bio import SeqIO
import time
import json
import sys
import subprocess
from pathlib import Path
import argparse


def subprocess_cmd(commands):
    for cmd in commands:
        subprocess.call(cmd, shell = True)


def downloadFiles(dataPath, force):
    mainUrl = "https://omabrowser.org/All/"
    omagroup = "oma-groups.txt.gz"
    fastaseq = "oma-seqs.fa.gz"
    specinfo = "oma-species.txt"
    Path(dataPath).mkdir(parents = True, exist_ok = True)

    if not (Path(dataPath + "/oma-groups.txt").exists() or force):
        print("############ Downloading oma-groups.txt...")
        try:
            downloadCmd = 'wget %s/%s -P %s' % (mainUrl, omagroup, dataPath)
            unzipCmd = 'gunzip %s/%s' % (dataPath, omagroup)
            subprocess_cmd([downloadCmd, unzipCmd])
        except:
            sys.exit("Error occurs while downloading %s/%s" % (mainUrl, omagroup))

    if not (Path(dataPath + "/oma-seqs.fa").exists() or force):
        print("############ Downloading oma-seqs.fa...")
        try:
            downloadCmd = 'wget %s/%s -P %s' % (mainUrl, fastaseq, dataPath)
            unzipCmd = 'gunzip %s/%s' % (dataPath, fastaseq)
            subprocess_cmd([downloadCmd, unzipCmd])
        except:
            sys.exit("Error occurs while downloading %s/%s" % (mainUrl, fastaseq))

    if not (Path(dataPath + "/oma-species.txt").exists() or force):
        print("############ Downloading oma-species.txt...")
        try:
            downloadCmd = 'wget %s/%s -P %s' % (mainUrl, specinfo, dataPath)
            subprocess_cmd([downloadCmd])
        except:
            sys.exit("Error occurs while downloading %s/%s" % (mainUrl, specinfo))

def createDicSpecies(proteins, file):
    start = time.time()
    sequenceDic = {}
    code = str(proteins.readline()[2:7])
    startline = 0
    lineNr = 0

    for i in proteins:
        lineNr += 1
        if code != i[2:7] and i[0] == ">":
            endline = lineNr - 1
            sequenceDic[code] = (startline,endline)
            code = i[2:7]
            startline = lineNr

    sequenceDic[code] = (startline,lineNr)
    json.dump(sequenceDic, file)
    ende = time.time()
    print('{:5.3f}s'.format(ende - start), end='  ')

def createDicOmaGroup(omaGroups, file):
    start = time.time()
    groupDic = {}
    for i in omaGroups:
        if i[0] != "#":
            line = i.split("\t")
            speciesSet = set()
            groupId = line[0]
            for j in range(2, len(line)):
                species = str(line[j])[0:5]
                speciesSet.add(species)
            groupDic[groupId] = tuple(speciesSet)

    for key in groupDic:
        speciesStr = str(groupDic[key]).replace("(", "")
        speciesStr = speciesStr.replace(")", "")
        speciesStr = speciesStr.replace("'", "")
        speciesStr = speciesStr.replace(" ", "")
        file.write(key + "\t" + speciesStr + "\n")
    ende = time.time()
    print('{:5.3f}s'.format(ende - start), end='  ')

def main():
    version = "1.0.0"
    parser = argparse.ArgumentParser(description="You are running createOmaDic version " + str(version) + ".")
    required = parser.add_argument_group('required arguments')
    optional = parser.add_argument_group('additional arguments')
    required.add_argument('-o', '--outPath', help='Directory for saving OMA Browser data', action='store', default='', required=True)
    optional.add_argument('-f', '--force', help='Force override old data', action='store_true')
    args = parser.parse_args()

    dataPath = args.outPath
    force = args.force

    downloadFiles(dataPath, force)

    allProteins = open(dataPath + "/oma-seqs.fa", "r")
    newFileSpecies = open(dataPath + "/oma-seqs-dic.fa", "w")
    omaGroups = open(dataPath + "/oma-groups.txt", "r")
    newFileOmaGroup = open(dataPath + "/oma-groups-tmp.txt", "w")

    print("############ Indexing proteins...")
    createDicSpecies(allProteins, newFileSpecies)
    print("############ Indexing OMA groups...")
    createDicOmaGroup(omaGroups, newFileOmaGroup)

    newFileSpecies.close()
    newFileOmaGroup.close()
    allProteins.close()
    omaGroups.close()
    print("Finished! All output files saved in %s" % dataPath)

if __name__ == '__main__':
    main()
