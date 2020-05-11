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
from pathlib import Path

def checkFileExist(file):
	try:
		my_abs_path = Path(file).resolve(strict=True)
	except FileNotFoundError:
		sys.exit("%s not found" % file)

def openFileToRead(location):
    #opens a file in mode read
    file = open(location, "r")
    return file

def openFileToWrite(location):
    # opens a file in mode write
    file = open(location, "w")
    return file

# def openFileToAppend(location):
#     #opens a file in mode append
#     file = open(location, "a+")
#     return file

def makeOneSeqSpeciesName(code,TaxId):
    # creates the file name for the dataset of the species
    name = code + "@" + TaxId + "@" + "2"
    return name


def getSequence(allProteins, speciesCode, newFile, name):
    check = False
    for record in allProteins:
        codeAllProteins = record.id[0:5]
        if codeAllProteins == speciesCode:
            check = True
            newFile.write(">" + str(record.id) + "\n")
            newFile.write(str(record.seq) + "\n")
        elif check == True:
            newFile.close()
            print("saved " + name)
            break


def getDataset(dataPath, speciesCode, speciesTaxId, outPath):
    Path(outPath).mkdir(parents = True, exist_ok = True)
    Path(outPath+"/genome_dir").mkdir(parents = True, exist_ok = True)

    start = time.time()
    toDo = []

    with open(dataPath + "/oma-seqs-dic.fa") as f:
        sequence_dic = json.load(f)

    for i in range(0,len(speciesCode)):
        name = makeOneSeqSpeciesName(speciesCode[i], speciesTaxId[i])
        try:
            os.mkdir(outPath + "/genome_dir/" + name)
            toDo.append(i)
        except FileExistsError:
            print("File exists already for species " + speciesCode[i])

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
    ende = time.time()
    print('{:5.3f}s'.format(ende - start), end='  ')

def main():
    version = "1.0.0"
    parser = argparse.ArgumentParser(description="You are running getGenomes version " + str(version) + ".")
    required = parser.add_argument_group('required arguments')
    required.add_argument('-n', '--name', help='List of OMA species abbr. names', action='store', default='', required=True)
    required.add_argument('-i', '--id', help='List of corresponding taxonomy IDs to OMA species', action='store', default='', required=True)
    required.add_argument('-d', '--dataPath', help='Path to OMA Browser data', action='store', default='', required=True)
    required.add_argument('-o', '--outPath', help='Path to output directory', action='store', default='', required=True)
    args = parser.parse_args()

    checkFileExist(args.dataPath)
    checkFileExist(args.dataPath+"/oma-seqs-dic.fa")
    dataPath = str(Path(args.dataPath).resolve())
    speciesCode = str(args.name).split(",")
    speciesTaxId = str(args.id).split(",")
    outPath = str(Path(args.outPath).resolve())

    getDataset(dataPath, speciesCode, speciesTaxId, outPath)


if __name__ == '__main__':
    main()
