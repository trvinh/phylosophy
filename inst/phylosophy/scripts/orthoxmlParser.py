# -*- coding: utf-8 -*-

#######################################################################
# Copyright (C) 2020 Vinh Tran
#
# This file is part of phylosophy.
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
#######################################################################

import os
import sys
import argparse
import time
from bs4 import BeautifulSoup
from pathlib import Path
import subprocess
from Bio import SeqIO
import multiprocessing as mp
import dccFn

def readFileToDict(file):
    name2id = {}
    name2abbr = {}
    with open(file, 'r') as f:
        for line in f:
            fields = line.rstrip().split('\t')
            name2id[fields[1]] = fields[0]
            name2abbr[fields[1]] = fields[2]
    return(name2id, name2abbr)

def concatFasta(fileIn, fileOut):
     cmd = "awk \'/^>/ { print (NR==1 ? \"\" : RS) $0; next } { printf \"%s\", $0 } END { printf RS }\' " + fileIn + " > " + fileOut
     subprocess.call([cmd], shell = True)

def checkFileExist(file):
    try:
        my_abs_path = Path(file).resolve(strict=True)
    except FileNotFoundError:
        sys.exit("%s not found" % file)

# def runBlast(args):
#     (specName, specFile, outFol) = args
#     blastCmd = 'makeblastdb -dbtype prot -in %s -out %s/blast_dir/%s/%s' % (specFile, outFol, specName, specName)
#     subprocess.call([blastCmd], shell = True)
#
# def runHmm(args):
#     (hmmFile, fastaFile, id) = args
#     hmmCmd = 'hmmbuild --amino -o %s.tmp %s  %s.aln' % (id, hmmFile, fastaFile)
#     subprocess.call([hmmCmd], shell = True)
#     subprocess.call(['rm ' + id + '.tmp'], shell = True)
#     print(id + ".hmm")
#
# def runMsa(args):
#     (fastaFile, aligTool, id) = args
#     if aligTool == "mafft":
#         alignCmd = 'mafft --quiet --localpair --maxiterate 1000 %s.fa > %s.aln' % (fastaFile, fastaFile)
#     elif aligTool == "muscle":
#         alignCmd = 'muscle -quiet -in %s.fa -out %s.aln' % (fastaFile, fastaFile)
#     else:
#         sys.exit("Invalid alignment tool given!")
#     if not Path(fastaFile + ".aln").exists():
#         subprocess.call([alignCmd], shell = True)
#     print(id + ".aln")

def main():
    version = "1.0.0"
    parser = argparse.ArgumentParser(description="You are running orthoxmlParser version " + str(version) + ".")
    required = parser.add_argument_group('required arguments')
    optional = parser.add_argument_group('additional arguments')
    required.add_argument('-i', '--inFile', help='Input sequence in orthoXML format', action='store', default='', required=True)
    required.add_argument('-o', '--outPath', help='Output directory', action='store', default='', required=True)
    required.add_argument('-g', '--geneSet', help='Path to gene set folder', action='store', default='', required=True)
    required.add_argument('-m', '--mappingFile', help='NCBI taxon ID mapping file', action='store', default='', required=True)
    optional.add_argument('-a', '--alignTool', help='Alignment tool (mafft|muscle). Default: mafft', action='store', default='mafft')
    optional.add_argument('-l', '--maxGroups', help='Maximum ortholog groups taken into account.', type=int, action='store', default=999999999)
    args = parser.parse_args()

    inFile = args.inFile
    checkFileExist(inFile)
    dataPath = args.geneSet
    checkFileExist(dataPath)
    mappingFile = args.mappingFile
    checkFileExist(mappingFile)
    outFol = args.outPath
    try:
        my_abs_path = Path(outFol).resolve(strict=True)
    except FileNotFoundError:
        Path(outFol).mkdir(parents = True, exist_ok = True)
    aligTool = args.alignTool.lower()
    if not aligTool == "mafft" or aligTool == "muscle":
        sys.exit("alignment tool must be either mafft or muscle")
    limit = args.maxGroups

    start = time.time()
    pool = mp.Pool(mp.cpu_count())
    ##### read mapping file
    (name2id, name2abbr) = readFileToDict(mappingFile)

    ##### read input file into beatifulsoup object
    print("Reading input XML file...")
    xmlIn = BeautifulSoup(open(inFile),"xml")

    ##### PARSING XML FILE
    ### get list of species together with NCBI taxon IDs and their corresponding genes
    taxonName = {}
    protID = {}
    fasta = {}

    ### create output folders
    Path(outFol + "/genome_dir").mkdir(parents = True, exist_ok = True)
    Path(outFol + "/blast_dir").mkdir(parents = True, exist_ok = True)
    Path(outFol + "/core_orthologs").mkdir(parents = True, exist_ok = True)

    ### copy species to genome_dir, blast_dir and create blastDBs
    print("Getting gene sets...")
    blastJobs = []
    for spec in xmlIn.findAll("species"):
        specNameOri = spec.get("name")
        if not specNameOri in name2abbr:
            sys.exit("%s not found in %s" % (specNameOri, mappingFile))
        specName = "%s@%s@1" % (name2abbr[specNameOri], name2id[specNameOri])
        Path(outFol + "/genome_dir/" + specName).mkdir(parents = True, exist_ok = True)
        Path(outFol + "/blast_dir/" + specName).mkdir(parents = True, exist_ok = True)
        # get gene set file
        lsCmd = 'ls %s/%s.*' % (dataPath, specNameOri)
        specFile = subprocess.check_output([lsCmd], shell = True).decode(sys.stdout.encoding).strip()
        fileExt = specFile.split(".")[-1]
        # read fasta file to dictionary
        fasta[specName] = SeqIO.to_dict(SeqIO.parse(open(specFile),'fasta'))

        # copy to genome_dir/specName/specName.fa and make smybolic link to blast_dir/specName
        fileInGenome = "%s/genome_dir/%s/%s.fa" % (outFol, specName, specName)
        if not Path(fileInGenome).exists():
            concatFasta(specFile, fileInGenome)
        # fileInBlast = "%s/blast_dir/%s/%s.fa" % (outFol, specName, specName)
        # if not Path(fileInBlast).exists():
        #     lnCmd = 'ln -fs %s %s' % (fileInGenome, fileInBlast)
        #     subprocess.call([lnCmd], shell = True)
        # get info for blast
        blastDbFile = "%s/blast_dir/%s/%s.phr" % (outFol, specName, specName)
        if not Path(blastDbFile).exists():
            blastJobs.append([specName, specFile, outFol])

        # save OG members and their spec name to dict
        for gene in spec.findAll("gene"):
            groupID = gene.get("id")
            orthoID = gene.get("protId")
            taxonName[orthoID] = specName
            protID[groupID] = orthoID

    # make blastDB
    print("Creating BLAST databases...")
    msa = pool.map(dccFn.runBlast, blastJobs)

    ### parse ortholog groups
    print("Parsing ortholog groups...")
    alignJobs = []
    hmmJobs = []
    n = 0
    for orthogroup in xmlIn.findAll("orthologGroup"):
        groupID = orthogroup.get("id")
        if groupID:
            n = n + 1
            if (n > limit):
                break
            if groupID.isdigit():
                groupID = "OG_"+str(groupID)
            Path(outFol + "/core_orthologs/" + groupID).mkdir(parents = True, exist_ok = True)

            # get fasta sequences
            with open(outFol + "/core_orthologs/" + groupID + "/" + groupID + ".fa", "w") as myfile:
                for ortho in orthogroup.findAll("geneRef"):
                    orthoID = protID[ortho.get("id")]
                    spec = taxonName[orthoID]
                    orthoSeq = str(fasta[spec][orthoID].seq)
                    myfile.write(">" + groupID + "|" + spec + "|" + orthoID + "\n" + orthoSeq + "\n")

            # get info for MSA
            ogFasta = outFol + "/core_orthologs/" + groupID + "/" + groupID
            alignJobs.append([ogFasta, aligTool, groupID])

            # get info for pHMM
            Path(outFol + "/core_orthologs/" + groupID + "/hmm_dir").mkdir(parents = True, exist_ok = True)
            hmmFile = "%s/core_orthologs/%s/hmm_dir/%s.hmm" % (outFol, groupID, groupID)
            flag = 0
            try:
                if os.path.getsize(hmmFile) == 0:
                    flag = 1
            except OSError as e:
                    flag = 1
            if flag == 1:
                hmmJobs.append([hmmFile, ogFasta, groupID])

    ### create MSAs and pHMMs
    print("Calculating MSAs and pHMMs for %s OGs..." % (len(alignJobs)))
    msa = pool.map(dccFn.runMsa, alignJobs)
    phmm = pool.map(dccFn.runHmm, hmmJobs)
    pool.close()

    ende = time.time()
    print("Finished in " + '{:5.3f}s'.format(ende-start))
    print("Output can be found in %s" % outFol)

if __name__ == "__main__":
    main()
