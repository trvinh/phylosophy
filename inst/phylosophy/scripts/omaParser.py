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
import os
import argparse
from pathlib import Path
import time
from Bio import SeqIO
import multiprocessing as mp
import dccFn

def gettingOmaGroups(dataPath, speciesSet, nr):
    omaGroupFull = dccFn.openFileToRead(dataPath + "/oma-groups.txt")
    omaGroups = {}
    for i in omaGroupFull:
        row = i.split("\t")
        if len(row) != 1:
            ProteinIds = []
            counter = 0
            for j in range(2, len(row)):
                species = row[j][0:5]
                if species in speciesSet:
                    ProteinIds.append(row[j])
                    counter += 1
            if counter >= (len(speciesSet) - int(nr)):
                omaGroups[row[0]] = ProteinIds
    return(omaGroups)

def main():
    version = "1.0.0"
    parser = argparse.ArgumentParser(description="You are running omaParser for OMA Browser version " + str(version) + ".")
    required = parser.add_argument_group('required arguments')
    optional = parser.add_argument_group('additional arguments')
    required.add_argument('-n', '--name', help='List of OMA species abbr. names', action='store', default='', required=True)
    required.add_argument('-i', '--id', help='List of corresponding taxonomy IDs to OMA species', action='store', default='', required=True)
    required.add_argument('-d', '--dataPath', help='Path to OMA Browser data', action='store', default='', required=True)
    required.add_argument('-o', '--outPath', help='Path to output directory', action='store', default='', required=True)
    required.add_argument('-j', '--jobName', help='Job name', action='store', default='', required=True)
    optional.add_argument('-m', '--missingTaxa', help='Number of allowed missing taxa. Default: 0', action='store', default=0)
    optional.add_argument('-a', '--alignTool', help='Alignment tool (mafft|muscle). Default: mafft', action='store', default='mafft')
    optional.add_argument('-f', '--annoFas', help='Perform FAS annotation', action='store_true')
    args = parser.parse_args()

    dccFn.checkFileExist(args.dataPath)
    dccFn.checkFileExist(args.dataPath+"/oma-seqs-dic.fa")
    dataPath = str(Path(args.dataPath).resolve())
    speciesList = str(args.name).split(",")
    speciesTaxId = str(args.id).split(",")
    outPath = str(Path(args.outPath).resolve())
    nrMissingTaxa = args.missingTaxa
    aligTool = args.alignTool.lower()
    if not aligTool == "mafft" or aligTool == "muscle":
        sys.exit("alignment tool must be either mafft or muscle")
    doAnno = args.annoFas
    jobName = args.jobName

    start = time.time()
    pool = mp.Pool(mp.cpu_count()-1)

    ### create output folders
    print("Creating output folders...")
    Path(outPath + "/genome_dir").mkdir(parents = True, exist_ok = True)
    Path(outPath + "/blast_dir").mkdir(parents = True, exist_ok = True)
    Path(outPath + "/core_orthologs/" + jobName).mkdir(parents = True, exist_ok = True)
    Path(outPath + "/weight_dir").mkdir(parents = True, exist_ok = True)

    ### Get genesets
    print("Getting %s gene sets..." % (len(speciesList)))
    dccFn.getGeneset(dataPath, speciesList, speciesTaxId, outPath)

    # read fasta file to dictionary
    fasta = {}
    blastJobs = []
    annoJobs = []
    for i in range(0,len(speciesList)):
        fileName = dccFn.makeOneSeqSpeciesName(speciesList[i], speciesTaxId[i])
        specFile = outPath+"/genome_dir/"+fileName+"/"+fileName+".fa"
        fasta[speciesList[i]] = SeqIO.to_dict(SeqIO.parse(open(specFile),'fasta'))
        # get info for BLAST
        blastDbFile = "%s/blast_dir/%s/%s.phr" % (outPath, fileName, fileName)
        if not Path(blastDbFile).exists():
            blastJobs.append([fileName, specFile, outPath])
        # get info for FAS annotation
        annoPfamFile = "%s/weight_dir/%s/pfam.xml" % (outPath, fileName)
        if not Path(annoPfamFile).exists():
            annoJobs.append([fileName, specFile, outPath])

    ### create blastDBs
    print("Creating BLAST databases for %s taxa..." % len(blastJobs))
    if dccFn.is_tool('makeblastdb'):
        msa = pool.map(dccFn.runBlast, blastJobs)
    else:
        print("makeblastdb not found!")

    ### get OGs and their fasta
    omaGroups = gettingOmaGroups(dataPath, set(speciesList), nrMissingTaxa)
    print("Getting protein sequences for %s OGs..." % len(omaGroups))
    msaJobs = []
    hmmJobs = []
    for omaGroupId in omaGroups:
        # Path(outPath + "/core_orthologs/" + omaGroupId).mkdir(parents = True, exist_ok = True)
        Path(outPath + "/core_orthologs/" + jobName + "/" + omaGroupId + "/hmm_dir").mkdir(parents = True, exist_ok = True)
        # getSeqJobs.append([omaGroups[omaGroupId], omaGroupId, outPath, fasta])  # slower than run sequentially
        dccFn.getOGseq([omaGroups[omaGroupId], omaGroupId, outPath, fasta, jobName])

        ogFasta = outPath + "/core_orthologs/" + jobName + "/" + omaGroupId + "/" + omaGroupId
        ### get MSA jobs
        try:
            msaFile = "%s/core_orthologs/%s/%s/%s.aln" % (outPath, jobName, omaGroupId, omaGroupId)
            flag = dccFn.checkFileEmpty(msaFile)
            if flag == 1:
                msaJobs.append([ogFasta, aligTool, omaGroupId])
        except:
            sys.exit("%s not found or %s not works correctly!" % (ogFasta+".fa", aligTool))
        ### get pHMM jobs
        try:
            hmmFile = "%s/core_orthologs/%s/%s/hmm_dir/%s.hmm" % (outPath, jobName, omaGroupId, omaGroupId)
            flag = dccFn.checkFileEmpty(hmmFile)
            if flag == 1:
                hmmJobs.append([hmmFile, ogFasta, omaGroupId])
        except:
            sys.exit("hmmbuild not works correctly for %s!" % (ogFasta+".fa"))

    ### calculate MSAs and pHMMs
    print("Calculating MSAs and pHMMs for %s OGs..." % len(omaGroups))
    if len(msaJobs) > 0:
        msa = pool.map(dccFn.runMsa, msaJobs)
    if len(hmmJobs) > 0:
        if dccFn.is_tool('hmmbuild'):
            hamm = pool.map(dccFn.runHmm, hmmJobs)
        else:
            print("hmmbuild not found!")

    ### do FAS annotation
    if doAnno:
        print("Doing FAS annotation...")
        if dccFn.is_tool('annoFAS'):
            anno = pool.map(dccFn.calcAnnoFas, annoJobs)

    pool.close()
    ende = time.time()
    print("Finished in " + '{:5.3f}s'.format(ende-start))
    print("Output can be found in %s" % outPath)

if __name__ == '__main__':
    main()
