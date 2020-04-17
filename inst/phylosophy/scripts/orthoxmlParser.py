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
import getopt
import glob
import time
from bs4 import BeautifulSoup
from pathlib import Path
import subprocess
from Bio import SeqIO

def appendToDict(key,value,aDict):
	if not key in aDict.keys():
		aDict[key] = []
		aDict[key].append(value)
	else:
		if not value in aDict[key]:
			aDict[key].append(value)

def main(argv):
	inFile = ''
	try:
		opts, args = getopt.getopt(argv,"i:g:a:o:h",["inFile", "genesets", "tool", "outPath","help"])
	except getopt.GetoptError:
		print('orthoxmlParser.py -i input -g path_to_genesets -a mafft -o output_path')
		sys.exit(2)

	for opt,arg in opts:
		if opt in ('-h','--help'):
			print('orthoxmlParser.py -i <orthoxml file> -g <absolute path to genesets> -a <alignment tool (mafft|muscle)> -o <output directory>')
			sys.exit()
		elif opt in ('-i','--inFile'):
			inFile = arg
			try:
			    my_abs_path = Path(inFile).resolve(strict=True)
			except FileNotFoundError:
				sys.exit("%s not found" % inFile)
		elif opt in ('-g','--genesets'):
			dataPath = arg
			try:
			    my_abs_path = Path(dataPath).resolve(strict=True)
			except FileNotFoundError:
				sys.exit("%s not found" % dataPath)
		elif opt in ('-a','--tool'):
			aligTool = arg
			if not aligTool == "mafft" or aligTool == "muscle":
				sys.exit("alignment tool must be either mafft or muscle")
		elif opt in ('-o','--outPath'):
			outFol = arg
			try:
			    my_abs_path = Path(outFol).resolve(strict=True)
			except FileNotFoundError:
				sys.exit("%s not found" % outFol)

	##### read file into beatifulsoup object
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
	print("Getting gene sets and creating BLAST databases...")
	for spec in xmlIn.findAll("species"):
		specName = spec.get("name")
		Path(outFol + "/genome_dir/" + specName).mkdir(parents = True, exist_ok = True)
		Path(outFol + "/blast_dir/" + specName).mkdir(parents = True, exist_ok = True)
		# get gene set file
		lsCmd = 'ls %s/%s.*' % (dataPath, specName)
		specFile = subprocess.check_output([lsCmd], shell = True).decode(sys.stdout.encoding).strip()
		fileExt = specFile.split(".")[-1]
		# read fasta file to dictionary
		fasta[specName] = SeqIO.to_dict(SeqIO.parse(open(specFile),'fasta'))

		# copy to genome_dir/specName and blast_dir/specName
		fileInGenome = "%s/blast_dir/%s/%s.%s" % (outFol, specName, specName, fileExt)
		if not Path(fileInGenome).exists():
			subprocess.call(['rsync', specFile, outFol + "/blast_dir/" + specName])
		fileInBlast = "%s/genome_dir/%s/%s.%s" % (outFol, specName, specName, fileExt)
		if not Path(fileInBlast).exists():
			subprocess.call(['rsync', specFile, outFol + "/genome_dir/" + specName])
		# make blastDB and save in blast_dir/specName
		blastDbFile = "%s/blast_dir/%s/%s.phr" % (outFol, specName, specName)
		if not Path(blastDbFile).exists():
			blastCmd = 'makeblastdb -dbtype prot -in %s -out %s/blast_dir/%s/%s' % (specFile, outFol, specName, specName)
			subprocess.call([blastCmd], shell = True)
		print("*** " + specName)

		for gene in spec.findAll("gene"):		# <gene id="1" protid="NP_050056.1|ATP synthase F0 subunit 8"></gene>
			groupID = gene.get("id")
			orthoID = gene.get("protId")# .split('|')[0]		# protid="NP_050057.1|NADH dehydrogenase subunit 5"
			taxonName[orthoID] = specName
			protID[groupID] = orthoID

	### parse ortholog groups, create MSA and pHMMs
	print("Calculating alignment and pHMM for OGs...")
	for orthogroup in xmlIn.findAll("orthologGroup"):
		groupID = orthogroup.get("id")
		if groupID:
			if groupID.isdigit():
				groupID = "OG_"+str(groupID)
			Path(outFol + "/core_orthologs/" + groupID).mkdir(parents = True, exist_ok = True)
			print("*** " + groupID)

			# get fasta sequences
			with open(outFol + "/core_orthologs/" + groupID + "/" + groupID + ".fa", "w") as myfile:
				for ortho in orthogroup.findAll("geneRef"):
					orthoID = protID[ortho.get("id")]
					spec = taxonName[orthoID]
					orthoSeq = str(fasta[spec][orthoID].seq)
					myfile.write(">" + groupID + "|" + specName + "|" + orthoID + "\n" + orthoSeq + "\n")

			# do MSA
			ogFasta = outFol + "/core_orthologs/" + groupID + "/" + groupID
			if aligTool == "mafft":
				alignCmd = 'mafft --quiet --localpair --maxiterate 1000 %s.fa > %s.aln' % (ogFasta, ogFasta)
			elif aligTool == "muscle":
				alignCmd = 'muscle -quiet -in %s.fa -out %s.aln' % (ogFasta, ogFasta)
			else:
				sys.exit("Invalid alignment tool given!")
			if not Path(ogFasta + ".aln").exists():
				subprocess.call([alignCmd], shell = True)

			# do pHMM
			Path(outFol + "/core_orthologs/" + groupID + "/hmm_dir").mkdir(parents = True, exist_ok = True)
			hmmFile = "%s/core_orthologs/%s/hmm_dir/%s.hmm" % (outFol, groupID, groupID)
			if os.stat(hmmFile).st_size == 0:
				hmmCmd = 'hmmbuild --amino -o hmmbuild.tmp %s  %s.aln' % (hmmFile, ogFasta)
				subprocess.call([hmmCmd], shell = True)
				subprocess.call(['rm hmmbuild.tmp'], shell = True)
			exit(groupID)

if __name__ == "__main__":
	if len(sys.argv[1:]) < 8:
		print('orthoxmlParser.py -i input -g path_to_genesets -a mafft -o output_path')
		sys.exit(2)
	else:
		main(sys.argv[1:])
