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

import sys
import os
import subprocess
import multiprocessing as mp

def calcAnnoFas(args):
	(species, path) = args
	fasta = path + "/genome_dir/" + species +  "/" + species + ".fa"
	annoCmd = 'annoFAS --fasta %s --path %s/weight_dir --name %s --cores 4' % (fasta, path, species)
	subprocess.call([annoCmd], shell = True)

def gettingId(line):
	id = line.replace("\n", "")
	return id

def main():
	parameter = sys.argv[1:]
	path = parameter[0]
	speciesFile = open("tmp/species.txt", "r")
	annoJobs = []
	for line in speciesFile:
		annoJobs.append([line.rstrip(), path])

	pool = mp.Pool(mp.cpu_count())
	anno = pool.map(calcAnnoFas, annoJobs)
	pool.close()

if __name__ == '__main__':
	main()
