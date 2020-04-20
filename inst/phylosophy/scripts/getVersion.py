import coreapi
import sys

dataPath = sys.argv[1]

client = coreapi.Client()
schema = client.get("https://omabrowser.org/api/docs")

action = ["version", "list"]
result = client.action(schema, action)["oma_version"]

file = open(dataPath + "/oma-groups.txt", "r")

line = file.readline()

version = (line.split(" ")[-1])[:-1]

if result == version:
    print("Oma version: " + version +  ". Your OmaDB files are up to date!")

else:
    print("Oma version: " + version +  ". Your OmaDB files are outdated, please update them. You can download them under: https://omabrowser.org/oma/current/")
