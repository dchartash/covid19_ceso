#!/usr/bin/python3
import requests
from bs4 import BeautifulSoup

data_uri = "https://data.ontario.ca/dataset/confirmed-positive-cases-of-covid-19-in-ontario"
r = requests.get(data_uri)
r_soup = BeautifulSoup(r.content,'lxml-html')
href = r_soup.find("a",{"class":"resource-url-analytics btn btn-primary"})

r = requests.get(href['href'])
f = open("data/conposcovidloc.csv","w")
f.write(r.text)
f.close()
