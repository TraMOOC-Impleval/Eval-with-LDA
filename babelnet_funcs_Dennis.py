import urllib.request
import urllib.parse
import json
import gzip
import sys

from io import BytesIO

def findTranslations(word, sourcelang, targetlang, maxcost, key = '021704ef-d4d4-48df-9560-8a91618ba6b8'):
    cost = 0
    translations = []
    params = {
        'word': word,
        'langs': sourcelang,
        'key': key
    }

    synset_ids = getSynsetIDs(params)
    cost += 1

    for i, synset_specs in enumerate(synset_ids):
        ID = synset_specs['id']

        params2 = {
            'id': ID,
            'filterLangs': targetlang,
            'key': key
        }

        synset_info = getInformation(params2)
        cost += 1

        lemmas = getLemmas(synset_info, ID, targetlang)

        translations = translations + lemmas

        if cost >= maxcost:
            return translations, cost

    return translations, cost

def getSynsetIDs(params):
    service_url = 'https://babelnet.io/v4/getSynsetIds'
    url = service_url + '?' + urllib.parse.urlencode(params)
    request = urllib.request.Request(url)
    request.add_header('Accept-encoding', 'gzip')
    response = urllib.request.urlopen(request)
    
    if response.info().get('Content-Encoding') == 'gzip':

            buf = BytesIO( response.read())
            f = gzip.GzipFile(fileobj=buf)
            data = json.loads(f.read().decode('utf-8'))
            return data

def getInformation(params):
    service_url = 'https://babelnet.io/v4/getSynset'
    url = service_url + '?' + urllib.parse.urlencode(params)
    request = urllib.request.Request(url)
    request.add_header('Accept-encoding', 'gzip')
    response = urllib.request.urlopen(request)

    if response.info().get('Content-Encoding') == 'gzip':

        buf = BytesIO( response.read())
        f = gzip.GzipFile(fileobj=buf)
        data = json.loads(f.read().decode('utf-8'))
        return data

def getLemmas(data, ID, targetlang):
    wordlist = []
    senses = data['senses']

    for sense in senses:
        if sense['language'] == targetlang:

            item = (ID, sense['lemma'], sense['pos'])
            wordlist.append(item)

    return wordlist

sys.argv[4] = int(sys.argv[4])
translations, cost = findTranslations(word = sys.argv[1], sourcelang = sys.argv[2], targetlang = sys.argv[3], maxcost = sys.argv[4])

for item in translations:
    print(item)
