from urllib import request
import json
import os
from base64 import b64encode
import logging

from datetime import datetime
lvvg = 9021014004663000
hvsnedre = 9021014003235000
authKey = os.getenv('VASTTRAFIKKEY')
authSecret = os.getenv('VASTTRAFIKSECRET')

authToken = b64encode("{k}:{s}".format(k=authKey,s=authSecret).encode('utf8')).decode('utf8')

authData = b"grant_type=client_credentials"
authUrl = "https://api.vasttrafik.se:443/token"

logger = logging.getLogger(__name__)
logLevel = logging.ERROR
logger.setLevel(logLevel)

def getToken():
    authReq = request.Request(authUrl, data=authData, headers={"Authorization": "Basic {}".format(authToken)})

    resp = json.loads(request.urlopen(authReq).read().decode('utf8'))
    return (resp['access_token'])


def getInfo(stopId, track='A', token=None):
    now = datetime.now()
    date, time = tuple(now.strftime("%Y-%m-%d %H:%M").split(" "))
    params = {"stopId": stopId, "date": date, "time": time, "timeSpan": 59}
    urlBase = "https://api.vasttrafik.se/bin/rest.exe/v2/departureBoard?id={stopId}&date={date}&time={time}&timeSpan={timeSpan}&maxDeparturesPerLine=2&format=json"
    url = urlBase.format(**params)
    token = getToken() if not token else token
    req = request.Request(url, headers={"Authorization": "Bearer {token}".format(token=token)})
    resp = json.loads(request.urlopen(req).read().decode('utf8'))
    try:
        ds = resp['DepartureBoard']['Departure']
        deps = [ds] if type(ds) is dict else ds
        toRes = lambda dep: {'name': dep['sname'], 'track': dep['track'], 'time': dep['rtTime'], 'date': dep['rtDate'], 'direction':dep['direction']}
        isTrack = lambda d: d['track'] == track
        def howLong(d):
            until = (datetime.strptime(" ".join([d['date'], d['time']]), "%Y-%m-%d %H:%M") - now).seconds//60
            d['until'] = until
            return d
        return list(map(howLong, map(toRes, filter(isTrack, deps))))
    except KeyError:
        # If the board is empty,
        # we get a key error.
        return []
    except Exception as err:
        logger.error("{}:{} when handling {}".format(type(err), err, resp))
        return []

def getBusTimes():
   logger.info("Fetching token...")
   token = getToken()
   logger.info("Fetching Lövviksvägen...")
   lvgRes = getInfo(lvvg, token=token)
   logger.info("Fetching Hovås Nedre...")
   hvsRes = getInfo(hvsnedre, token=token)
   getTimes = lambda name, res: list(map(lambda d: d['until'], filter(lambda d: d['name'] == name, res)))
   r82 = getTimes('82', lvgRes)[:2]
   rRosa = getTimes('ROSA', hvsRes)[:2]
   r158 = getTimes('158', hvsRes)[:2]
   return [r82, rRosa, r158]


if __name__ == "__main__":
    logger.info(getBusTimes())
