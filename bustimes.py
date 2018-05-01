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

authToken = b64encode("{k}:{s}"
                     .format(k=authKey,s=authSecret)
                     .encode('utf8')
                     ).decode('utf8')

authData = b"grant_type=client_credentials"
authUrl = "https://api.vasttrafik.se:443/token"

timeSpan = 59

logger = logging.getLogger(__name__)
logLevel = logging.ERROR
logger.setLevel(logLevel)

def getToken():
    authReq = request.Request(authUrl,
                              data=authData,
                              headers={"Authorization":
                                         "Basic {}".format(authToken)})
    resp = json.loads(request.urlopen(authReq).read().decode('utf8'))
    return (resp['access_token'])


def getInfo(stopId, track='A', token=None):
    now = datetime.now()
    date, time = tuple(now.strftime("%Y-%m-%d %H:%M").split(" "))
    params = {"stopId": stopId,
              "date": date,
              "time": time,
              "timeSpan": timeSpan}
    urlBase = "https://api.vasttrafik.se/bin/rest.exe/v2/"\
            "departureBoard?id={stopId}&date={date}&time={time}"\
            "&timeSpan={timeSpan}&maxDeparturesPerLine=2&format=json"
    url = urlBase.format(**params)
    token = getToken() if not token else token
    req = request.Request(url,
                          headers={"Authorization":
                                     "Bearer {token}".format(token=token)})
    resp = json.loads(request.urlopen(req).read().decode('utf8'))
    try:
        ds = resp['DepartureBoard']['Departure']
        deps = [ds] if type(ds) is dict else ds
        toRes = lambda dep: {'name': dep['sname'],
                             'track': dep['track'],
                             'time': dep['rtTime'], #rtTime is realtime time
                             'date': dep['rtDate'],
                             'direction':dep['direction']}
        isTrack = lambda d: d['track'] == track
        def howLong(d):
            until = (datetime.strptime(" ".join([d['date'], d['time']]),
                                       "%Y-%m-%d %H:%M") - now).seconds//60
            # Sometimes the arrival time will be in the past,
            # since the bus is almost there. Then we just show 0.
            d['until'] = until if until <= timeSpan else 0
            return d
        return list(map(howLong, map(toRes, filter(isTrack, deps))))
    except KeyError:
        # If the board is empty, we get a key error. Then we want to show
        # no times.
        return []
    except Exception as err:
        logger.error("{}:{} when handling {}".format(type(err), err, resp))
        return []


class Either(object):
    left = None
    right = None
    def __init__(self, left=None, right=None):
        self.left = left
        self.right = right
        assert(left or right)

def getBusTimes(token = None):
   logger.info("Fetching token...")
   token = getToken() if not token else token
   logger.info("Fetching Lövviksvägen...")
   lvgRes = getInfo(lvvg, token=token)
   lvgBkRes = getInfo(lvvg, track='B', token=token)
   logger.info("Fetching Hovås Nedre...")
   hvsRes = getInfo(hvsnedre, token=token)
   getTimes = lambda name, res: list(map(lambda d: d['until'],
                                         filter(lambda d: d['name'] == name,
                                                res)))
   r82 = getTimes('82', lvgRes)[:2]
   r82Bk = getTimes('82', lvgBkRes)[:2]
   rRosa = getTimes('ROSA', hvsRes)[:2]
   r158 = getTimes('158', hvsRes)[:2]
   if r158:
     rLast = Either(left=r158)
   else:
     rLast = Either(right=r82Bk)
   return [r82, rRosa, rLast]


if __name__ == "__main__":
    logger.info(getBusTimes())
