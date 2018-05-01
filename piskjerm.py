#!/usr/bin/env python3

import inkyphat
import bustimes
import time
from datetime import datetime, timedelta
import logging
from bustimes import Either

from urllib.error import HTTPError

fontheight = 12
fontwidth = fontheight
linespace = 2

# Set the default rotation
inkyphat.set_rotation(180)
vt = inkyphat.Image.open("vt.png")

font = inkyphat.ImageFont.truetype(inkyphat.fonts.PressStart2P, fontwidth)

bigfont = inkyphat.ImageFont.truetype(inkyphat.fonts.PressStart2P, 14)
logger = logging.getLogger(__name__)

logLevel = logging.ERROR
logger.setLevel(logLevel)

def printlines(msg, pos = None, color=inkyphat.BLACK):
  lines = msg.splitlines()
  if pos is not None:
    x,y = pos
  else:
    x,y = (2, inkyphat.HEIGHT//2 - fontheight*len(lines)//2)
  for i, l in enumerate(lines):
    inkyphat.text((x,y+ i*(fontheight + linespace)),l,color, font)

def printBusTimes(times):
   prefix = ["82:","Rosa:", bustimes.Either("158:","82B:")]
   m = "min"
   op = []
   toL5 = lambda s: " "*(max(5-len(s),0)) + s
   for i,ts in enumerate(times):
     if type(ts) is bustimes.Either:
       pf = prefix[i].left if ts.left else prefix[i].right
       next_times = ts.left if ts.left else ts.right
     else:
       pf = prefix[i]
       next_times = ts
     s = [pf, "-","-"]
     for j,t in enumerate(next_times):
       s[j+1] = str(t) + m
     op.append(" ".join(map(toL5, s)))
   bt = "\n".join(op)
   inkyphat.clear()
   td = datetime.now().strftime("%Y-%m-%d %H:%M")
   inkyphat.text(((inkyphat.WIDTH-16*fontwidth)//2, 18), td, inkyphat.RED, font)
   inkyphat.text((inkyphat.WIDTH//2 - 14*fontwidth//2,2)
                , "Strætóferðir", inkyphat.RED, bigfont)
   printlines(bt)
   vtc = ((inkyphat.WIDTH - 10*14 -vt.size[0] - 10)//2, inkyphat.HEIGHT-2-14)
   inkyphat.text( (vtc[0]+vt.size[0]+2,vtc[1]-4)
                , "Västtrafik", inkyphat.RED, bigfont)
   inkyphat.paste(vt,((vtc[0],vtc[1]-vt.size[1]+14-2)))
   inkyphat.show()


if __name__ == "__main__":
  frequency = timedelta(0,60)
  token = None
  while True:
    now = datetime.now()
    logger.info("Waking up at {now}".format(now=now.strftime("%Y-%m-%d %H:%M")))
    logger.info("Fetching bus departures...")
    token = bustimes.getToken() if not token else token
    try:
      bt = bustimes.getBusTimes(token)
    except HTTPError as err:
      # If the code is 401 forbidden, we need to refresh our token
      if err.code == 401:
        token = bustimes.getToken()
        bt = bustimes.getBusTimes(token)
      else:
        bt = [[],[],[]]
        logger.error("{}:{}".format(type(err),err))
    logger.info("Bus times are: {}".format(bt))
    logger.info("Outputting bus departures...")
    printBusTimes(bt)
    now_again = datetime.now()
    seconds_until = max((now+frequency - now_again).seconds, 0)
    logger.info("Sleeping for {}secs...".format(seconds_until))
    time.sleep(seconds_until)
