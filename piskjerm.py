#!/usr/bin/env python3

import inkyphat
import bustimes
import time
from datetime import datetime, timedelta


fontheight = 12
fontwidth = fontheight
linespace = 2

# Set the default rotation
inkyphat.set_rotation(180)
vt = inkyphat.Image.open("vt.png")

font = inkyphat.ImageFont.truetype(inkyphat.fonts.PressStart2P, fontwidth)
bigfont = inkyphat.ImageFont.truetype(inkyphat.fonts.PressStart2P, 14)

def clear(color=inkyphat.WHITE):
  for x in range(0,inkyphat.WIDTH):
    for y in range(0, inkyphat.HEIGHT):
      inkyphat.putpixel((x,y),color)

def printlines(msg, pos = None, color=inkyphat.BLACK):
  offset = 0
  lines = msg.splitlines()
  if pos is not None:
    x,y = pos
  else:
    x,y = (2, inkyphat.HEIGHT//2 - fontheight*len(lines)//2)
  for l in lines:
    inkyphat.text((x,y+offset),l,color, font)
    offset += fontheight + linespace

def printBusTimes(times):
   prefix = ["82:","Rosa:", "158:"]
   m = "min"
   op = []
   toL5 = lambda s: " "*(max(5-len(s),0)) + s
   for i,ts in enumerate(times):
     s = [prefix[i], "-","-"]
     for j,t in enumerate(ts):
       s[j+1] = str(t) + m
     op.append(" ".join(map(toL5, s)))
   bt = "\n".join(op)
   inkyphat.clear()
   inkyphat.text((inkyphat.WIDTH//2 - 14*fontwidth//2,2)
                , "Strætóferðir", inkyphat.RED, bigfont)
   printlines(bt) #,color=inkyphat.RED)
   vtc = ((inkyphat.WIDTH - 10*14 -vt.size[0] - 10)//2  ,inkyphat.HEIGHT-2-14)
   inkyphat.text( (vtc[0]+vt.size[0]+2,vtc[1]-4)
                , "Västtrafik", inkyphat.RED, bigfont)
   ti = inkyphat.ink
   inkyphat.ink = inkyphat.RED
   inkyphat.paste(vt,((vtc[0],vtc[1]-vt.size[1]+14-2)))
   inkyphat.ink = ti
   inkyphat.show()


if __name__ == "__main__":
  frequency = timedelta(0,60)
  while True:
    now = datetime.now()
    print("Waking up at {now}".format(now=now.strftime("%Y-%m-%d %H:%M")))
    print("Fetching bus departures...")
    bt = bustimes.getBusTimes()
    print("Bus times are: {}".format(bt))
    print("Outputting bus departures...")
    printBusTimes(bt)
    now_again = datetime.now()
    seconds_until = max((now+frequency - now_again).seconds, 0)
    print("Sleeping for {}secs...".format(seconds_until))
    time.sleep(seconds_until)
