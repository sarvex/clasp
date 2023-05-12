# gdb will 'recognize' this as python
#  upon 'source pygdb-logg.py'
# however, from gdb functions still have
#  to be called like:
#  (gdb) python print logExecCapture("bt")

import sys
import gdb
import os
import re

def logExecCapture(instr):
  # /dev/shm - save file in RAM
  ltxname="/tmp/c.log"

  gdb.execute(f"set logging file {ltxname}")
  gdb.execute("set logging redirect on")
  gdb.execute("set logging overwrite on")
  gdb.execute("set logging on")
  gdb.execute(instr)
  gdb.execute("set logging off")
  return open(ltxname, 'r').read()

# stepi until breakpoint
def stepiUntilBreakpoint(log_file):
  isInBreakpoint = -1;
#  log_file = "/tmp/gdb.log"
  print(f"Writing to {log_file}")
  # as long as we don't find "Breakpoint" in report:
  gdb.execute("set disassemble-next-line on")
  gdb.execute("display/i $pc")
  with open(log_file, 'w') as logEverything:
    idx = 0
    while isInBreakpoint == -1:
      REP=logExecCapture("stepi")
      isInBreakpoint = REP.find("Breakpoint")
      print( "LOOP:: %s\n%s" % (isInBreakpoint, REP))
      logEverything.write("LOOP:: %d\n" % idx)
      idx = idx + 1
      logEverything.write("%s\n" % REP)
    

def extractInstructions(log_file,inst_file):
  with open(log_file,"r") as si:
    so = open(inst_file,"w")
    print("%s  ->  %s\n" % (log_file,inst_file))
    lines = si.readlines()
    i = 0
    print("Number of lines = %d\n" % len(lines))
    while (i<len(lines)):
      if lines[i].find("x/i $pc")!=-1:
        line = lines[i+1][:-1]
        if (line[-2]==':'):
          line = line + lines[i+2][:-1]
        inst = line[5:]
        match = re.search(r'[^0-9a-f]',inst)
        so.write("%s\n" % inst[match.start():])
        i += 1
      i += 1
  so.close()
    
  
