import lldb


debugger = None
process = None

ByteOrder = 'little'

def read_memory(address,len=8):
    global debugger,process
    error = lldb.SBError()
    return process.ReadUnsignedFromMemory(address,len,error)

def test_debugger(arg):
    print(f"In udb test_debugger arg: {arg}")

def dump_memory(address,bytes=False):
    global debugger
    if (bytes):
        cmd0 = "x -c 64 0x%x" % (address-64)
    else:
        cmd0 = "x/8xg 0x%x" % (address-64)
    print(f"======dump before header: {cmd0}")
    debugger.HandleCommand(cmd0)
    cmd = "x -c 128 0x%x" % address if bytes else "x/16xg 0x%x" % address
    print(f"------Dump from header: {cmd}")
    debugger.HandleCommand(cmd)
    
def evaluate(string):
    global debugger,process
    thread = process.GetSelectedThread()
    frame = thread.GetSelectedFrame()
    return frame.EvaluateExpression(string).unsigned

def convenience_variable(name):
    return evaluate(f"${name}")

def set_convenience_variable(name,val):
    evaluate(f"uintptr_t ${name} = {val}")

def set_debugger(dbg):
    global debugger, process
    debugger = dbg
    process = dbg.GetSelectedTarget().GetProcess()
    
    
