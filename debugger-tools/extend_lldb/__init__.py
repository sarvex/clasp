
import extend_lldb.print_function
import extend_lldb.loadperf
import extend_lldb.globals

def do_lldb_init_module(debugger, internal_dict,prefix):
    prefix = f"{prefix}.extend_lldb"
    extend_lldb.print_function.do_lldb_init_module(debugger,internal_dict,prefix)
    extend_lldb.loadperf.do_lldb_init_module(debugger,internal_dict,prefix)
    extend_lldb.globals.do_lldb_init_module(debugger,internal_dict,prefix)
    
