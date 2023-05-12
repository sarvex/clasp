import lldb

def dump_globals(debugger,command,result,internal_dict):
    if not (target := debugger.GetSelectedTarget()):
        return
    if module := target.module[target.executable.basename]:
            # Keep track of which variables we have already looked up
        global_names = []
            # Iterate through all symbols in the symbol table and watch for any
            # DATA symbols
        for symbol in module.symbols:
            if symbol.type == lldb.eSymbolTypeData:
                # The symbol is a DATA symbol, lets try and find all global variables
                # that match this name and print them
                global_name = symbol.name
                    # Make sure we don't lookup the same variable twice
                if global_name not in global_names:
                    global_names.append(global_name)
                    if global_variable_list := module.FindGlobalVariables(
                        target, global_name, lldb.UINT32_MAX
                    ):
                            # Print results for anything that matched
                        for global_variable in global_variable_list:
                                # returns the global variable name as a string
                            print(f'name = {global_variable.name}')
                                # Returns the variable value as a string
                            print(f'value = {global_variable.value}')
                            print(f'type = {global_variable.type}')
                                # Returns an lldb.SBAddress (section offset
                                # address) for this global
                            print(f'addr = {global_variable.addr}')
                            # Returns the file virtual address for this
                            # global
                            print('file_addr = 0x%x' % global_variable.addr.file_addr)
                                # returns the global variable value as a string
                            print(f'location = {global_variable.location}')
                                # Returns the size in bytes of this global
                                # variable
                            print(f'size = {global_variable.size}')
                            print()

def find_global(debugger,command,result,internal_dict):
    print(f"command = {command}")
    seek_name = command
    if target := debugger.GetSelectedTarget():
        if module := target.module[target.executable.basename]:
            # Keep track of which variables we have already looked up
            global_names = []
            # Iterate through all symbols in the symbol table and watch for any
            # DATA symbols
            for symbol in module.symbols:
                if symbol.type == lldb.eSymbolTypeData:
                    # The symbol is a DATA symbol, lets try and find all global variables
                    # that match this name and print them
                    global_name = symbol.name
                    # Make sure we don't lookup the same variable twice
                    if global_name not in global_names:
                        global_names.append(global_name)
                        if global_variable_list := module.FindGlobalVariables(
                            target, global_name, lldb.UINT32_MAX
                        ):
                            # Print results for anything that matched
                            for global_variable in global_variable_list:
                                if (global_variable.name == seek_name):
                                    # returns the global variable name as a string
                                    print(f'name = {global_variable.name}')
                                    # Returns the variable value as a string
                                    print(f'value = {global_variable.value}')
                                    print(f'type = {global_variable.type}')
                                    # Returns an lldb.SBAddress (section offset
                                    # address) for this global
                                    print(f'addr = {global_variable.addr}')
                                    # Returns the file virtual address for this
                                    # global
                                    print('file_addr = 0x%x' % global_variable.addr.file_addr)
                                    # returns the global variable value as a string
                                    print(f'location = {global_variable.location}')
                                    # Returns the size in bytes of this global
                                    # variable
                                    print(f'size = {global_variable.size}')
                                    print()
                                    return global_variable
    return None


def do_lldb_init_module(debugger, internal_dict,prefix):
    prefix = f"{prefix}.globals"
    debugger.HandleCommand(
        f'command script add -f {prefix}.count_globals count_globals'
    )
    debugger.HandleCommand(
        f'command script add -f {prefix}.find_global find_global'
    )

