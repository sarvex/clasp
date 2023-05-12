import lldb


global_DataTypes = {}
global_Kinds = {}

class DataType:
    def __init__(self,data_type,name):
        self._data_type = data_type
        self._name = name

class ClassKind:
    def __init__(self,stamp,name,size):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._fields = {}
        self._variable_array0 = None
        self._variable_capacity = None
        self._variable_fields = {}

class TemplatedKind:
    def __init__(self,stamp,name,size):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._fields = {}

class ContainerKind:
    def __init__(self,stamp,name,size):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._variable_array0 = None
        self._variable_capacity = None
        self._variable_fields = {}

class BitunitContainerKind:
    def __init__(self,stamp,name,size,bits_per_bitunit):
        self._stamp = stamp
        self._name = name
        self._size = size
        self._bits_per_bitunit = bits_per_bitunit
        self._variable_array0 = None
        self._variable_capacity = None
        self._variable_fields = {}
        
class FixedField:
    def __init__(self,index,data_type,field_name,field_offset):
        self._index = index
        self._data_type = data_type
        self._field_name = field_name
        self._field_offset = field_offset

class VariableArray0:
    def __init__(self,name,offset):
        self._name = name
        self._offset = offset

class VariableCapacity:
    def __init__(self,element_size,end_offset,capacity_offset):
        self._element_size = element_size
        self._end_offset = end_offset
        self._capacity_offset = capacity_offset

class VariableField:
    def __init__(self,index,data_type,field_name,field_offset):
        self._index = index
        self._data_type = data_type
        self._field_name = field_name
        self._field_offset = field_offset

        
def Init_data_type(data_type,name):
    global_DataTypes[data_type] = DataType(data_type,name)

def Init_class_kind(stamp, name, size):
    # print("Init__class_kind stamp = %d\n" % stamp)
    global_Kinds[stamp] = ClassKind(stamp,name,size)

def Init_templated_kind(stamp, name, size):
    # print("Init__templated_kind stamp = %d\n" % stamp)
    global_Kinds[stamp] = TemplatedKind(stamp,name,size)

def Init_container_kind(stamp, name, size):
    # print("Init__container_kind stamp = %d\n" % stamp)
    global_Kinds[stamp] = ContainerKind(stamp,name,size)


def Init_bitunit_container_kind(stamp, name, size, bits_per_bitunit):
    # print("Init__bitunit_container_kind stamp = %d\n" % stamp)
    global_Kinds[stamp] = BitunitContainerKind(stamp,name,size,bits_per_bitunit)
    
def Init__fixed_field(stamp,index,data_type,field_name,field_offset):
    # print("Init__fixed_field stamp = %d\n" % stamp)
    classKind = global_Kinds[stamp]
    field = FixedField(index,data_type,field_name,field_offset)
    classKind._fields[index] = field

def Init__variable_array0(stamp,name,offset):
    # print("Init__variable_array0 stamp = %d\n" % stamp)
    classKind = global_Kinds[stamp]
    classKind._variable_array0 = VariableArray0(name,offset)

def Init__variable_capacity(stamp,element_size,end_offset,capacity_offset):
    # print("Init__variable_capacity stamp = %d\n" % stamp)
    classKind = global_Kinds[stamp]
    classKind._variable_capacity = VariableCapacity(element_size,end_offset,capacity_offset)

def Init__variable_field(stamp,index,data_type,field_name,field_offset):
    # print("Init__variable_field stamp=%d\n" % stamp)
    classKind = global_Kinds[stamp]
    field = VariableField(index,data_type,field_name,field_offset)
    classKind._variable_fields[index] = field



execfile("layout.py")


def is_int(str,base):
    try:
        int(str,base)
        return True
    except ValueError:
        return False

def generalp(tptr):
    return (tptr&3==1)

def untag_general(tptr):
    return tptr-1

def consp(tptr):
    return (tptr&3==3)

def untag_cons(tptr):
    return tptr-3

def read_unsigned_at_offset(debugger,verbose,base,offset):
    process = debugger.GetSelectedTarget().GetProcess()
    err = lldb.SBError()
    tptr = process.ReadUnsignedFromMemory(base+offset,8,err)
    if (verbose): print("read_unsigned_at_offset offset: %x" % (base+offset))
    return tptr
    
def print_object_type(debugger,verbose,obj,type_=0):
    if (type_==0):
        print_tagged_ptr(debugger,verbose,obj)
    print("print_object_type Handle obj: %d  type: %d\n" % (obj, type_))

def print_variable_array0(debugger,verbose,indent,class_,obj):
    base = untag_general(obj)
    data_offset = class_._variable_array0._offset
    length_offset = class_._variable_capacity._end_offset
    length_ = read_unsigned_at_offset(debugger,verbose,base,length_offset)
    print("%d slots" % length_)
    for index in range(0,length_):
        data = read_unsigned_at_offset(debugger,verbose,base,data_offset+8*index)
        print("Data[%d] at 0x%x"% (index,data))
    
def print_simple_base_string(debugger,verbose,indent,class_,obj):
    base = untag_general(obj)
    data_offset = class_._variable_array0._offset
    length_offset = class_._variable_capacity._end_offset
    length_ = read_unsigned_at_offset(debugger,verbose,base,length_offset)
    err = lldb.SBError()
    process = debugger.GetSelectedTarget().GetProcess()
    data = process.ReadMemory(base+data_offset,length_,err)
    if (err.Success()):
        print(f"Data: {data} ")
    else:
        print("Data Could not read!!!")

def print_Closure_O(debugger,verbose,indent,class_,obj):
    print(f"class dict -> {class_.__dict__}")
    
def print_shallow_object_type(debugger,verbose,indent,obj,type_=0):
    if (type_ == 0) and (generalp(obj)):
        base = untag_general(obj)
        header_ptr = base - 8
        err = lldb.SBError()
        process = debugger.GetSelectedTarget().GetProcess()
        header = process.ReadUnsignedFromMemory(header_ptr,8,err)
        if (err.Success()):
            stamp = header>>4
            if (verbose): print("%sheader@%x stamp = %d" % (indent,header_ptr,stamp))
            class_ = global_Kinds[stamp]
            name = class_._name
            if (name=="core::SimpleBaseString_O"):
                if verbose:
                    print(f"class_ = {class_.__dict__}")
                print_simple_base_string(debugger,verbose,indent,class_,obj)
                return
            if verbose:
                print(f"{indent}class = {name}")
        return
    print("%sprint_object_type Handle obj: %d  type: %d\n" % (indent, obj, type_))
    
def print_tagged_ptr(debugger,verbose,tptr):
    if (generalp(tptr)):
        base = tptr-1
        header_ptr = base - 8
        err = lldb.SBError()
        process = debugger.GetSelectedTarget().GetProcess()
        header = process.ReadUnsignedFromMemory(header_ptr,8,err)
        if (err.Success()):
            stamp = header>>4
            if (verbose): print("header@%x stamp = %d" % (header_ptr,stamp))
            class_ = global_Kinds[stamp]
            name = class_._name
            print(f"a {name}")
            for field in class_._fields.values():
                val = read_unsigned_at_offset(debugger,verbose,base,field._field_offset)
                print("field@0x%x: %s" % (val,field._field_name))
                type_ = field._data_type
                print_shallow_object_type(debugger,verbose,"  ",val,type_)
            if (class_._variable_array0):
                print_variable_array0(debugger,verbose,"  ",class_,tptr)
            return
        print("Error %s\n" % err)
        return
    print("print_tagged_ptr handle: %s\n" % tptr)

def inspect(debugger,command,result,internal_dict):
    args = command.split(" ")
    arg = args[0]
    verbose = len(args) > 1
    ptr = None
    if arg[:2] == '$r':
        print("Handle register %s\n" % arg)
        return
    if (is_int(arg,16)):
        tptr = int(arg,16)
    elif (is_int(arg,10)):
        tptr = int(arg,10)
    else:
        key = lldb.frame.FindVariable(arg)
        if verbose:
            print(f"arg = {key}")
        theObject = key.GetChildMemberWithName("theObject")
        # theObject.GetValue() returns a string - why? dunno
        if verbose:
            print(f"theObject.GetValue() = {theObject.GetValue()}")
        tptr = int(theObject.GetValue(),16)
    print_tagged_ptr(debugger,verbose,tptr)
