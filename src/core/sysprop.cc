/*
    File: sysprop.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#include <clasp/core/foundation.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/symbol.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA(key area value);
CL_DECLARE();
CL_DOCSTRING(R"dx(put_sysprop - returns value)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__put_sysprop(T_sp key, T_sp area, T_sp value) {
  ASSERT(_lisp->_Roots._Sysprop.notnilp());
  HashTableEql_sp sysprops = gc::As_unsafe<HashTableEql_sp>(_lisp->_Roots._Sysprop);
  KeyValuePair *area_pair = sysprops->find(area);
  if (area_pair)
    return gc::As<HashTableEql_sp>(area_pair->_Value)->hash_table_setf_gethash(key, value);

  HashTableEql_sp new_hash_table = gc::As<HashTableEql_sp>(
      HashTable_O::create_thread_safe(cl::_sym_eql, SimpleBaseString_O::make("SYSPRRD"), SimpleBaseString_O::make("SYSPRWR")));
  sysprops->hash_table_setf_gethash(area, new_hash_table);
  return new_hash_table->hash_table_setf_gethash(key, value);
}

CL_LAMBDA(key area);
CL_DECLARE();
CL_DOCSTRING(R"dx(get_sysprop - returns (values val foundp))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__get_sysprop(T_sp key, T_sp area) {
  ASSERT(_lisp->_Roots._Sysprop.notnilp());
  HashTableEql_sp sysprops = gc::As_unsafe<HashTableEql_sp>(_lisp->_Roots._Sysprop);
  KeyValuePair *area_pair = sysprops->find(area);
  if (area_pair)
    return gc::As<HashTableEql_sp>(area_pair->_Value)->gethash(key);

  return Values(nil<T_O>(), nil<T_O>());
}

CL_LAMBDA(key area);
CL_DECLARE();
CL_DOCSTRING(R"dx(rem_sysprop)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__rem_sysprop(T_sp key, T_sp area) {
  ASSERT(_lisp->_Roots._Sysprop.notnilp());
  HashTableEql_sp sysprops = gc::As_unsafe<HashTableEql_sp>(_lisp->_Roots._Sysprop);
  KeyValuePair *area_pair = sysprops->find(area);
  return area_pair && gc::As<HashTableEql_sp>(area_pair->_Value)->remhash(key);
}

SYMBOL_SC_(CorePkg, put_sysprop);
SYMBOL_SC_(CorePkg, get_sysprop);
SYMBOL_SC_(CorePkg, rem_sysprop);

}; // namespace core
