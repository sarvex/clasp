(in-package #:koga)

(defparameter +os-features+ '(:bsd :darwin :freebsd :linux :unix))

(defun create-keyword-list (x)
  (if x
      (format nil "Cons_O::create(_lisp->internKeyword(\"~A\"), ~A)"
              (car x) (create-keyword-list (cdr x)))
      "nil<T_O>()"))

(defmethod print-variant-target-source
    (configuration (name (eql :config-h)) output-stream (target (eql :scraper)) source)
  (declare (ignore configuration))
  (write-defines output-stream
                 (string-upcase (substitute #\_ #\- (substitute #\_ #\. (file-namestring (source-path source)))))
                 (intern (format nil "<~a>" (source-path source)) 'keyword)))

(defmethod print-prologue (configuration (name (eql :config-h)) output-stream)
  (write-line "// Do not edit. Generated by the koga script" output-stream)
  (write-ifndef output-stream "CLASP_CONFIG_H")
  (write-defines output-stream
                 "CLASP_CONFIG_H" t
                 "CST" (cst configuration)
                 "USE_PARALLEL_BUILD" (parallel-build configuration)
                 "CLASP_BUILD_MODE" (position (build-mode configuration)
                                              '(:fasl :object :bitcode :faso :fasoll :fasobc))
                 "USE_COMPILE_FILE_PARALLEL" (if (compile-file-parallel configuration) 1 0)
                 "FORCE_STARTUP_EXTERNAL_LINKAGE" (if (force-startup-external-linkage configuration) 1 0)
                 "USE_PRECISE_GC" *variant-precise*
                 "USE_BOEHM" (eq :boehm *variant-gc*)
                 "USE_MMTK" (eq :mmtk *variant-gc*)
                 "USE_MPS" (eq :mps *variant-gc*)
                 "RUNNING_PRECISEPREP" *variant-prep*
                 "PROGRAM_CLASP" t
                 "CLASP_THREADS" t
                 "CLBIND_DYNAMIC_LINK" t
                 "DEFINE_CL_SYMBOLS" t
                 "USE_SOURCE_DATABASE" t
                 "USE_COMPILED_CLOSURE" t
                 "CLASP_UNICODE" t
                 "INCLUDED_FROM_CLASP" t
                 "INHERITED_FROM_SRC" t
                 "NDEBUG" t
                 "X86_64" t
                 "BUILD_EXTENSION" (and (extensions configuration) t)
                 "DEFAULT_STAGE" (if (extensions configuration) :|'e'| :|'c'|)
                 "EXTENSION_SYSTEMS" (intern (create-keyword-list (extension-systems configuration)) "KEYWORD")
                 "_ADDRESS_MODEL_64" t
                 "__STDC_CONSTANT_MACROS" t
                 "__STDC_FORMAT_MACROS" t
                 "__STDC_LIMIT_MACROS" t
                 "ENABLE_BACKTRACE_ARGS" t
                 "DEBUG_MONITOR_SUPPORT" t
                 "DEBUG_DTREE_INTERPRETER" (debug-dtree-interpreter configuration)
                 "DEBUG_DTRACE_LOCK_PROBE" (debug-dtrace-lock-probe configuration)
                 "DEBUG_STACKMAPS" (debug-stackmaps configuration)
                 "DEBUG_ASSERT_TYPE_CAST" (debug-assert-type-cast configuration)
                 "SOURCE_DEBUG" (source-debug configuration)
                 "DEBUG_JIT_LOG_SYMBOLS" (debug-jit-log-symbols configuration)
                 "DEBUG_GUARD" (debug-guard configuration)
                 "DEBUG_GUARD_VALIDATE" (debug-guard-validate configuration)
                 "DEBUG_GUARD_BACKTRACE" (debug-guard-backtrace configuration)
                 "DEBUG_GUARD_EXHAUSTIVE_VALIDATE" (debug-guard-exhaustive-validate configuration)
                 "DEBUG_TRACE_INTERPRETED_CLOSURES" (debug-trace-interpreted-closures configuration)
                 "DEBUG_ENVIRONMENTS" (debug-environments configuration)
                 "DEBUG_RELEASE" (debug-release configuration)
                 "DEBUG_CACHE" (debug-cache configuration)
                 "DEBUG_BITUNIT_CONTAINER" (debug-bitunit-container configuration)
                 "DEBUG_DYNAMIC_BINDING_STACK" (debug-dynamic-binding-stack configuration)
                 "DEBUG_VALUES" (debug-values configuration)
                 "DEBUG_IHS" (debug-ihs configuration)
                 "DEBUG_TRACK_UNWINDS" (debug-track-unwinds configuration)
                 "DEBUG_NO_UNWIND" (debug-no-unwind configuration)
                 "DEBUG_STARTUP" (debug-startup configuration)
                 "DEBUG_REHASH_COUNT" (debug-rehash-count configuration)
                 "DEBUG_MONITOR" (debug-monitor configuration)
                 "DEBUG_MONITOR_SUPPORT" (debug-monitor-support configuration)
                 "DEBUG_MEMORY_PROFILE" (debug-memory-profile configuration)
                 "DEBUG_BCLASP_LISP" (debug-bclasp-lisp configuration)
                 "DEBUG_CCLASP_LISP" (debug-cclasp-lisp configuration)
                 "DEBUG_COUNT_ALLOCATIONS" (debug-count-allocations configuration)
                 "DEBUG_COMPILER" (debug-compiler configuration)
                 "DEBUG_VERIFY_MODULES" (debug-verify-modules configuration)
                 "DEBUG_LONG_CALL_HISTORY" (debug-long-call-history configuration)
                 "DEBUG_GFDISPATCH" (debug-gfdispatch configuration)
                 "DEBUG_FASTGF" (debug-fastgf configuration)
                 "DEBUG_SLOT_ACCESSORS" (debug-slot-accessors configuration)
                 "DEBUG_THREADS" (debug-threads configuration)
                 "DEBUG_STORES" (debug-stores configuration)
                 "DEBUG_ENSURE_VALID_OBJECT" (debug-ensure-valid-object configuration)
                 "DEBUG_QUICK_VALIDATE" (debug-quick-validate configuration)
                 "DEBUG_MPS_SIZE" (debug-mps-size configuration)
                 "DEBUG_MPS_UNDERSCANNING" (debug-mps-underscanning configuration)
                 "DEBUG_DONT_OPTIMIZE_BCLASP" (debug-dont-optimize-bclasp configuration)
                 "DEBUG_RECURSIVE_ALLOCATIONS" (debug-recursive-allocations configuration)
                 "DEBUG_ALLOC_ALIGNMENT" (debug-alloc-alignment configuration)
                 "DEBUG_LLVM_OPTIMIZATION_LEVEL_0" (debug-llvm-optimization-level-0 configuration)
                 "DEBUG_SLOW" (debug-slow configuration)
                 "USE_HUMAN_READABLE_BITCODE" (human-readable-bitcode configuration)
                 "DEBUG_COMPILE_FILE_OUTPUT_INFO" (debug-compile-file-output-info configuration)
                 "SNAPSHOT_START" :|_binary_snapshot_start|
                 "SNAPSHOT_END" :|_binary_snapshot_end|
                 "SNAPSHOT_SIZE" :|_binary_snapshot_size|
                 "SNAPSHOT_SEGMENT" "__CLASP"
                 "SNAPSHOT_SECTION" "__clasp"
                 "BUILD_LIB" (string-trim " "
                                          (concatenate 'string
                                                       *variant-ldlibs*
                                                       " "
                                                       (ldlibs configuration)))
                 "BUILD_STLIB" ""
                 "BUILD_LINKFLAGS" (string-trim " "
                                                (concatenate 'string
                                                             *variant-ldflags*
                                                             " "
                                                             (ldflags configuration)))
                 "BUILD_CPPFLAGS" (string-trim " "
                                               (concatenate 'string
                                                            *variant-cxxflags*
                                                            " "
                                                            (cxxflags configuration)))
                 "EXECUTABLE_NAME" (build-name :iclasp)
                 "CLASP_DEV_TEST_PATH" "generated"
                 "CLASP_DEV_SYS_PATH" "../.."
                 "CLASP_DEV_LIB_PATH" (namestring (merge-pathnames (root :variant-lib)
                                                                   (build-path configuration)))
                 "CLASP_DEV_GENERATED_PATH" (namestring (merge-pathnames (root :variant-generated)
                                                                         (build-path configuration)))
                 "CLASP_DEV_INCLUDE_PATH" "include"
                 "CLASP_INSTALL_SYS_PATH" (namestring (root :install-share))
                 "CLASP_INSTALL_LIB_PATH" (namestring (root :install-lib))
                 "CLASP_INSTALL_GENERATED_PATH" (namestring (root :install-generated))
                 "CLASP_INSTALL_INCLUDE_PATH" (namestring (merge-pathnames (make-pathname :directory '(:relative "include"))
                                                                           (share-path configuration)))
                 "APP_NAME" "CLASP"
                 "BITCODE_NAME" *variant-bitcode-name*
                 "VARIANT_NAME" *variant-name*
                 "VARIANT_DIR" *variant-bitcode-name*
                 "CLASP_CLANG_PATH" (namestring (cc configuration))
                 "CXX_BINARY" (namestring (cxx configuration))
                 "NM_BINARY" (namestring (nm configuration))
                 "OBJCOPY_BINARY" (and (objcopy configuration)
                                       (namestring (objcopy configuration))))
  (loop for os in +os-features+
        when (member os *features*)
          do (write-defines output-stream
                            (format nil "_TARGET_OS_~A" os)
                            t))
  (destructuring-bind (major minor &rest junk)
      (uiop:parse-version (llvm-version configuration))
    (declare (ignore junk))
    (write-defines output-stream
                   "LLVM_VERSION_X100" (+ (* 100 major) minor)
                   "LLVM_VERSION" (+ major (* 0.01 minor))
                   "LLVM_VERSION_INT" major))
  (if *variant-debug*
      (write-defines output-stream
                     "_DEBUG_BUILD" t
                     "DEBUG_ASSERT" (debug-assert configuration)
                     "DEBUG_BOUNDS_ASSERT" (debug-bounds-assert configuration)
                     "CONFIG_VAR_COOL" (config-var-cool configuration))
      (write-defines output-stream
                     "_RELEASE_BUILD" t
                     "ALWAYS_INLINE_MPS_ALLOCATIONS" (always-inline-mps-allocations configuration))))

(defmethod print-epilogue (configuration (name (eql :config-h)) output-stream)
  (write-endif output-stream))

(defmethod print-prologue (configuration (name (eql :version-h)) output-stream)
  (write-line "// Do not edit. Generated by the koga script" output-stream)
  (write-ifndef output-stream "CLASP_VERSION_H")
  (write-defines output-stream
                 "CLASP_VERSION_H" t
                 "CLASP_GIT_COMMIT" (commit-short configuration)
                 "CLASP_GIT_FULL_COMMIT" (commit-full configuration)
                 "CLASP_VERSION" (version configuration))
  (write-endif output-stream))
