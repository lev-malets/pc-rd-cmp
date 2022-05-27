$(utils_exec_exe): force $(KEYS)/deps/done
	dune build --build-dir _build_release --release $D/exec.exe
