all: build_client build_server

build_client:
	@echo "Building Client Binary"
	sbcl --eval "(ql:quickload :des-chat)" \
		--eval "(in-package :client)" \
		--eval "(sb-ext:save-lisp-and-die #p\"client\" :toplevel #'main :executable t)"
build_server:
	@echo "Building Server Binary"
	sbcl --eval "(ql:quickload :des-chat)" \
		--eval "(in-package :server)" \
		--eval "(sb-ext:save-lisp-and-die #p\"server\" :toplevel #'main :executable t)"
