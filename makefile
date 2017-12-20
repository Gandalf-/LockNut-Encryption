
all:
	@echo "nothing to compile"

tests:
	@racket test/test-common.rkt
	@racket test/test-encryption.rkt
	@racket test/test-waterfall.rkt
