# TODO: make it so one can pass the argument `3`, for p003.hs (i.e., padding with zeros).
%:
	@ghc p$(MAKECMDGOALS).hs >/dev/null
	@./p$(MAKECMDGOALS)

clean:
	@find . -type f -executable -delete
	@rm -rf *.hi *.o
