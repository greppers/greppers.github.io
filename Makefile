all: publish

publish:
	@echo "Publishing..."
	emacs --quick --batch --load lisp/publish.el --funcall org-publish-all t t

clean:
	@echo "Cleaning up..."
	@rm -rvf public/
	@rm -rvf .cache/
