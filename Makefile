all: publish

publish:
	@echo "Publishing..."
	emacs --quick --batch --load lisp/publish.el --funcall org-publish-all t t

# Recipe to clean the artifacts produced by the `publish` recipe.
clean:
	@echo "Cleaning up..."
	@rm -rvf public/
	@rm -rvf .cache/
