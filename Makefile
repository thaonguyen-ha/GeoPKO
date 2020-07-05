updatesite: docs/Index.Rmd
    Rscript -e 'workflowr::wflow_publish(c("$<"))'
    
clean:
	rm -rf *.html *.md *.docx figure/ cache/
