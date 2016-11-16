

LIBDIR = site-lisp
PHPDOCDIR = php-chunked-xhtml

all: update

update: update-php-doc
update-php-doc:
	rm -rf $(PHPDOCDIR); wget -N 'http://us2.php.net/get/php_manual_en.tar.gz/from/fr2.php.net/mirror' && tar xzf mirror

clean: clean-php-doc
clean-php-doc:
	rm -rf $(PHPDOCDIR) mirror

flush-cache: flush-elpa-cache
flush-elpa-cache:
	rm -f elpa/*.txt

.PHONY: all clean clean-php-doc flush-cache flush-elpa-cache update update-php-doc 
