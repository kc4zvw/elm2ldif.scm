#! /usr/local/bin/guile3 -s
 !#

; ###----------------------------------------------------------------###
; #
; #    Author : David Billsbrough 
; #   Created : Monday, August 26, 2024 at 04:21:56 AM (EDT)
; #   License : GNU General Public License - version 2
; #   Version : $Revision: 0.24 $
; #  Warranty : None
; #
; #   Purpose : Convert an Elm address book for use with
; #           :  Earthlink contact book lists.
; #
; #  $Id: elm2ldif.scm,v 0.24 2024/08/27 00:45:21 kc4zvw Exp kc4zvw $
; #
; ###----------------------------------------------------------------###

(use-modules (ice-9 format))
(use-modules (ice-9 iconv))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))
(use-modules (ice-9 string-fun))

(define os_sep "/")

(define Now (current-time))

(define Today (strftime "%A, %B %d, %y at %H:%M:%S (%Z)" (localtime Now)))

(define first_name "")
(define last_name "")
(define temp "")

(define (get_home_dir)
	(define myHOME (getenv "HOME"))
	(display (format #f "My $HOME directory is ~a.~%~%" myHOME)))

; return local date in format of "2020-01-28T03:19:32"
(define get_timestamp
	(strftime "%Y-%m-%dT%H:%M:%S" (localtime Now)))

;(define delete_comment
;	(display "Deleting comment\n"))

;(define get_firstname
;	(display "comment: parsing first name\n"))

;(define get_lastname
;	(display "comment: parsing last name\n"))

;(define (get_fullname first0 last0)
;	(define first_name first0)
;	(define last_name last0)
;	(string-append first_name "_" last_name))

(define (display_entry alias0 name0 email0)
	(define myalias alias0)
	(define myname name0)
	(define myemail email0)
	(display (format #f "Converting ~a (~a) <~a>.~%" myname myalias myemail)))

; # """ Print title line """ 
(define (write_address_header)
	(display (format #f "# LDIF file for import into Thunderbird email client.~%~%") outfile))

(define (swap_first_last_names temp)
	;(define first_name "")
	(begin
		(define pos1 (string-index temp #\, 0))		;# search for a comma
		(define pos2 (string-index temp #\; 0))		;# search for a semicolon
		;(display (format #f "comma at '~a'; semicolon at '~a' ~%~%" pos1 pos2))
		(if (> pos2 0)
			(begin
				(set! first_name (substring temp (+ pos2 2)))
				(set! last_name (substring temp 0 pos2 ))))))

 
(define (write_address_record alias first last email)

	;(display (format #f "$ ~a:~a:~a:~a~%~%" alias  first_name last_name email))

	(define fullname (format #f "~a ~a" first last))
	(define timestamp (format #f "~a" get_timestamp))
	;(define timestamp "2024-08-26T00:00:00")

	(display (format #f "dn: cn=~a,mail=~a~%" fullname email) outfile)
	(display (format #f "objectclass: top~%") outfile)
	(display (format #f "objectclass: person~%") outfile)
	(display (format #f "objectclass: organizationalPerson~%") outfile)
	(display (format #f "objectclass: inetOrgPerson~%") outfile)
	(display (format #f "objectclass: mozillaAbPersonAlpha~%") outfile)
	(display (format #f "givenName: ~a~%" first) outfile)
	(display (format #f "sn: ~a~%" last) outfile)
	(display (format #f "cn: ~a~%" fullname) outfile)
	(display (format #f "mozillaNickname: ~a~%" alias) outfile)
	(display (format #f "mail: ~a~%" email) outfile)
	(display (format #f "modifytimestamp: ~a~%" timestamp) outfile)
	(display (format #f "~%") outfile))

(define (process_line line2)

	;(display (format #f "~a~%" line2))
	(define m1 (string-index line2 #\= 0))
	(define m2 (string-index-right line2 #\= 0))
	;(display (format #f "match1 = ~a; match2 = ~a.~%" m1 m2))

	(define alias (substring line2 0 (- m1 1)))
	(define name0 (substring line2 (+ m1 2)(- m2 1)))
	(define email (substring line2 (+ m2 2)))
	;(display (format #f "alias = ~a~%" alias))
	;(display (format #f "name0 = ~a~%" name0))
	;(display (format #f "email = ~a~%" email))

	; ### string-index s char_pred [start [end]]
	(define temp name0)

	(swap_first_last_names temp)

	;(if (> pos1 0)
	;	(display "Matched a comment: deleting a comment\n") 
	;	(define name2 = (substring name0 0 (- pos1 1))))

	(display_entry alias name0 email)							;# display progress
	(write_address_record alias first_name last_name email))	;# write single entry
	

; ###------------------------------------------------------###
; ###---                  Main Routine                  ---###
; ###------------------------------------------------------###

(newline)
(display (format #f "Converting an 'elm' address book to LDIF file.~%"))
(newline)
(display (format #f "Today's date is ~a.~%" Today))
(newline)

(define myhome (get_home_dir))

(define elm_path "/home/kc4zvw/.elm/aliases.text")
;(define (elm_path
;	(string-append (or (getenv "HOME") "/home/kc4zvw") "/" ".elm" "/" "aliases.text")))

; ## abook = os.sep.join((home, "new_addresses.ldif"))

(define abook_path "/home/kc4zvw/new_addresses.ldif")
;(define (abook_path
;	(string-append (or (getenv "HOME") "/home/kc4zvw") "/" "new_addresses.ldif")))

(display (format #f "The Elm mail alias file is ~a.~%" elm_path))
(display (format #f "The export address book file is ~a.~%" abook_path))
(newline)

;## input = open(elm, 'r')
(begin
	;(define infile (open-input-file some-file))
	(define infile (open-input-file elm_path))
;rescue
;	print "Couldn't open #{calendarFile} for reading dates.\n"
;	exit 2 
;end
)

;## output = open(abook, 'w')
(begin
	;(define infile (open-input-file some-file))
	(define outfile (open-output-file abook_path))
;rescue
;	print "Couldn't open #{calendarFile} for reading dates.\n"
;	exit 2 
;end
)

(write_address_header)

;(write_address_record alias first last email)

(define (first-loop)
	(let loop ((line (read-line)))
	(if (not (eof-object? line))

	(begin
		;(format #t "line: ~a\n" line)
		; process the line
		(process_line line)
		
		(let ((m (string-match "^[ \t]*#" line)))
		(if m (format #t "comment: ~a\n" line)))
		;(format #t "read next line\n")
		(loop (read-line))))))

(define (main)
	(begin
		(with-input-from-file "/home/kc4zvw/.elm/aliases.text" first-loop)))
	
(main)

; close input file
(close-input-port infile)
; close output file
(close-output-port outfile)

(newline)
(display "Conversion completed.\n")

; ** End of Program **
