(load "docbook")

(docbook-file "bookinfo2.xml"
 (:bookinfo
  (:title "&clsql; Users' Guide")
  (:authorgroup
   (:author
    (:firstname "Kevin")
    (:othername "M.")
    (:surname "Rosenberg")
    (:affiliation
     (:jobtitle "Maintainer of CLSQL")))
   (:author
    (:firstname "Pierre")
    (:othername "R.")
    (:surname "Mai")
    (:affiliation
     (:jobtitle "Author of Original MaiSQL Code"))))
  (:printhistory
   (:simpara "$Date: 2003-11-11 01:20:00 -0700 (Tue, 11 Nov 2003) $")
   (:simpara "$Id: bookinfo.xml 8125 2003-11-11 08:20:00Z kevin $"))
  (:legalnotice
   (:itemizedlist
    (:listitem
     (:para "&clsql; is Copyright &copy; 2002-2003 by Kevin M. Rosenberg and
	Copyright &copy; 1999-2001 by Pierre R. Mai."))
    (:listitem
     (:param
      (:application "Allegro CL")
      "&reg; is a registered trademark of Franz Inc."))
    (:listitem
     (:param
      (:application "Common SQL")
      ", "
      (:application "LispWorks")
      " and "
      (:application "Xanalys")
      " are trademarks or registered trademarks of Xanalys Inc."))
    (:listitem
     (:para
      (:application "Microsoft Windows")
      "&reg; is a registered trademark of Microsoft Inc."))
    (:listitem
     (:para
      "Other brand or
	    product names are the registered trademarks or trademarks of
	    their respective holders."))))))

