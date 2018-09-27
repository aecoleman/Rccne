library(httr)
library(xml2)
library(rvest)
library(data.table)
library(stringr)

# This script is designed to read all of the data from the CCNE webpage listing 
# of accredited Nursing programs.

url.loc <- 'http://directory.ccnecommunity.org/reports/rptAccreditedPrograms_New.asp?sort=institution&sProgramType=1'

# Read HTML Data ----

html.doc <- read_html(url.loc, options = 'HUGE')

# Find Locations of School Data ----

school.table.nodes <- rvest::html_nodes(html.doc, css = 'table[width="100%"]')

xml_find_all(school.table.nodes, './/br') %>% xml_add_sibling('p', '//NL//')

# Get the Node Values ----

school.nm <- 
  html_node(school.table.nodes, 
             css = 'tr > td[style="width: 100%"] > h3') %>% 
  html_text(trim = TRUE)

school.info <- 
  html_node(school.table.nodes,
             css = 'tr > td > table > tr > td[style="width: 20%"]') %>% 
  html_text(trim = TRUE) %>% str_squish()

school.accred <- 
  html_node(school.table.nodes,
             css = 'tr > td > table > tr > td[style="width: 60%;"] > table') %>% 
  html_text(trim = TRUE) %>% str_squish()

school.web <- 
  html_node(school.table.nodes,
             css = 'tr > td > table > tr > td[style="width: 20%"] a') %>% 
  html_attr('href')

# Create the data.table ----

dt.school <- data.table('INSTNM' = school.nm,
                          'INFO' = school.info,
                        'ACCRED' = school.accred,
                        'WEBADDR2' = school.web
                        )[!(is.na(INSTNM) & is.na(INFO) & is.na(WEBADDR2))]

dt.school[, WEBADDR := str_replace(WEBADDR2, '^https?://([^/]+).*$', '\\1')]

# Parsing INFO Column ----

# Create "Passenger" column that will be carried through the process of parsing 
# the data column. This retains the original INFO column intact for audit/debug
dt.school[, PSSNGR := INFO]

# Remove the "Link to Website" text that is present for each row
dt.school[, PSSNGR := str_remove(PSSNGR, 'Link to Website//NL//')]

# Get the School Name, then remove it from passenger
dt.school[, SCH.NM := str_extract(PSSNGR, '^.*?(?=//NL//)')
          ][, PSSNGR := str_remove(PSSNGR, '^.*?//NL//')]

# Get the Street Address, then remove it from passenger
dt.school[, STREET.ADDR := str_extract(PSSNGR, '^.*?(?=//NL//)')
          ][, PSSNGR := str_remove(PSSNGR, '^.*?//NL//')]

# Split Street Address, for when there is a Box or Suite number
# Does this by extracting everything after a comma followed by a space
dt.school[, STREET.ADDR2 := str_extract(STREET.ADDR, '(?<=, ).*$')
          ][, STREET.ADDR := str_remove(STREET.ADDR, ', .*$')]

# Get City name
dt.school[, CITY := str_extract(PSSNGR, '^.*?(?=, )') 
          ][, PSSNGR := str_remove(PSSNGR, '^.*?, ')]

# Get State Abbr
dt.school[, STABBR := str_extract(PSSNGR, '^[A-Z]{2}(?= )')
          ][, PSSNGR := str_remove(PSSNGR, '^[A-Z]{2} ')]

# Get 5 and 9 digit ZIPs
dt.school[, ZIP := str_extract(PSSNGR, '^[0-9]{5}')
          ][, ZIPFOUR := str_extract(PSSNGR, '^[0-9]{5}(-[0-9]{4})?')
            ][, PSSNGR := str_remove(PSSNGR, '^.*?//NL// *(//NL// *)?')]

# Get Chief Nurse Administrator
dt.school[, ADMIN.NAME := str_squish(str_extract(PSSNGR, '(?<=: ).*?(?=//NL//)'))
          ][, PSSNGR := str_remove(PSSNGR, '.*?//NL// *')]

# Get Chief Nurse Administrator's Title
dt.school[, ADMIN.TITLE := str_squish(str_extract(PSSNGR, '(?<=: ).*?(?=//NL//)'))
          ][, PSSNGR := str_remove(PSSNGR, '.*?//NL// *')]

# Get Chief Nurse Administrator's E-Mail
dt.school[, ADMIN.EMAIL := str_squish(str_extract(PSSNGR, '(?<=: ).*?(?=//NL//)'))
          ][, PSSNGR := str_remove(PSSNGR, '.*?//NL// *')]

# Get Chief Nurse Administrator's Phone
dt.school[, ADMIN.PHONE := str_squish( str_extract(PSSNGR, '(?<=: ).*?(?=//NL//)'))
          ][, PSSNGR := str_remove(PSSNGR, '.*?//NL// *')]

# Get Chief Nurse Administrator's Fax
dt.school[, ADMIN.FAX := str_squish( str_extract(PSSNGR, '(?<=: ).*?(?=//NL//)'))
          ][, PSSNGR := str_remove(PSSNGR, '.*?//NL// *')]

# Get the Email Domain Name, as a means to potentially join with other 
# information about the schools that resides in other sources
dt.school[, EMAIL.DOMAIN := str_extract(ADMIN.EMAIL, '(?<=@).*$')]

# Parse ACCRED Data ----

# Initialize passenger for parsing
dt.school[, PSSNGR := ACCRED]

# Get Accreditation Type
dt.school[, ACCRED.TYPE := str_extract(PSSNGR, '^.*?(?= Accreditation)')
          ][, PSSNGR := str_remove(PSSNGR, '^.*? Accreditation *')]

# Get Initial Accreditation Date
dt.school[, INTL.ACCRED.DT := as.Date(
                                str_extract(PSSNGR, 
                                            '(?<=: )[A-Za-z]+ [0-9]+, [0-9]+(?= Most Recent)'),
                                format = '%B %d, %Y')
          ][, PSSNGR := str_remove(PSSNGR, '^.*?: [A-Za-z]+ [0-9]+, [0-9]+ *')]

# Get Recent Accreditation Date
dt.school[, RCNT.ACCRED.DT := as.Date(
  str_extract(PSSNGR, 
              '(?<=: )[A-Za-z]+ [0-9]+, [0-9]+(?= Accreditation)'),
  format = '%B %d, %Y')
  ][, PSSNGR := str_remove(PSSNGR, '^.*?: [A-Za-z]+ [0-9]+, [0-9]+ *')]

# Get Accreditation Expiration Date
dt.school[, EXPR.ACCRED.DT := as.Date(
  str_extract(PSSNGR, 
              '(?<=: )[A-Za-z]+ [0-9]+, [0-9]+(?= Last)'),
  format = '%B %d, %Y')
  ][, PSSNGR := str_remove(PSSNGR, '^.*?: [A-Za-z]+ [0-9]+, [0-9]+ *')]

# Get Last Evaluation Time
dt.school[, LAST.EVAL := 
  str_extract(PSSNGR, 
              '(?<=: )[A-Za-z]+ [0-9]+(?= Next)')
  ][, PSSNGR := str_remove(PSSNGR, '^.*?: [A-Za-z]+ [0-9]+ *')]

# Get Next Evaluation Time
dt.school[, NEXT.EVAL := 
            str_extract(PSSNGR, 
                        '(?<=: )[A-Za-z]+ [0-9]+')
          ][, PSSNGR := str_remove(PSSNGR, '^.*?: [A-Za-z]+ [0-9]+ *')]
