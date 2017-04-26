import re
import pprint
import csv

# Note: requires pums_csv folder with Census data dictionary txt file.

# Getting a list
"""Create one file. title, id, variable_name"""
def output_to_csv(all_records):
    filename = "code_dictionary.csv"
    fieldnames = ['title', 'title_note', 'id', 'variable_name']

    with open(filename, 'w', newline='\n') as csvfile:
        myCsvWriter = csv.DictWriter(csvfile, delimiter=',', quotechar='"', fieldnames = fieldnames,lineterminator='\n')

        myCsvWriter.writeheader()

        for record in all_records:
            for row in record['table']:
                row['title'] = record['title']
                row['title_note'] = record['title_note']
                myCsvWriter.writerow(row)

# {'table': [{'id': 'b',
#             'variable_name': 'N/A (age less than 15 years; never married)'},
#            {'id': '1', 'variable_name': 'Yes'},
#            {'id': '2', 'variable_name': 'No'}],
#  'title': 'MARHD',
#  'title_code': '1',
#  'title_note': 'Divorced in the past 12 months'}

#def output_to_csv_file_per(record_dict): #Creates csv file per variable_name
#    filename = "pums_csv/{}.csv".format(record_dict['title'])
#    fieldnames = ['id', 'variable_name']
#    comment = record_dict['title_note']
#    with open(filename, 'w') as csvfile:
#        csvfile.write("# " + comment + "\n")
#        myCsvWriter = csv.DictWriter(csvfile, delimiter=',', quotechar='"', fieldnames = fieldnames)
#
#        myCsvWriter.writeheader()
#
#        for row in record_dict['table']:
#        	myCsvWriter.writerow(row)

# DIVISION        1
#         Division code
#                 0 .Puerto Rico
#                 1 .New England (Northeast region)
#                 2 .Middle Atlantic (Northeast region)
#                 3 .East North Central (Midwest region)
#                 4 .West North Central (Midwest region)
#                 5 .South Atlantic (South region)
#                 6 .East South Central (South region)
#                 7 .West South Central (South Region)
#                 8 .Mountain (West region)
#                 9 .Pacific (West region)

def parse_record(record_string):
#    print("Record to parse:")
#    print("----------------\n")
#    print(record_string)
#    print("\n\n")
    record_dict = {} # title, title_note, title_code, table

    lines = record_string.split("\n")
    #print(lines)
    # get title and title_code
    matches = re.search('^([A-Z0-9]+)[\s\t]+(\d+)\t*$', lines[0])
    record_dict["title"] = matches.group(1)
    record_dict["title_code"] = matches.group(2)

    matches = re.search('^[\t\s]*(.*)$', lines[1])
    record_dict["title_note"] = matches.group(1)

    rows = [] # keys: id, variable_name
    for line in lines[2:]:
        if (line == ""):
            continue
        #print("--------")
        row_dict = {}
        #pprint.pprint(line)
        matches = re.search('^\t+([^\t\s]+)[\t\s]+\.(.*)$', line)
        if (not matches):
            continue
        row_dict["id"] = matches.group(1)
        row_dict["variable_name"] = matches.group(2)
        # pprint.pprint(row_dict)
        rows.append(row_dict)
#    pprint.pprint(rows)

    record_dict["table"] = rows

#    pprint.pprint(record_dict)

    return(record_dict)

filename = 'PUMS_Data_Dictionary_2011-2015.txt'
with open(filename, encoding="ISO-8859-2") as infile:
    all_records = []
    in_record = False
    record = ""
    for line in infile:
        if re.search("^[A-Z][A-Z0-9]+", line):
            if (line[:4] == "NOTE"):
                continue # skip to next line, false positive.
            in_record = True
            record = "" # reset
        elif re.search("^$", line):
            if (in_record):
                all_records.append(parse_record(record))
                in_record = False


        if (in_record):
            record += line

output_to_csv(all_records)
