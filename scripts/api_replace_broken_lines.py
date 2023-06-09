# open file in read mode
path = "src/core/TTHouse/Api/Foreign/SendGrid/src/model/ContactResponseCustomFields.js"
file = open(path, "r")
replaced_content = ""
i = 0

# looping through the file
for line in file:
    
    # stripping line break
    line = line.strip()

    # replacing the text if the line number is reached
    if "this.;" in line:
        new_line = "return this;"
    elif "this['']" in line:
        new_line = "this[''] = undefined;"     
    else:
        new_line = line

    # concatenate the new string and add an end-line break
    replaced_content = replaced_content + new_line + "\n"

    # Increase loop counter
    i = i + 1

    
# close the file
file.close()

# Open file in write mode
write_file = open(path, "w")

# overwriting the old file contents with the new/replaced content
write_file.write(replaced_content)

# close the file
write_file.close()