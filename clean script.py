import csv
import os

def clean_csv(input_file, output_columns):
    # Create the output file name
    output_file = input_file.split(".")[0] + "_clean.csv"

    # Open the input file
    with open(input_file, "r", newline='') as f:  # Add the newline parameter
        reader = csv.DictReader(f)

        # Filter the input columns to only include the desired output columns
        output_rows = [{col: row[col] for col in output_columns} for row in reader]

    # Write the output file
    with open(output_file, "w", newline='') as f:  # Add the newline parameter
        writer = csv.DictWriter(f, fieldnames=output_columns)
        writer.writeheader()
        writer.writerows(output_rows)

    return output_file


def read_txt_file(input_file):
    with open(input_file, "r") as f:
        lines = f.readlines()
    return [line.strip() for line in lines]

cols = read_txt_file("C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\experiments\\PDM\\vars.txt")

#print(cols)
folder_path = 'C:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\experiments\\results\\results\\results_pdm'
os.chdir(folder_path)

for file_name in os.listdir(folder_path):
    #print(file_name, '------------------', folder_path)
    if file_name.endswith('.csv'):
        clean_csv(file_name, cols)


